{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.C.Compiler
  ( genC, writeCFile
  ) where

import System.IO (IOMode(..), openFile, hClose)
import Data.Typeable (Typeable(..))
import Data.Text.Lazy.IO (hPutStr)
import Data.Text.Lazy (pack)

import Language.C.Quote
import qualified Language.C.Parser as P
import qualified Language.C.Parser.Tokens as T
import qualified Language.C.Syntax as C
import Language.Floorplan.C.Analysis

import qualified Language.C.Pretty as PrettyC
import Text.PrettyPrint.Mainland (prettyLazyText)
import Text.PrettyPrint.Mainland.Class (ppr)

import Language.Floorplan.Core.Syntax as FLPS
import Language.Floorplan.Preproc.Types
import Language.Floorplan.Syntax (BoolExpr(..), printBoolExpr)

import Data.Bifunctor ( bimap )
import Data.Functor ( ($>) )
import Data.Ord (comparing)
import Data.List (sortBy, nub, inits, intersperse)
import Data.Char (toUpper, toLower)
import Data.Maybe (isJust, fromMaybe, catMaybes, fromJust)
import Data.Bits
import qualified Debug.Trace as D

import Language.C.Quote.C
import qualified Language.C.Syntax as S
import Data.Loc -- (SrcLoc(..), noLoc)

import Text.RE.TDFA.String (RE(..), (?=~), matched)

-- TODO: Introduce spans based on position in these Haskell files.
fakeLoc = noLoc --Span (Position 0 0 0) (Position 0 0 0)
fL = fakeLoc

justNameExp :: BaseExp -> Maybe (NameID, BaseExp)
justNameExp (n ::: e) = Just (n, e)
justNameExp _ = Nothing

mkInitConst s = (Nothing, ExpInitializer (Const s fL) fL)

mkStrConst t = S.StringConst ['"' : t ++ "\""] t fakeLoc

mkPoundDefs' :: Int -> [NameID] ->  [MacroDef]
mkPoundDefs' i (n:ns) = (MacroDef n [] $ ExpBody $ CastC "unsigned int" $ IntC i) : (mkPoundDefs' (i+1) ns)
mkPoundDefs' _ [] = []

mkPoundDefs = mkPoundDefs' 0

-- TODO: produce a warning if there's a regex which doesn't match
-- on any of the output types.

doFilterOut :: [String] -> [RE] -> [Maybe String]
doFilterOut types res0 = let
    -- | Does the given type match any of the provided regular expressions?
    --   If so, remove it.
    dFO :: String -> [RE] -> [Maybe String]
    dFO t [] = [Just t]               -- No matches. Keep it.
    dFO t (re:res)
      | matched (t ?=~ re) = [Nothing]  -- It matched. Remove it.
      | otherwise = dFO t res    -- Check the rest of the regular expressions.
  in concatMap (\f -> f res0) (map dFO types)

-- | Wholly filtered out types (ones resulting a FLP_ALL_* decl with an empty RHS of the #define)
--   probably imply that there were unused types / dead code in the FLP spec. Either need to
--   report a warning in such a case, or write a preprocessing analysis pass that checks that this
--   won't happen.
mkAllPrefixes :: [BaseExp] -> [RE] -> [MacroDef]
mkAllPrefixes bes res = let

    ns = findNames bes
    uniques = nub $ concatMap fst ns

    -- Tuples of the constructed type identifier and the list of names from which it was constructed.
    types0 :: [String]
    types0 = map (mkTypeIdentifier . fst) ns

    findFor :: String -> [(([NameID], BaseExp), Maybe String)] -> [String]
    findFor u []      = []
    findFor u (((n_qual, _), Just _):ns0) -- Not filtered out - make and keep the identifier if it involves the type 'u'.
      | u `elem` n_qual = mkTypeIdentifier n_qual : findFor u ns0
      | otherwise       = findFor u ns0
    findFor u (((_, _), Nothing):ns0) = findFor u ns0 -- This particular name is filtered out, as indicated by the Nothing

    mkAll :: (String, [String]) -> MacroDef
    mkAll (unique_ty, paths)
      = MacroDef ("FLP_ALL_" ++ unique_ty) [] (StmtsBody [NameListM $ map ("FLP_" ++) paths])

  in map mkAll (zip uniques (map (\f -> f $ zip ns (doFilterOut types0 res)) (map findFor uniques)))

-- | Get an ordered list of the alternatives which make up this expression
--   and also (safely) pushing constraints down into the returned BaseExps
--   wherever possible to retain that information.
getAltSeqs :: BaseExp -> [ (Maybe NameID, [ (Maybe NameID, BaseExp) ]) ]
getAltSeqs be@(Prim _) = error $ "Unimplemented getAltSeqs: " ++ show be -- [("FLP_Primitive_Bytes", fn be)] -- TODO?
getAltSeqs (Con n be) = getAltSeqs be
getAltSeqs (be :@ align) = getAltSeqs be
-- Constraints function cannot be applied to sequences:
getAltSeqs (e1 :+ e2) = [ (Nothing, getSeqs e1 ++ getSeqs e2) ] -- This is the case I'm modeling against (union of 4 unnamed seqs).
getAltSeqs (e1 :|| e2) = getAltSeqs e1 ++ getAltSeqs e2
getAltSeqs (n ::: be) = [ (Just n, [(Nothing, be)]) ]
-- NOTE: valid existential configurations of a memory are not preserved by this transformation. You cannot
-- assume the results of this function produce coherent BaseExp layouts wrt existentials:
getAltSeqs (Exists n be) = getAltSeqs be
getAltSeqs be@(n :# be') = error $ "Unimplemented getAltSeqs: " ++ show be
getAltSeqs (FLPS.Attr (Contains _) be) = getAltSeqs be
getAltSeqs be@(FLPS.Attr (BaseType (EnumBT _)) be') = error $ "Unimplemented getAltSeqs: " ++ show be
getAltSeqs be@(FLPS.Attr (BaseType (BitsBT _)) be') = error $ "Unimplemented getAltSeqs: " ++ show be
getAltSeqs be@(FLPS.Attr (BaseType (PtrBT _)) be')  = error $ "Unimplemented getAltSeqs: " ++ show be
getAltSeqs be@(FLPS.Attr (BaseType (SizeBT _)) be') = error $ "Unimplemented getAltSeqs: " ++ show be

type MapID = Int
type TypeID = String
type AddressExp = MacroExp
type SizeExp = MacroExp

data MacroExp =
    ExpID String
  | PlusC  MacroExp MacroExp      -- Plus calculation
  | IntC   Int          -- Int calc
  | CastC  String MacroExp   -- Cast calc
  deriving (Eq, Ord, Show)

data MacroStmt =
    IfM       BoolExpr [MacroStmt]
  | ElifM     BoolExpr [MacroStmt]
  | ShadowSet MapID AddressExp TypeID SizeExp
  | CallM     String   [MacroExp] -- Call another function or macro.
  | NameListM [String] -- A comma-separated list of names, without any surrounding brackets
  | BlockStmt [MacroStmt]
  | PlusEq    String MacroExp -- s += exp;
  | InitStmt  String String (Maybe MacroExp) -- e.g., 'unsigned int x = 0;' or just 'unsigned int x;'
  deriving (Eq, Ord, Show)

data MacroBody =
    StmtsBody [MacroStmt]
  | ExpBody   MacroExp
  deriving (Eq, Ord, Show)

data MacroDef = MacroDef String [String] MacroBody
  deriving (Eq, Ord, Show)

printMacros :: [MacroDef] -> String
printMacros ms = concat $ intersperse "\n" $ map printMacro ms

printMacro (MacroDef name [] body)
  = "#define " ++ name ++ " " ++ printBody 2 body
printMacro (MacroDef name args body)
  = "#define " ++ name ++ "(" ++ concat (intersperse "," args) ++ ") " ++ printBody 2 body

printBody :: Int -> MacroBody -> String
printBody c (StmtsBody []) = ""
printBody c (StmtsBody [s@(BlockStmt _)]) = " \\\n" ++ printStmt c s -- Single block stmt indentation better when on a separate line
printBody c (StmtsBody [stmt]) = printStmt c stmt
printBody c (StmtsBody stmts) =
  " \\\n" ++ (concat $ intersperse " \\\n" $ map (printTabbed printStmt c) stmts)
printBody c (ExpBody e) = "(" ++ printExp e ++ ")"

printTabbed :: (Int -> a -> String) -> Int -> (a -> String)
printTabbed fncn cnt = ((replicate cnt ' ') ++) . fncn cnt

printTabbedS :: Int -> (String -> String)
printTabbedS cnt = ((replicate cnt ' ') ++)

printStmt :: Int -> MacroStmt -> String
printStmt c (IfM be ms) =
  "if (" ++ printBoolExpr be ++ ") { \\\n" ++ concat (intersperse " \\\n" $ map (printTabbed printStmt $ c+2) ms) ++ " }"
printStmt c (ElifM be ms) = "else if (" ++ printBoolExpr be ++ ") { \\\n" ++ concat (intersperse " \\\n" $ map (printTabbed printStmt $ c+2) ms) ++ "  }"
printStmt c (ShadowSet mid addr typ sz) = "ShadowSet(" ++ show mid ++ ", " ++ printExp addr ++ ", " ++ typ ++ ", " ++ printExp sz ++ ");"
printStmt c (CallM fncn args) = fncn ++ "(" ++ concat (intersperse "," $ map printExp args) ++ ")"
printStmt c (NameListM ns) = concat $ intersperse ", " ns
printStmt c (BlockStmt []) = error $ "Empty BlockStmt is suspect."
printStmt c (BlockStmt [stmt]) = printTabbedS c "{ " ++ printStmt c stmt ++ " }"
printStmt c (BlockStmt (s:ss))  = printTabbedS c "{ " ++ printStmt c s ++ " \\\n" ++ concat (intersperse " \\\n" $ map (printTabbed printStmt $ c+2) ss) ++ printTabbedS c "}"
printStmt c (PlusEq lhs rhs) = printTabbedS c $ lhs ++ " += " ++ printExp rhs ++ ";"
printStmt c (InitStmt typ varID Nothing) = printTabbedS c $ typ ++ " " ++ varID ++ ";"
printStmt c (InitStmt typ varID (Just rhs)) = printTabbedS c $ typ ++ " " ++ varID ++ " = " ++ printExp rhs ++ ";"

printExp :: MacroExp -> String
printExp (ExpID s) = s
printExp (PlusC e1 e2) = printExpSafe e1 ++ " + " ++ printExpSafe e2
printExp (IntC i) = show i
printExp (CastC typ e) = "(" ++ typ ++ ")" ++ printExpSafe e

-- Safely parenthesize things.
printExpSafe (ExpID s) = s
printExpSafe (IntC i) = show i
printExpSafe other = "(" ++ printExp other ++ ")"

macro_name (MacroDef n _ _) = n
macro_args (MacroDef _ a _) = a
macro_body (MacroDef _ _ b) = b

prevSizes :: [BaseExp] -> [MacroExp]
prevSizes bes0 = let

    pS [] = []
    pS (be:bes) = Just 0 : (map (plus $ expSize be) $ pS bes)
  
    mkAccum :: [MacroExp] -> Maybe Int -> [MacroExp]
    mkAccum result Nothing = (ExpID $ "param_" ++ (show $ length result)) : result
    mkAccum result (Just sz) = (IntC sz) : result

  in reverse $ foldl mkAccum [] (pS bes0)

baseRefName :: String -> String
baseRefName n = "base_transition_FLP_" ++ n

baseRefCall :: Bool -> ([NameID], BaseExp) -> MacroStmt --String
baseRefCall is_explicit (n:[], be) = CallM (baseRefName n) $ [ExpID "__FLP_NO_ARG"] ++ map ExpID (transParams is_explicit be)
baseRefCall is_explicit (n:ns, be) = CallM (baseRefName n) $ [ExpID $ '_' : mkTypeIdentifier ns] ++ map ExpID (transParams is_explicit be)

-- | The entire size of the given BaseExp for every configuration. If two or more configurations disagree on
--   the size (or allow for unbounded repetitions) then the size is parametrized.
wholeSize :: BaseExp -> MacroExp
wholeSize be =
  case expSize be of
    Nothing -> ExpID "size_in_bytes"
    Just sz -> IntC sz

transParams :: Bool -> BaseExp -> [String]
transParams True  be = ["addr"]
transParams False be = ["addr"] ++
  case expSize be of
    Nothing -> ["size_in_bytes"]
    Just sz -> []

explicitTransParams = transParams True
implicitTransParams = transParams False

explicitTransition :: [NameID] -> BaseExp -> [(Int, BoolExpr)] -> MacroDef
explicitTransition qname be bool_exprs =
  MacroDef ("transition_FLP_" ++ mkTypeIdentifier qname) (explicitTransParams be) --["addr"]
    (StmtsBody [ baseRefCall True (qname, be) ])
  --[ EscDef ("#define transition_FLP_" ++ mkTypeIdentifier qname ++ "(addr) " ++ baseRefCall (qname, be)) fL
  --]

-- | TODO: normal transitions should handle all FLP decl forms. This is the same as explicitTransition
implicitTransition :: [NameID] -> BaseExp -> MacroDef
implicitTransition qname be = let

    just_call = baseRefCall False (qname, be)

    -- Consumed bytes initializer
    consumed_init = InitStmt "size_t" "__flp_consumed_bytes" (Just $ IntC 0)

    body | "size_in_bytes" `elem` transParams False be = [ just_call ]
         | otherwise = [ BlockStmt [consumed_init, just_call] ]
  in 
    MacroDef ("transition_FLP_" ++ mkTypeIdentifier qname) (implicitTransParams be) --["addr"]
      (StmtsBody body)
  --[ EscDef ("#define transition_FLP_" ++ mkTypeIdentifier qname ++ "(addr) " ++ baseRefCall (qname, be)) fL
  --]

-- | Inputs: top-level name, the entire subexpression, and the size in bytes of it.
mkBaseRefMacro :: Bool -> Transitions -> NameID -> BaseExp -> [Maybe Int] -> MacroDef
mkBaseRefMacro is_explicit explicit_ts n be _ = let

    isExplicit' x = isExplicit x explicit_ts

    -- Make a single ShadowSet call based on number of bytes prior to this call.
    mkOneSet :: (MacroExp, (Int, (Maybe NameID, BaseExp))) -> MacroStmt
    mkOneSet (prev_sz, (field_num, (Nothing, be'))) = error $ "Unimplemented mkOneSet: " ++ n ++ "(" ++ show be' ++ ")"
    mkOneSet (prev_sz, (field_num, (Just sub_n, be'))) = let
          curr_sz =
            case expSize be' of
              Nothing -> ExpID $ "param_" ++ show field_num
              Just sz -> IntC sz
      in  ShadowSet 0
            (PlusC (CastC "ShadowAddr" $ ExpID "addr") prev_sz)
            ("FLP##qname_type##_" ++ n ++ "_" ++ sub_n)
            curr_sz
          --[ EscDef ("    ShadowSet(0, " ++ addr_ref ++ ", " ++ curr_name  ++ ", " ++ curr_sz ++ "); \\") fL
          --]

    -- TODO: more useful error messages when input is ill-formed or something is currently unimplemented.
    lookupAlt :: NameID -> Int -> BoolExpr
    lookupAlt n i
      | not (isExplicit n explicit_ts) = error $ "Error - '" ++ n ++ "' is not actually explicitly specified for transitions."
      | otherwise =
          case lookup i (fromJust $ lookup n explicit_ts) of
            Nothing -> error $ "Error - explicit transition for type '" ++ n ++ "' does not specify "
                                ++ "type of alternative " ++ show i
            Just be -> be

    -- TODO
    singletonBranch :: String -> BaseExp -> [MacroStmt]
    singletonBranch n' be = []
      --[ EscDef ("TODO") fL ]

    -- For explicit boolean expressions.
    getIfStmt :: Int -> ([MacroStmt] -> MacroStmt)
    getIfStmt 0 = IfM (lookupAlt n 0)
    getIfStmt alt_num = ElifM (lookupAlt n alt_num)

    mFBR :: (Int, (Maybe NameID, [ (Maybe NameID, BaseExp) ])) -> MacroStmt
    mFBR (alt_num, (Just n', [(_, be)])) = getIfStmt alt_num $ singletonBranch n' be
    mFBR (alt_num, (Nothing, cases)) = getIfStmt alt_num $ map mkOneSet $ zip (prevSizes $ map snd cases) (zip [0..length cases - 1] cases)

    single_implicit_set = ShadowSet 0 (CastC "ShadowAddr" $ ExpID "addr") ("FLP##qname_type" ++ "##_" ++ n) (wholeSize be)

    implicit_set
      | "size_in_bytes" `elem` transParams False be = single_implicit_set
      | otherwise = BlockStmt [ single_implicit_set, PlusEq "__flp_bytes_consumed" (wholeSize be)]

    seqs :: [(Maybe NameID, BaseExp)]
    seqs = getSeqs be

    -- Auto-filling semantics with a "size_in_bytes" parameter passed to a type with only
    -- one branch that has an indeterminate size in a sequence of two or more pieces of memory.
    seqAutoFillBody :: MacroBody
    seqAutoFillBody = let

        -- TODO: Continue here for auto-filling transition bodies.
        sAFB :: (Maybe Int, (Maybe NameID, BaseExp)) -> MacroStmt
        sAFB (Just sz, (Just n_sub, be_sub))
          | isExplicit' n_sub = error $ "TODO: " ++ show be_sub
          | otherwise         = error $ "TODO: " ++ show be_sub

      in StmtsBody $ map sAFB $ zip (map (expSize.snd) seqs) seqs

    -- The list contains exactly one expression with a single free-variable.
    --isOneFill :: [(Maybe NameID, BaseExp)] -> Bool
    --isOneFill [] = False
    --isOneFill (n, be):bes
    --  | isExplicit n && all (/= Nothing) (expSizes be) = isOneFill bes
    --  | isExplicit n                                   = isZeroFill bes

    implicit_body = StmtsBody [implicit_set]
    --  | length seqs == 0 = error $ "Fatal Error: getSeqs cannot return an empty list. BaseExp is this: " ++ show be
      -- There's only one big thing to work with - just set it at the current type level:
    --  | length seqs == 1 = StmtsBody [implicit_set]
      -- There's only one free-variable in a sequence of 2 or more things.
    --  | isOneFill seqs = seqAutoFillBody --length (filter (== Nothing) $ map (expSize.snd) seqs) == 1 = seqAutoFillBody
      -- TODO: 'Object' consists of an explicit 'Header' which will support bytes_consumed based on the fact that
      -- each branch contains a fixed number of bytes (varied across branches). I need to restructure this codegen
      -- to associate attributes with each of the BaseExp expressions such that I don't have to keep re-figuring out
      -- how to determine the interface supported by a particular type. It's a bit like how the Rust code is generated
      -- where functions get associated with 'impls' but in this case it's with respect to the "capabilities" of a
      -- type to be able to support some mode memory sizes (particularly fixed vs unbounded and the explicit vs implicit
      -- constructions I've come up with which determine the kind of interface generated). Each node in the BaseExp should
      -- be labeled with whether or not it's explicit/implicit, and the fixity/boundedness of subexpressions.
    --  | otherwise = StmtsBody [implicit_set]

    def | is_explicit = MacroDef (baseRefName n) (baseRefParams True be) $ StmtsBody (map mFBR $ zip [0..] (getAltSeqs be))
        | otherwise   = MacroDef (baseRefName n) (baseRefParams False be) $ implicit_body

  in def

-- type Transition = (String, [(Int, BoolExpr)])
mkTransitions :: [([NameID], BaseExp)] -> Transitions -> [MacroDef]
mkTransitions qual_ns ts = let

    -- TODO: this should be nubBy or something, and there should be a preprocessing
    -- pass that checks for duplicate definitions of the same name prior to grafting.
    unique_ns = nub $ map (\(ns, be) -> (head ns, be)) qual_ns

    -- Should be unique already, but include `nub` for good measure.
    ts_names = nub $ map fst ts

    dispatchImplicitExplicit :: ([NameID], BaseExp) -> MacroDef
    dispatchImplicitExplicit (qname, be) =
      case lookup (head qname) ts of
        -- No explicit union-disambiguators specified in the FLP program.
        Nothing -> implicitTransition qname be
        -- The most-specific (last) name in the qname has a transition decl associated with it:
        (Just bool_exprs) -> explicitTransition qname be bool_exprs

    -- (EscDef (baseRefParams (qname, be) ++ " " ++ baseRefDecl bool_exprs (head qname) be) fL)
    makeBaseRef :: (NameID, BaseExp) -> MacroDef
    makeBaseRef (name, be) = mkBaseRefMacro (name `elem` ts_names) ts name be (expSizes be)
{-          let szs = expSizes be
          in if Nothing `elem` szs
                then [] -- TODO? Non-fixed size requires an extra parameter.
                else mkFixedBaseRef ts name be (map fromJust szs)-}

  in map makeBaseRef unique_ns ++ map dispatchImplicitExplicit qual_ns

genC :: [BaseExp] -> [RE] -> Transitions -> ([S.Definition], [MacroDef])
genC bes res transitions = let
    mp = mkAnalysisMap bes res transitions
    types0 = map (mkTypeIdentifier . fst) (findNames bes)
    types = "FLP_UNMAPPED" : (map ("FLP_" ++) $ catMaybes $ doFilterOut types0 res)
    num_types = MacroDef "__FLP_NUM_VALID_TYPES" [] (ExpBody $ CastC "unsigned int" $ IntC $ length types)
    xs    = map (mkInitConst . mkStrConst) types
    pound_defs = mkPoundDefs types
    initializer = CompoundInitializer xs fL
    all_prefixes = mkAllPrefixes bes res
    transition_defs = mkTransitions (findNames bes) transitions
    noarg = MacroDef "__FLP_NO_ARG" [] (StmtsBody [])
  in  ([cunit|
        static const char* const __FLP_TYPES[] = $init:initializer;
      |], noarg : pound_defs ++ [num_types] ++ all_prefixes ++ transition_defs)
      --"PC_UNMAPPED", "PC_SOMETHING_ELSE" };

-- TODO: work with Text instead of String

-- writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
-- | The pair of strings are the header and footer and to append
writeCFile :: FilePath -> (String, String) -> ([S.Definition], [MacroDef]) -> IO ()
writeCFile outdir (header, footer) (c_defs, c_macros) = do
  fd <- openFile outdir WriteMode
  hPutStr fd $ pack header
  hPutStr fd (prettyLazyText 160 $ ppr c_defs) -- 160-character wide terminal viewing width of source files (it's the 2020 people)
  hPutStr fd (pack $ printMacros c_macros) -- TODO: print the macros with correct syntax.
  hPutStr fd $ pack footer
  hClose fd

