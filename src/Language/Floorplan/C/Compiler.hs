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
import Language.Floorplan.C.Types

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
import Data.Maybe (isJust, fromMaybe, catMaybes, fromJust, isNothing)
import Data.Bits
import qualified Debug.Trace as D

import Language.C.Quote.C
import qualified Language.C.Syntax as S
import Data.Loc -- (SrcLoc(..), noLoc)

import Text.RE.TDFA.String (RE(..), (?=~), matched)
import qualified Data.Map.Lazy as M
import Control.Monad.State.Lazy (State(..), runState, put, get)

-- TODO: Introduce spans based on position in these Haskell files.
fakeLoc = noLoc --Span (Position 0 0 0) (Position 0 0 0)
fL = fakeLoc

justNameExp :: BaseExp -> Maybe (NameID, BaseExp)
justNameExp (n ::: e) = Just (n, e)
justNameExp _ = Nothing

mkInitConst s = (Nothing, ExpInitializer (Const s fL) fL)

mkStrConst t = S.StringConst ['"' : t ++ "\""] t fakeLoc

mkPoundDefs' :: Int -> [NameID] ->  [MacroDef]
mkPoundDefs' i (n:ns) = (MacroDef n [] $ ExpBody True $ CastC "__FLP_Entry" $ IntC i) : (mkPoundDefs' (i+1) ns)
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

baseRefName :: String -> String
baseRefName n = "base_transition_FLP_" ++ n

-- | The entire size of the given BaseExp for every configuration. If two or more configurations disagree on
--   the size (or allow for unbounded repetitions) then the size is parametrized.
wholeSize :: BaseExp -> MacroExp
wholeSize be =
  case expSize be of
    Nothing -> ExpID "size_in_bytes"
    Just sz -> IntC sz

baseRefCall :: LabelMap -> ([NameID], BaseExp) -> MacroStmt --String
baseRefCall mp (n:[], be) = CallM (baseRefName n) $ [ExpNoArg] -- This is the "empty" case (empty qualified name)
                                              ++ map ExpID (n `trans_params` mp)
baseRefCall mp (n:ns, be) = CallM (baseRefName n) $ [ExpID $ '_' : mkTypeIdentifier ns] ++ map ExpID (n `trans_params` mp)

-- | Inputs: top-level name, the entire subexpression, and the size in bytes of it.
mkBaseRefMacro :: LabelMap -> (NameID, BaseExp) -> State CompilerEnv MacroDef
mkBaseRefMacro mp (n, be) = let

    {- -- Make a single PSet call based on number of bytes prior to this call.
    mkOneSet :: (MacroExp, (Int, (Maybe NameID, BaseExp))) -> MacroStmt
    mkOneSet (prev_sz, (field_num, (Nothing, be'))) = error $ "Unimplemented mkOneSet: " ++ n ++ "(" ++ show be' ++ ")"
    mkOneSet (prev_sz, (field_num, (Just sub_n, be'))) = let
          curr_sz =
            case expSize be' of
              Nothing -> ExpID $ "param_" ++ show field_num
              Just sz -> IntC sz
      in  PSet 0
            (PlusC (CastC "ShadowAddr" $ ExpID "addr") prev_sz)
            ("FLP##qname_type##_" ++ n ++ "_" ++ sub_n)
            curr_sz -}

    lookupAlt :: NameID -> Int -> BoolExpr
    lookupAlt n i =
      case lookup i (n `explicit_bools` mp) of
        Nothing -> error $ "Error - explicit transition for type '" ++ n ++ "' does not specify " ++ "type of alternative " ++ show i
        Just be -> be

    -- TODO: was this right?
    singletonBranch :: String -> String -> BaseExp -> [MacroStmt]
    singletonBranch n' addrN be =
      [ CallM (baseRefName n') $
          [ ExpID $ "qname_type##_" ++ n
          , ExpID addrN
          ] ++ (if n' `fixed_size` mp then [] else [ExpID "size_in_bytes"])
      ]

    -- For explicit boolean expressions.
    getIfStmt :: Int -> ([MacroStmt] -> MacroStmt)
    getIfStmt 0 = IfM (lookupAlt n 0)
    getIfStmt alt_num = ElifM (lookupAlt n alt_num)

    mkOneCall :: String -> String -> (Int, (Maybe NameID, BaseExp)) -> MacroStmt
    mkOneCall fls addrN (field_num, (Nothing, be')) = error $ "Unimplemented mkOneCall: " ++ n ++ "(" ++ show be' ++ ")"
    mkOneCall fls addrN (field_num, (Just sub_n, be'))
      | n `fixed_size` mp = CallM (baseRefName sub_n) $ [ ExpID $ "qname_type##_" ++ n ]
      | otherwise         =
          CallM (baseRefName sub_n)
          [ ExpID $ "qname_type##_" ++ n
          , PlusC (ExpID addrN)
                  (MinusC (ExpID "__flp_bytes_consumed") (ExpID fls))
          ]

    -- First parameter is the uncapturable name we generated to track the number of bytes
    -- consumed prior to the code generated by this `mFBR` function:
    mFBR :: String -> String -> (Int, (Maybe NameID, [ (Maybe NameID, BaseExp) ])) -> MacroStmt
    mFBR fls addrN (alt_num, (Just n', [(_, be)])) = getIfStmt alt_num $ singletonBranch n' addrN be
    mFBR fls addrN (alt_num, (Nothing, cases)) = getIfStmt alt_num $ map (mkOneCall fls addrN) $ zip [0..length cases - 1] cases

    single_implicit_set :: String -> MacroStmt
    single_implicit_set addrN = PSet 0 (ExpID addrN) ("FLP##qname_type##_" ++ n) (wholeSize be)

    implicit_set :: String -> MacroStmt
    implicit_set addrN
      | "size_in_bytes" `elem` (n `trans_params` mp) = BlockStmt [ single_implicit_set addrN, PlusEq "__flp_bytes_consumed" (ExpID "size_in_bytes") ]
      | otherwise = BlockStmt [ single_implicit_set addrN, PlusEq "__flp_bytes_consumed" (wholeSize be)]

    seqs :: [(Maybe NameID, BaseExp)]
    seqs = n `sequences` mp
    seqs_ns = map fst seqs

    -- Figure out how to compute the fill size based on surrounding expressions and the requested
    -- total size (size_in_bytes) of all of the expressions in this group summed together, *excluding*
    -- (all others) the expression with an indeterminate size.
    mkFillSizeOthers :: MacroExp
    mkFillSizeOthers = let -- n_sub =
      
        mFSO :: (Maybe NameID, BaseExp) -> MacroExp
        mFSO (Nothing, be) = -- Anonymous expressions don't have an entry in `mp`, but we just want the size.
          case expSize be of
            Nothing -> IntC 0 -- The thing with indeterminate size doesn't contribute to the size we want.
            Just sz -> IntC sz
        mFSO (Just n, _) =
          case n `size_calc` mp of
            Nothing -> IntC 0 -- Same as above, indeterminate thing doesn't contribute.
            Just calc -> calc -- Analysis pass precomputed the size we want, taking into account the messy nature of explicit boolean expressions so we don't have to handle that here.

      in foldl PlusC (IntC 0) $ map mFSO seqs --(fromJust . (flip size_calc $ mp) . fromJust) $ filter (/= Just n_sub) seqs_ns

    -- Auto-filling semantics with a "size_in_bytes" parameter passed to a type with only
    -- one branch that has an indeterminate size in a sequence of two or more pieces of memory.
    --
    -- Precondition: This function is only called if `num_free_fills` is precisely `1`, meaning
    -- we can ignore `Nothings` in size calculation functions by assuming that the Nothing computed
    -- by expSize in those instances exactly corresponds to the function free-filled function. More careful
    -- logic will be necessary to implement parametrization of 2 or more sizes in the presence of 2 or more
    -- `num_free_fills`.
    seqAutoFillBody :: String -> State CompilerEnv [MacroStmt]
    seqAutoFillBody addrN = let

        -- First parameter is the uncapturable name we've created explicitly for the purposes of
        -- calls generated by this `sAFB` function:
        sAFB :: String -> (Maybe NameID, BaseExp) -> MacroStmt
        sAFB fls (Just n_sub, be_sub)
          | n_sub `requires_size_arg` mp = CallM (baseRefName n_sub) $
              [ ExpID $ "qname_type##_" ++ n
              , PlusC (ExpID addrN) (MinusC (ExpID "__flp_bytes_consumed") (ExpID fls))
              , MinusC (ExpID "size_in_bytes") mkFillSizeOthers -- Total size requested, minus surrounding sizes.
              ]
          | otherwise = CallM (baseRefName n_sub) $
              [ ExpID $ "qname_type##_" ++ n
              , PlusC (ExpID addrN) (MinusC (ExpID "__flp_bytes_consumed") (ExpID fls))
              ]
        -- Anonymous sub-layout:
        sAFB fls (Nothing, be_sub)
          | isNothing (expSize be_sub) = BlockStmt -- Variable sized sub-layout
              [ -- This InitStmt can get large, so store it in a local variable since it gets used twice.
                InitStmt "size_t" "__flp_local_sz" (Just $ MinusC (ExpID "size_in_bytes") mkFillSizeOthers)
              , PSet 0
                  (PlusC (ExpID addrN) (MinusC (ExpID "__flp_bytes_consumed") (ExpID fls)))
                  ("FLP##qname_type##_" ++ n) (ExpID "__flp_local_sz")
              , PlusEq "__flp_bytes_consumed" (ExpID "__flp_local_sz")
              ] 
          | otherwise = BlockStmt -- Fixed-size sub-layout
              [ PSet 0
                  (PlusC (ExpID addrN) (MinusC (ExpID "__flp_bytes_consumed") (ExpID fls)))
                  ("FLP##qname_type##_" ++ n) (IntC $ fromJust $ expSize be_sub)
              , PlusEq "__flp_bytes_consumed" (IntC $ fromJust $ expSize be_sub)
              ]
        -- = error $ "sAFB: " ++ show be_sub ++ "(ctx=> " ++ show (n,be) ++ ")"

      in do fls <- newName "__flp_local_start"
            return  [ BlockStmt $
                      [ InitStmt "size_t" fls (Just $ ExpID "__flp_bytes_consumed")
                      ] ++ map (sAFB fls) seqs
                    ]

    -- Mimic call-by-value semantics in our macros.
    initGrabAddr addr_name = InitStmt "ShadowAddr" addr_name (Just $ CastC "ShadowAddr" $ ExpID "addr")

    body :: State CompilerEnv MacroBody
    body  | n `is_explicit_ts` mp =
              do  fls <- newName "__flp_local_start"
                  addr_name <- newName "__flp_addr"
                  return $ StmtsBody $
                    [ BlockStmt $ [ initGrabAddr addr_name -- grab the address and save it.
                                  , InitStmt "size_t" fls (Just $ ExpID "__flp_bytes_consumed") ]
                                  ++ (map (mFBR fls addr_name) $ zip [0..] (n `alt_sequences` mp))
                    ]
          | n `num_free_fills` mp == 1 =
              do  addr_name <- newName "__flp_addr"
                  stmts <- seqAutoFillBody addr_name
                  return $ StmtsBody $ ((initGrabAddr addr_name) : stmts)
          | otherwise                  = 
              do  addr_name <- newName "__flp_addr"
                  return $ StmtsBody [initGrabAddr addr_name, implicit_set addr_name]

  in MacroDef (baseRefName n) (n `base_ref_params` mp) <$> body

mkTransitions :: LabelMap -> [([NameID], BaseExp)] -> State CompilerEnv [MacroDef]
mkTransitions mp qual_ns = let
    unique_ns = nub $ map (\(ns, be) -> (head ns, be)) qual_ns
    mkT :: ([NameID], BaseExp) -> MacroDef
    mkT (qname@(n:_), be) = MacroDef
      ("transition_FLP_" ++ mkTypeIdentifier qname)
      (n `trans_params` mp)
      (StmtsBody [ BlockStmt
        [ InitStmt "size_t" "__flp_bytes_consumed" (Just $ IntC 0)
        , baseRefCall mp (qname, be)
        ]
      ])
  in do macro <- mapM (mkBaseRefMacro mp) unique_ns
        return $ macro ++ map mkT qual_ns
  --map (mkBaseRefMacro mp) unique_ns ++ map mkT qual_ns

genC :: [BaseExp] -> [RE] -> Transitions -> IO ([S.Definition], [MacroDef])
genC bes res transitions = let
    mp = mkAnalysisMap bes res transitions
    types0 = map (mkTypeIdentifier . fst) (findNames bes)
    types = "FLP_UNMAPPED" : (map ("FLP_" ++) $ catMaybes $ doFilterOut types0 res)
    num_types = MacroDef "__FLP_NUM_VALID_TYPES" [] (ExpBody True $ CastC "__FLP_Entry" $ IntC $ length types)
    xs    = map (mkInitConst . mkStrConst) types
    pound_defs = mkPoundDefs types
    initializer = CompoundInitializer xs fL
    all_prefixes = mkAllPrefixes bes res
    --noarg = MacroDef "__FLP_NO_ARG" [] (StmtsBody [])
    typ_typedef = MacroDef "__FLP_Entry" [] (ExpBody False $ ExpID "unsigned short int")
    (transition_defs,_) = runState (mkTransitions mp (findNames bes)) (CompilerEnv 0)
  in return ([cunit|
                static const char* const __FLP_TYPES[] = $init:initializer;
            |], typ_typedef : pound_defs ++ [num_types] ++ all_prefixes ++ transition_defs)

-- | The pair of strings are the header and footer and to append
writeCFile :: FilePath -> (String, String) -> ([S.Definition], [MacroDef]) -> IO ()
writeCFile outdir (header, footer) (c_defs, c_macros) = do
  fd <- openFile outdir WriteMode
  hPutStr fd $ pack header
  hPutStr fd (prettyLazyText 160 $ ppr c_defs) -- 160-character wide terminal viewing width of source files (it's the 2020 people)
  hPutStr fd (pack $ printMacros c_macros) -- TODO: print the macros with correct syntax.
  hPutStr fd $ pack footer
  hClose fd
