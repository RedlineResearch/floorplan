module Language.Floorplan.Preproc.Passes where
import Language.Floorplan.Core.Syntax hiding (accum)
import Language.Floorplan.Syntax
import Language.Floorplan.Preproc.Types

import qualified Data.Map.Strict as M
import Data.List (sort, nub, tails)
import Data.Maybe (fromJust, isJust)
import Debug.Trace as D

import Text.RE.TDFA.String (RE(..), compileRegex)
--import Control.Exception.Base (IOException(..)) --(SomeException(..), catch, IOException(..))
--import Control.Exception
--import Prelude hiding (catch)
import GHC.IO.Exception
import Control.Exception (catch)

uniquePairs :: [a] -> [(a,a)]
uniquePairs lst = [ (x,y) | (x:ys) <- tails lst, y <- ys ]

-- | The two bool expressions compute different truth tables over the free-variables of
--   the two expressions (at least one True/False or False/True).
notIdentical :: BoolExpr -> BoolExpr -> Bool
notIdentical b1 b2 =
  any id [ r1 /= r2
      | let free_vars = nub $ (onlyBoolIDs b1 ++ onlyBoolIDs b2)
      , assignment <- sequence $ replicate (length free_vars) [True, False]
      , let ctx = zip free_vars assignment
      , let r1 = evalBool ctx b1
      , let r2 = evalBool ctx b2
      ]

transitionAnalysis :: [Decl] -> ([PreprocError], Transitions)
transitionAnalysis ds = let
    
    ts :: [(String, Int, BoolExpr)]
    ts = onlyTransitions ds

    unique_ns :: [String]
    unique_ns = nub $ map (\(n,_,_) -> n) ts

    -- | Each `fst` (name) in this list is unique, paired with all the corresponding
    --   transition declarations seen for that name in the input program.
    uniques :: [(String, [(Int, BoolExpr)])]
    uniques = [ (n, pairs)
              | n <- unique_ns
              , let pairs = map (\(_,b,c) -> (b,c)) $ filter (\(n',_,_) -> n' == n) ts
              ]

    disjointUnionNums :: Transitions -> [PreprocError]
    disjointUnionNums [] = [] -- No errors
    disjointUnionNums ((n, []) : rest) = disjointUnionNums rest
    disjointUnionNums ((n, (union_num, bool_expr) : pairs) : rest)
      | union_num `elem` (map fst pairs) = [DuplicateTransitionNum n union_num]
      | otherwise = disjointUnionNums rest
    
    disjointBoolExprs :: [PreprocError]
    disjointBoolExprs = let
        dBE (n, []) = [] -- No errors
        dBE (n, pairs) =
          [ IdenticalBools n b1 b2
          | (b1, b2) <- uniquePairs (map snd pairs)
          , not (notIdentical b1 b2) -- Find ones that *are* identical (double not)
          ]
      in concatMap dBE uniques

  in (disjointUnionNums uniques ++ disjointBoolExprs, uniques)


-- TODO: analysis to check for multiple header / footer declarations in the input. Unless
-- we want to just output headers / footers in the order in which they appear in the input file.

-- TODO: the two String types should be given names (headers and footers) to be
-- used throughout the code.

-- | For now the header/footer analysis only produces an error when it sees a 'header' or
--   or 'footer' attribute occur in conjunction with 
headFootAnalysis :: [Decl] -> ([PreprocError], String, String)
headFootAnalysis [] = ([], [], [])
headFootAnalysis (HeaderDecl s : ds) =
  let (errs, hds, foots) = headFootAnalysis ds
  in  (errs, s ++ hds, foots)
headFootAnalysis (FooterDecl s : ds) =
  let (errs, hds, foots) = headFootAnalysis ds
  in  (errs, hds, s ++ foots)
headFootAnalysis (ScopeDecl as : ds)
  | OutputHeader `elem` as =
      let (errs, hds, foots) = headFootAnalysis ds
          new_err = InvalidAttribute $ "Error: 'header' scope attribute cannot occur with other attributes: " ++ show as
      in  (new_err : errs, hds, foots)
  | OutputFooter `elem` as =
      let (errs, hds, foots) = headFootAnalysis ds
          new_err = InvalidAttribute $ "Error: 'footer' scope attribute cannot occur with other attributes: " ++ show as
      in  (new_err : errs, hds, foots)
  | otherwise = headFootAnalysis ds
headFootAnalysis (_ : ds) = headFootAnalysis ds

balancedScopeAnalysis :: [Decl] -> [PreprocError]
balancedScopeAnalysis ds = let
    bSA :: Int -> [Decl] -> [PreprocError]
    bSA 0 [] = []
    bSA n [] = [UnbalancedScope n]
    bSA n (ScopeDecl _ : ds') = bSA (n+1) ds'
    bSA n (EndScopeDecl : ds') = bSA (n-1) ds'
    bSA n (_ : ds') = bSA n ds'
  in bSA 0 ds

duplicateAttrAnalysis :: [Decl] -> [PreprocError]
duplicateAttrAnalysis ds = let
    dAA (ScopeDecl as)
      | length (nub as) == length as = [DuplicateAttribute as]
      | otherwise = []
    dAA _ = []
  in concatMap dAA ds

handleRegexFail :: IOException -> IO (Either PreprocError RE)
handleRegexFail (IOError _ _ _ err _ _) = return $ Left $ RegexError err
--handleRegexFail (IOError Nothing UserError [] err) = return $ Left $ RegexError err


regexAnalysis :: [Decl] -> IO [Either PreprocError RE]
regexAnalysis [] = return []
regexAnalysis (FilterOutDecl re : ds') = do
  re   <- catch (Right <$> compileRegex re) handleRegexFail
  rest <- regexAnalysis ds'
  return $ re : rest
regexAnalysis (_ : ds') = regexAnalysis ds'

-- | Build the dictionary / map of layer (and field) identifiers to their inner Demarc.
--   Todo: warning when multiple layers have the same name (which one `union` picks is undefined behavior)
buildMap :: Demarc -> M.Map String Demarc
buildMap (Enum{})  = M.empty
buildMap (Bits{})  = M.empty
buildMap (Union ds) = M.unions $ map buildMap ds
buildMap (Seq ds) = M.unions $ map buildMap ds
buildMap (PtrF{}) = M.empty
buildMap (PtrL{}) = M.empty
buildMap (Blob{}) = M.empty
buildMap (Graft{}) = M.empty
buildMap f@(Field name d) = M.insert name d (buildMap d)
buildMap (Pound d) = buildMap d
buildMap (Repetition _ d) = buildMap d
buildMap l@(Layer{}) = M.insert (name l) l (buildMap $ rhs l)

-- | Detect any loop that might arise during the grafting process, returning a witness,
--   e.g. Left ["A", "B", "C"], if there exists a grafting loop by traversing A->B->C->A.
graftingAnalysis :: [Decl] -> [PreprocError]
graftingAnalysis ds_init = let

    ds_map = M.unions $ map buildMap (onlyLayers ds_init)

    lookup lid =
      case M.lookup lid ds_map of
        Nothing -> Left $ UndefinedSymbol lid
        Just d -> Right d

    gr :: [String] -> Demarc -> [PreprocError]
    gr as d@(Enum{})         = []
    gr as d@(Bits{})         = []
    gr as d@(PtrF{})         = []
    gr as d@(PtrL{})         = []
    gr as d@(Blob{})         = []
    gr as d@(Union ds)       = concatMap (gr as) ds
    gr as d@(Seq ds)         = concatMap (gr as) ds
    gr as   (Pound d)        = gr as d
    gr as   (Repetition _ d) = gr as d
    gr as   (Field fid d)
      | fid `elem` as = [Recursive $ fid : as]
      | otherwise     = (gr $ fid : as) d
    gr as d@(Layer{})
      | (name d) `elem` as = [Recursive $ name d : as]
      | otherwise = gr (name d : as) (rhs d)
    gr as (Graft (lid, args))
      | lid `elem` as = [Recursive $ lid : as]
      | otherwise =
          case lookup lid of
            Left err -> [err]
            Right (Field _ d) -> gr (lid : as) d
            Right (l@(Layer{})) -> {- D.trace lid $ -} gr (lid : as) (rhs l)

    gr_decl (LayerDecl d) = gr [] d
    gr_decl _ = []

  in concatMap gr_decl ds_init

-- | Grafting pre-processing pass.
grafting' :: M.Map String Demarc -> Demarc -> Demarc
grafting' ds demarc = let

    lookup lid =
      case M.lookup lid ds of
        Nothing -> error $ "Fatal Error: undefined symbol '" ++ lid ++ "' during graft pre-processing phase."
        Just d -> d

    -- | Bool is whether or not we changed the input value
    gr (Graft (lid, args)) =
      case lookup lid of
        (Field _ d)   -> (d, True)
        (l@(Layer{})) -> (l, True) -- TODO: replace formals with args (no partial application)
    gr d = (d, False)

  in case fmapD gr demarc of
      (d, True)   -> grafting' ds d
      (d, False)  -> d

-- | All layers (including non-globally defined ones) are given as the first argument,
--   while the second argument should only be passed globally defined ones, i.e. the
--   ones we want to be globally referenceable. This function will still work correctly
--   on non-global layers, but those layers will already be expanded anyways during processing
--   of other layers which reference them.
grafting :: [Demarc] -> Demarc -> Demarc
grafting ds demarc = grafting' (M.unions $ map buildMap ds) demarc

-- | Precondition: scopes are balanced.
--   Postcondition: only Demarcs which should be globally defined are returned.
removeNoGlobalPass :: [Decl] -> [Demarc]
removeNoGlobalPass ds = let
    rNGP :: Int -> [[ScopeAttribute]] -> [Decl] -> [Demarc]
    rNGP n ctx (LayerDecl d : ds')
      | n > 0     = rNGP n ctx ds'     -- skip this decl
      | otherwise = d : rNGP n ctx ds' -- keep this decl
    rNGP n ctx (ScopeDecl as : ds')
      | NoGlobalAttr `elem` as = rNGP (n + 1) (as : ctx) ds' -- start ignoring decls
      | otherwise              = rNGP n (as : ctx) ds'       -- keep doing the same as before
    rNGP n (as : ctx) (EndScopeDecl : ds')
      | NoGlobalAttr `elem` as = rNGP (n - 1) ctx ds' -- Stop ignoring decls (or at least, reduce by one level of ignoring), and pop the top of the ctx stack.
      | otherwise              = rNGP n ctx ds'       -- Not ending a NoGlobalAttr scope, pop the top of the ctx stack and move on.
    rNGP n ctx (_ : ds') = rNGP n ctx ds'
    rNGP 0 [] [] = []
    rNGP n ctx [] = error $ "Internal Error - Bad Scope: n=" ++ show n ++ ", ctx=" ++ show ctx
  in rNGP 0 [] ds -- 0 implies a default of globally keeping a decl
