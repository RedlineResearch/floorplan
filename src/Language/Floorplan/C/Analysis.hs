{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.C.Analysis 
  ( mkAnalysisMap, LabelEntry(..), LabelMap(..), mkTypeIdentifier, isExplicit
  , findNames, baseRefParams, prevSizes
  ) where

import System.IO (IOMode(..), openFile, hClose)
import Data.Typeable (Typeable(..))
import Data.Text.Lazy.IO (hPutStr)
import Data.Text.Lazy (pack)

import Language.C.Quote
import qualified Language.C.Parser as P
import qualified Language.C.Parser.Tokens as T
import qualified Language.C.Syntax as C
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
import Data.Maybe (isJust, fromMaybe, catMaybes, fromJust)
import Data.Bits
import qualified Debug.Trace as D

import Language.C.Quote.C
import qualified Language.C.Syntax as S
import Data.Loc -- (SrcLoc(..), noLoc)

import Text.RE.TDFA.String (RE(..), (?=~), matched)

import qualified Data.Map.Lazy as M

-- TODO: callees in other file think this takes a tuple.
-- | (["A", "B", "C"], _) becomes "C_B_A"
mkTypeIdentifier :: [NameID] -> String
mkTypeIdentifier = concat . intersperse "_" . reverse

-- | Get all of the unique name identifiers in the spec and their
--   corresponding subexpression.
findNames :: [BaseExp] -> [([NameID], BaseExp)]
findNames = nub . concatMap (accumTopDown justName)

justName :: [([NameID], BaseExp)] -> BaseExp -> Maybe ([NameID], BaseExp)
justName [] (n ::: e) = Just ([n], e)
justName ((ns,_):_) (n ::: e) = Just (n : ns, e)
justName _ _ = Nothing

baseRefParams :: Bool -> BaseExp -> [String]
baseRefParams True be
  | numFreeFills be == 0 = ["qname_type", "addr"] -- purposely weaker test here for excluding size paramter.
  | otherwise            = ["qname_type", "addr"] ++ maybeSizeParam be
baseRefParams False be = ["qname_type", "addr"] ++ maybeSizeParam be

isExplicit :: NameID -> Transitions -> Bool
isExplicit n ts =
  case lookup n ts of
    Nothing -> False
    Just _  -> True

maybeSizeParam be =
  case expSize be of
    Nothing -> ["size_in_bytes"] -- Extra parameter when the size is indeterminate
    Just sz -> []

transParams :: Bool -> BaseExp -> [String]
transParams True  be
  | numFreeFills be == 0 = ["addr"] -- weaker test of fixed-sizedness. nested explicit transitions should work here, but probably gross in general to use.
  | otherwise            = ["addr"] ++ maybeSizeParam be
transParams False be     = ["addr"] ++ maybeSizeParam be


--type BaseExp = FLPS.Exp AttrBaseExp

-- Does the given qualified type name match any of the given regular expressions?
-- If so, it's filtered out.
isFilteredOut :: [NameID] -> [RE] -> Bool
isFilteredOut qs [] = False
isFilteredOut [] _  = error $ "Invalid empty qualified name during filtering."
isFilteredOut qs (re:res)
  | matched (mkTypeIdentifier qs ?=~ re) = True
  | otherwise = isFilteredOut qs res

prevSizes :: [BaseExp] -> [MacroExp]
prevSizes bes0 = let

    pS [] = []
    pS (be:bes) = Just 0 : (map (plus $ expSize be) $ pS bes)
  
    mkAccum :: [MacroExp] -> Maybe Int -> [MacroExp]
    mkAccum result Nothing = (ExpID $ "param_" ++ (show $ length result)) : result
    mkAccum result (Just sz) = (IntC sz) : result

  in reverse $ foldl mkAccum [] (pS bes0)

  -- | Input: the related transitions, expSize e, alt seqs.
  --   Output: Expression computing the size of calling `base_transition_...` for the associated type.
szCalc :: NameID -> [(Int, BoolExpr)] -> Maybe Int -> [ (Maybe NameID, [ (Maybe NameID, BaseExp) ]) ] -> Maybe MacroExp
szCalc _ _  (Just i) _ = Just $ IntC i
szCalc _ [] Nothing  _ = Nothing
szCalc n explicit_ts Nothing alt_seqs = let

    lookupAlt :: Int -> BoolExpr
    lookupAlt i =
      case lookup i explicit_ts of
        Nothing -> error $ "Error - explicit transition for type '" ++ n ++ "' does not specify type of alternative " ++ show i
        Just be -> be

    sC :: Maybe MacroExp -> (Int, (Maybe NameID, [ (Maybe NameID, BaseExp) ])) -> Maybe MacroExp
    sC accum (alt_num, (Just n', [(_, be)])) = Nothing
    sC accum (alt_num, (Nothing, cases)) = do
      sz <- foldl plus (Just 0) (map (expSize . snd) cases)
      TernaryIf (lookupAlt alt_num) (IntC sz) <$> accum

  in foldl sC (Just $ IntC 0) $ zip [0..] alt_seqs

mkAnalysisMap :: [BaseExp] -> [RE] -> Transitions -> LabelMap
mkAnalysisMap bes res transitions = let
    
    -- Should be unique already, but include `nub` for good measure.
    ts_names = nub $ map fst transitions

    -- Third parameter contains all the LabelEntry results from calling this
    -- function on the subtypes of the currently named type.
    mkLabelEntry :: [NameID] -> BaseExp -> LabelMap -> LabelEntry
    mkLabelEntry (n:ns) e recurs_map = let
        ise = isExplicit n transitions
        bools =
          case lookup n transitions of
            Nothing -> []
            Just r  -> r
        sz = expSize e
        szs = expSizes e
        nff = numFreeFills e
        seqs = getSeq e
        subtys = findNames [e]
        brp = baseRefParams ise e
        isfo = isFilteredOut (n:ns) res
        trans_params = transParams ise e
        alt_seq = getAltSeq e
      in LabelEntry ise bools sz szs nff
            seqs subtys brp isfo (expSize e) (expSizes e)
            trans_params alt_seq (expSize e /= Nothing)
            ("size_in_bytes" `elem` trans_params)
            (szCalc n bools sz alt_seq)

    mkAM qs (Prim n) = M.empty
    mkAM qs (Con n e) = mkAM qs e
    mkAM qs (e :@ a) = mkAM qs e
    mkAM qs (e1 :+ e2) = (mkAM qs e1) `M.union` (mkAM qs e2)
    mkAM qs (e1 :|| e2) = (mkAM qs e1) `M.union` (mkAM qs e2)
    mkAM qs (n ::: e) =
      let recur = mkAM (n:qs) e
      in  M.insert n (mkLabelEntry (n:qs) e recur) recur
    mkAM qs (Exists s e) = mkAM qs e
    mkAM qs (s :# e) = mkAM qs e
    mkAM qs (FLPS.Attr a e) = mkAM qs e

  in M.unions $ map (mkAM []) bes

-- | Number of ostensibly unbounded free-filling operations in the given expression `be`
--   which ultimately affect the total size of `be`. Notably a constrained (`Con`) will
--   not contribute unboundedness to the expression by definition because any subexpression
--   to the `Con` must fit into exactly a statically known number of bytes. TODO: preprocessing
--   analysis which verifies the satisfiability of layouts with a `Cons` in them. Unsatisfiable
--   layouts are compiler errors early in the process.
numFreeFills :: BaseExp -> Int
numFreeFills be = let
    nFF (Prim _) = 0
    nFF (Con _ be) = 0
    nFF (be :@ _) = nFF be
    nFF (e1 :+ e2) = nFF e1 + nFF e2
    nFF (e1 :|| e2) = nFF e1 + nFF e2
    nFF (n ::: be) = nFF be
    nFF (Exists _ be) = nFF be
    nFF (n :# be) = 1
    nFF (FLPS.Attr a e) = nFF e
  in nFF be

-- For now everything needs to have an explicit name, but once the codegen is working
-- smoothly we should relax this restriction and reference a library of primitive
-- transitions and types in the generated C code for specs which don't wish to give
-- an explicit name to everything (such as for padding, though it seems like good
-- memory layout hygiene to specify these names anyways).
getSeq :: BaseExp -> [ (Maybe NameID, BaseExp) ]
getSeq be@(Prim _) = [(Nothing, be)]
getSeq (Con _ be) = getSeq be
getSeq (be :@ _) = getSeq be
getSeq (e1 :+ e2) = getSeq e1 ++ getSeq e2
getSeq be@(e1 :|| e2) = [(Nothing, be)]
getSeq (n ::: be) = [ (Just n, be) ]
getSeq (Exists _ be) = getSeq be
getSeq be@(n :# be') = [(Nothing, be)]
getSeq be@(FLPS.Attr _ be') = [(Nothing, be)]

-- | Get an ordered list of the alternatives which make up this expression
--   and also (safely) pushing constraints down into the returned BaseExps
--   wherever possible to retain that information.
getAltSeq :: BaseExp -> [ (Maybe NameID, [ (Maybe NameID, BaseExp) ]) ]
getAltSeq be@(Prim _) = error $ "Unimplemented getAltSeq: " ++ show be -- [("FLP_Primitive_Bytes", fn be)] -- TODO?
getAltSeq (Con n be) = getAltSeq be
getAltSeq (be :@ align) = getAltSeq be
-- Constraints function cannot be applied to sequences:
getAltSeq (e1 :+ e2) = [ (Nothing, getSeq e1 ++ getSeq e2) ] -- This is the case I'm modeling against (union of 4 unnamed seqs).
getAltSeq (e1 :|| e2) = getAltSeq e1 ++ getAltSeq e2
getAltSeq (n ::: be) = [ (Just n, [(Nothing, be)]) ]
-- NOTE: valid existential configurations of a memory are not preserved by this transformation. You cannot
-- assume the results of this function produce coherent BaseExp layouts wrt existentials:
getAltSeq (Exists n be) = getAltSeq be
getAltSeq be@(n :# be') = error $ "Unimplemented getAltSeq: " ++ show be
getAltSeq (FLPS.Attr (Contains _) be) = getAltSeq be
getAltSeq be@(FLPS.Attr (BaseType (EnumBT _)) be') = error $ "Unimplemented getAltSeq: " ++ show be
getAltSeq be@(FLPS.Attr (BaseType (BitsBT _)) be') = error $ "Unimplemented getAltSeq: " ++ show be
getAltSeq be@(FLPS.Attr (BaseType (PtrBT _)) be')  = error $ "Unimplemented getAltSeq: " ++ show be
getAltSeq be@(FLPS.Attr (BaseType (SizeBT _)) be') = error $ "Unimplemented getAltSeq: " ++ show be
