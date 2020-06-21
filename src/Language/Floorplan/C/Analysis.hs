{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.C.Analysis 
  ( mkAnalysisMap, LabelEntry(..), LabelMap(..), mkTypeIdentifier, isExplicit
  , getSeqs, findNames, baseRefParams
  ) where

import System.IO (IOMode(..), openFile, hClose)
import Data.Typeable (Typeable(..))
import Data.Text.Lazy.IO (hPutStr)
import Data.Text.Lazy (pack)

import Language.C.Quote
import qualified Language.C.Parser as P
import qualified Language.C.Parser.Tokens as T
import qualified Language.C.Syntax as C

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

data LabelEntry = LabelEntry -- Attribute for labels in the FLPS.Exp
  { is_explicit_ts :: Bool
  , explicit_bools :: [(Int, BoolExpr)]
  , attr_sz        :: Maybe Int
  , distinct_sizes :: [Maybe Int]
  , num_free_fills :: Int
  , sequences      :: [(Maybe NameID, BaseExp)]
  , subtypes       :: [([NameID], BaseExp)]   -- Names that can be found as subexpressions of the attributed expression.
  , base_ref_params :: [String]          -- parameters to the 'base_transition_...' macro of the attributed expression.
  , is_filtered_out :: Bool -- Is the qualified name associated with this entry filtered out by one of the regular expressions.
  }
  deriving (Eq, Ord, Show)

type LabelMap = M.Map [NameID] LabelEntry

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
baseRefParams is_explicit be
  | is_explicit = ["qname_type", "addr"]
  | otherwise   = ["qname_type", "addr"] ++ 
      case expSize be of
        Nothing -> ["size_in_bytes"] -- Extra parameter when the size is indeterminate
        Just sz -> []

isExplicit :: NameID -> Transitions -> Bool
isExplicit n ts =
  case lookup n ts of
    Nothing -> False
    Just _  -> True

--type BaseExp = FLPS.Exp AttrBaseExp

-- Does the given qualified type name match any of the given regular expressions?
-- If so, it's filtered out.
isFilteredOut :: [NameID] -> [RE] -> Bool
isFilteredOut qs [] = False
isFilteredOut [] _  = error $ "Invalid empty qualified name during filtering."
isFilteredOut qs (re:res)
  | matched (mkTypeIdentifier qs ?=~ re) = True
  | otherwise = isFilteredOut qs res

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
        nff = 0 -- TODO
        seqs = getSeqs e
        subtys = findNames [e]
        brp = baseRefParams ise e
        isfo = isFilteredOut (n:ns) res
      in LabelEntry ise bools sz szs nff seqs subtys brp isfo

    mkAM qs (Prim n) = M.empty
    mkAM qs (Con n e) = mkAM qs e
    mkAM qs (e :@ a) = mkAM qs e
    mkAM qs (e1 :+ e2) = (mkAM qs e1) `M.union` (mkAM qs e2)
    mkAM qs (e1 :|| e2) = (mkAM qs e1) `M.union` (mkAM qs e2)
    mkAM qs (n ::: e) =
      let recur = mkAM (n:qs) e
      in  M.insert (n:qs) (mkLabelEntry (n:qs) e recur) recur
    mkAM qs (Exists s e) = mkAM qs e
    mkAM qs (s :# e) = mkAM qs e
    mkAM qs (FLPS.Attr a e) = mkAM qs e

  in M.unions $ map (mkAM []) bes

-- For now everything needs to have an explicit name, but once the codegen is working
-- smoothly we should relax this restriction and reference a library of primitive
-- transitions and types in the generated C code for specs which don't wish to give
-- an explicit name to everything (such as for padding, though it seems like good
-- memory layout hygiene to specify these names anyways).
getSeqs :: BaseExp -> [ (Maybe NameID, BaseExp) ]
getSeqs be@(Prim _) = [(Nothing, be)] -- error $ "Unimplemented getSeqs: " ++ show be
getSeqs (Con _ be) = getSeqs be
getSeqs (be :@ _) = getSeqs be
getSeqs (e1 :+ e2) = getSeqs e1 ++ getSeqs e2
-- TODO: this case implies more than one sequence, meaning the return type might need to be adjusted...:
getSeqs be@(e1 :|| e2) = [(Nothing, be)]
getSeqs (n ::: be) = [ (Just n, be) ]
getSeqs (Exists _ be) = getSeqs be
getSeqs be@(n :# be') = [(Nothing, be)] -- error $ "Unimplemented getSeqs: " ++ show be
getSeqs be@(FLPS.Attr _ be') = [(Nothing, be)]
--getSeqs be@(FLPS.Attr (BaseType (EnumBT _)) be') = [(Nothing, be)] -- error $ "Unimplemented getSeqs: " ++ show be
--getSeqs be@(FLPS.Attr (BaseType (BitsBT _)) be') = [(Nothing, be)] -- error $ "Unimplemented getSeqs: " ++ show be
--getSeqs be@(FLPS.Attr (BaseType (PtrBT _)) be')  = [(Nothing, be)] -- error $ "Unimplemented getSeqs: " ++ show be
--getSeqs be@(FLPS.Attr (BaseType (SizeBT _)) be') = [(Nothing, be)] -- error $ "Unimplemented getSeqs: " ++ show be
