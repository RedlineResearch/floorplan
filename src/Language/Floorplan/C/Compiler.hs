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

import qualified Language.C.Pretty as PrettyC
import Text.PrettyPrint.Mainland (prettyLazyText)
import Text.PrettyPrint.Mainland.Class (ppr)

import Language.Floorplan.Core.Syntax

import Data.Bifunctor ( bimap )
import Data.Functor ( ($>) )
import Data.Ord (comparing)
import Data.List (sortBy, nub, inits, intersperse)
import Data.Char (toUpper, toLower)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Bits
import qualified Debug.Trace as D

import Language.C.Quote.C
import qualified Language.C.Syntax as S
import Data.Loc -- (SrcLoc(..), noLoc)

import Text.RE.TDFA.String (RE(..), (?=~), matched)

-- TODO: Introduce spans based on position in these Haskell files.
fakeLoc = noLoc --Span (Position 0 0 0) (Position 0 0 0)
fL = fakeLoc

justName :: [([NameID], BaseExp)] -> BaseExp -> Maybe ([NameID], BaseExp)
justName [] (n ::: e) = Just ([n], e)
justName ((ns,_):_) (n ::: e) = Just (n : ns, e)
justName _ _ = Nothing

justNameExp :: BaseExp -> Maybe (NameID, BaseExp)
justNameExp (n ::: e) = Just (n, e)
justNameExp _ = Nothing

-- | Get all of the unique name identifiers in the spec and their
--   corresponding subexpression.
findNames :: [BaseExp] -> [([NameID], BaseExp)]
findNames = nub . concatMap (accumTopDown justName)

mkInitConst s = (Nothing, ExpInitializer (Const s fL) fL)

mkStrConst t = S.StringConst ['"' : t ++ "\""] t fakeLoc

mkPoundDefs' :: Int -> [NameID] ->  [Definition]
mkPoundDefs' i (n:ns) = EscDef ("#define " ++ n ++ " ((unsigned int)" ++ show i ++ ")") fL : (mkPoundDefs' (i+1) ns)
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

mkAllPrefixes :: [BaseExp] -> [RE] -> [S.Definition]
mkAllPrefixes bes res = let

    ns = findNames bes
    uniques = nub $ concatMap fst ns

    -- Tuples of the constructed type identifier and the list of names from which it was constructed.
    types0 :: [String]
    types0 = map mkTypeIdentifier ns

    findFor :: String -> [(([NameID], BaseExp), Maybe String)] -> [String]
    findFor u []      = []
    findFor u ((tuple@(n_qual, _), Just _):ns0) -- Not filtered out - make and keep the identifier if it involves the type 'u'.
      | u `elem` n_qual = mkTypeIdentifier tuple : findFor u ns0
      | otherwise       = findFor u ns0
    findFor u (((_, _), Nothing):ns0) = findFor u ns0 -- This particular name is filtered out, as indicated by the Nothing

    mkAll :: (String, [String]) -> S.Definition
    mkAll (unique_ty, paths)
      = EscDef ("#define FLP_ALL_" ++ unique_ty ++ " " ++ (concat . intersperse ", " . map ("FLP_" ++) $ paths)) fL

  in map mkAll (zip uniques (map (\f -> f $ zip ns (doFilterOut types0 res)) (map findFor uniques)))

-- | (["A", "B", "C"], _) becomes "C_B_A"
mkTypeIdentifier :: ([NameID], BaseExp) -> String
mkTypeIdentifier = concat . intersperse "_" . reverse . fst

genC :: [BaseExp] -> [RE] -> [S.Definition]
genC bes res = let
    types0 = map mkTypeIdentifier (findNames bes)
    types = "FLP_UNMAPPED" : (map ("FLP_" ++) $ catMaybes $ doFilterOut types0 res)
    num_types = [EscDef ("#define __FLP_NUM_VALID_TYPES ((unsigned int)" ++ show (length types) ++ ")") fL]
    xs    = map (mkInitConst . mkStrConst) types
    pound_defs = mkPoundDefs types
    initializer = CompoundInitializer xs fL
    all_prefixes = mkAllPrefixes bes res
  in  pound_defs ++ num_types ++ [cunit|
        static const char* const __FLP_TYPES[] = $init:initializer;
      |] ++ all_prefixes
      --"PC_UNMAPPED", "PC_SOMETHING_ELSE" };

-- TODO: work with Text instead of String

-- writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
-- | The pair of strings are the header and footer and to append
writeCFile :: FilePath -> (String, String) -> [S.Definition] -> IO ()
writeCFile outdir (header, footer) c_code = do
  fd <- openFile outdir WriteMode
  hPutStr fd $ pack header
  hPutStr fd (prettyLazyText 160 $ ppr c_code) -- 160-character wide terminal viewing width of source files (it's the 2020 people)
  hPutStr fd $ pack footer
  hClose fd

