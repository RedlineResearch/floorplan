{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.C.Compiler
  ( genC, writeCFile
  ) where

import System.IO (IOMode(..), openFile, hClose)
import Data.Typeable (Typeable(..))
import Data.Text.Lazy.IO (hPutStr)

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
import Data.Maybe (isJust, fromMaybe)
import Data.Bits
import qualified Debug.Trace as D

import Language.C.Quote.C
import qualified Language.C.Syntax as S
import Data.Loc -- (SrcLoc(..), noLoc)

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

genC :: [BaseExp] -> [S.Definition]
genC bes = let
    types = "FLP_UNMAPPED" : map (("FLP_" ++) . concat . intersperse "_" . reverse . map (map id {- toUpper -}) . fst) (findNames bes)
    xs    = map (mkInitConst . mkStrConst) types
    pound_defs = mkPoundDefs types
    initializer = CompoundInitializer xs fL
  in  pound_defs ++ [cunit|
        static const char* const __FLP_TYPES[] = $init:initializer;
      |]
      --"PC_UNMAPPED", "PC_SOMETHING_ELSE" };

-- writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
writeCFile :: FilePath -> [S.Definition] -> IO ()
writeCFile outdir c_code = do
  fd <- openFile outdir WriteMode
  hPutStr fd (prettyLazyText 160 $ ppr c_code) -- 160-character wide terminal viewing width of source files (it's the 2020 people)
  hClose fd

