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
import Data.List (sortBy, nub, inits)
import Data.Char (toUpper, toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.Bits
import qualified Debug.Trace as D

genC :: [BaseExp] -> [InitGroup]
genC bes = [] -- TODO

-- writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
writeCFile :: FilePath -> [InitGroup] -> IO ()
writeCFile outdir c_code = do
  fd <- openFile outdir WriteMode
  hPutStr fd (prettyLazyText 160 $ ppr c_code) -- 160-character wide terminal viewing width of source files (it's the 2020 people)
  hClose fd

