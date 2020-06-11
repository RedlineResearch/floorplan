{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Prelude hiding (putStrLn)
import qualified Prelude as P
import System.Environment
import System.IO hiding (putStr, putStrLn, hGetLine)
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (assert)
import Data.List (isSuffixOf)

import Language.Rust.Syntax (SourceFile(..))
import Language.Rust.Data.Position (Span(..))
import Language.Rust.Pretty (pretty')

import Language.Floorplan
import qualified Language.Floorplan.Rust.Compiler as RC

--import qualified Language.Floorplan.Parser as P
import qualified Language.Floorplan.Parser as P
import qualified Language.Floorplan.Syntax as S
import qualified Language.Floorplan.Core.Compiler as CC
import qualified Language.Floorplan.Core.Syntax as CS

import qualified Language.Floorplan.C.Compiler as CComp

usage = do
  pName <- getProgName
  P.putStrLn $ pName ++ " [file.flp] [path/to/dest.{rs,c}]"
  exitFailure

data CompilerOutput = RustOutput | COutput | Unknown

doResult :: CompilerOutput -> FilePath -> [Demarc] -> IO ()
doResult RustOutput outputFile result = do
  let grafted  :: [S.Demarc]      = map (CC.grafting result) result
      core_flp :: [CS.BaseExp]    = map CC.compile grafted
      sf       :: SourceFile Span = RC.genRust core_flp
  --print $ pretty' sf
  assert ((CC.countGrafting grafted) == 0) (return ())
  (RC.writeModule outputFile) sf
  exitSuccess
doResult COutput outputFile result = do
  let grafted  :: [S.Demarc]      = map (CC.grafting result) result
      core_flp :: [CS.BaseExp]    = map CC.compile grafted
      sf = CComp.genC core_flp
  assert ((CC.countGrafting grafted) == 0) (return ())
  (CComp.writeCFile outputFile) sf
  exitSuccess
doResult _ outputFile _ = P.putStrLn ("Error: File type unknown '" ++ outputFile ++ "'") >> usage

oops s = P.putStr s >> exitFailure

checkSuffix f
  | ".rs" `isSuffixOf` f = RustOutput
  | ".c"  `isSuffixOf` f = COutput
  | otherwise = Unknown

main = do
  args <- getArgs
  case args of
    (flpFile : outputFile : rst) ->
      do  contents <- readFile flpFile
          let r = P.parseLayers contents
          doResult (checkSuffix outputFile) outputFile r
    _ -> usage


