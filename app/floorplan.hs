{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Prelude hiding (putStrLn)
import qualified Prelude as P
import System.Environment
import System.IO hiding (putStr, putStrLn, hGetLine)
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (assert)

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

usage = do
  pName <- getProgName
  P.putStrLn $ pName ++ " [file.flp] [path/to/dest.rs]"

doResult :: String -> FilePath -> [Demarc] -> IO ()
doResult compiler outDir result = do
  let grafted  :: [S.Demarc]      = map (CC.grafting result) result
      core_flp :: [CS.BaseExp]    = map CC.compile grafted
      sf       :: SourceFile Span = RC.genRust core_flp
  --print $ pretty' sf
  assert ((CC.countGrafting grafted) == 0) (return ())
  case compiler of
    "rust"  -> (RC.writeModule outDir) sf
    _       -> usage
  exitSuccess

oops s = P.putStr s >> exitFailure

main = do
  args <- getArgs
  case args of
    (flpFile : outDir : rst) ->
      do  contents <- readFile flpFile
          let r = P.parseLayers contents
          doResult "rust" outDir r
    _ -> usage


