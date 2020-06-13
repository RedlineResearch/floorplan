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

doAnalysis :: [Demarc] -> IO ()
doAnalysis result =
  case CC.graftingAnalysis result of
    [] -> return ()
    xs -> P.putStrLn (show xs) >> exitFailure

doResult :: CompilerOutput -> FilePath -> [Demarc] -> IO ()
doResult RustOutput outputFile result = do
  --print $ pretty' sf
  doAnalysis result
  P.putStrLn ("No grafting errors. Proceeding to graft.")
  grafted  :: [S.Demarc]      <- return $ map (CC.grafting result) result
  core_flp :: [CS.BaseExp]    <- return $ map CC.compile grafted
  sf       :: SourceFile Span <- return $ RC.genRust core_flp
  assert ((CC.countGrafting grafted) == 0) (return ())
  P.putStrLn ("Grafting completed.")
  (RC.writeModule outputFile) sf
  exitSuccess
doResult COutput outputFile result = do
  doAnalysis result
  P.putStrLn ("No grafting errors. Proceeding to graft.")
  grafted  :: [S.Demarc]      <- return $ map (CC.grafting result) result
  core_flp :: [CS.BaseExp]    <- return $ map CC.compile grafted
  sf                          <- return $ CComp.genC core_flp
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
      do  P.putStrLn $ "Loading FLP file from " ++ flpFile ++ "..."
          contents <- readFile flpFile
          P.putStrLn $ "Parsing layers..."
          let r = P.parseLayers contents
          let cnt = sum $ map countDemarcNodes r
          P.putStrLn $ "Parsed contents: " ++ show r
          P.putStrLn ("Surface-syntax AST nodes: " ++ show cnt)
          doResult (checkSuffix outputFile) outputFile r
    _ -> usage


