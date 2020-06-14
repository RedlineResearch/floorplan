{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Prelude hiding (putStrLn)
import qualified Prelude as P
import System.Environment
import System.IO hiding (putStr, putStrLn, hGetLine)
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (assert)
import Data.List (isSuffixOf)
import Data.Either (partitionEithers)
import Text.RE.TDFA.String (RE(..), compileRegex)

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
import qualified Language.Floorplan.Preproc.Passes as Preproc

usage = do
  pName <- getProgName
  P.putStrLn $ pName ++ " [file.flp] [path/to/dest.{rs,c}]"
  exitFailure

data CompilerOutput = RustOutput | COutput | Unknown

validateAnalysis [] = return ()
validateAnalysis xs = P.putStrLn (show xs) >> exitFailure

-- | Returns a tuple (for now just the [RE]) of the results of analyses with computed results.
doAnalyses :: [Decl] -> IO [RE]
doAnalyses result = do
  (errors, regexes) <- partitionEithers <$> Preproc.regexAnalysis result
  validateAnalysis     errors
  validateAnalysis  $  Preproc.balancedScopeAnalysis result
  validateAnalysis  $  Preproc.graftingAnalysis result
  return regexes

--print $ pretty' sf

doResult :: CompilerOutput -> FilePath -> [Decl] -> IO ()
doResult Unknown outputFile _ = P.putStrLn ("Error: File type unknown '" ++ outputFile ++ "'") >> usage
doResult out_type outputFile result = do
  filterOutRegexes <- doAnalyses result
  P.putStrLn ("No grafting errors. Proceeding to graft.")
  let layers   :: [S.Demarc]      = Preproc.removeNoGlobalPass result
      grafted  :: [S.Demarc]      = map (Preproc.grafting $ onlyLayers result) layers
      core_flp :: [CS.BaseExp]    = map CC.compile grafted
  assert (CC.countGrafting grafted == 0) (return ())
  P.putStrLn ("Grafting completed.")
  case out_type of
    RustOutput -> do  sf :: SourceFile Span <- return $ RC.genRust core_flp
                      (RC.writeModule outputFile) sf
    COutput    -> do  sf <- return $ CComp.genC core_flp
                      (CComp.writeCFile outputFile) sf
  exitSuccess

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
          P.putStrLn $ "Parsing top-level declarations..."
          let result = P.parseTopLevelDecls contents
          let cnt = sum $ map countDeclNodes result
          P.putStrLn $ "Parsed contents: " ++ show result
          P.putStrLn ("Surface-syntax AST nodes: " ++ show cnt)
          doResult (checkSuffix outputFile) outputFile result
    _ -> usage


