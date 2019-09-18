{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable, BangPatterns, UnicodeSyntax #-}
module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
--import Test.Framework.Providers.QuickCheck2
--import Test.QuickCheck (Property, quickCheck, (==>))

import Language.Floorplan.Syntax
import Language.Floorplan.Token
import qualified Language.Floorplan.Parser as P
import System.IO (openFile, IOMode(..), hGetContents)
import qualified Debug.Trace as D
import Data.List (sort)

import Language.Rust.Pretty (pretty)
import Language.Floorplan.Core.Compiler
import qualified Language.Floorplan.Rust as R

{-
testTokenizes fn = testCase fn $ do
  fd  <- openFile fn ReadMode
  contents <- hGetContents fd
  let !xs = show $ P.tokenize contents
  return ()

testParses fn = testCase fn $ do
  fd  <- openFile fn ReadMode
  contents <- hGetContents fd
  let !xs = P.parseFloorplanDemarcsGLR fn 0 0 contents
  return ()

testParsesExp fn expected = testCase fn $ do
  fd  <- openFile fn ReadMode
  contents <- hGetContents fd
  let xs = P.parseFloorplanDemarcsGLR fn 0 0 contents
  assertEqual "Not equal" xs expected
-}

-- Happy parser generator versions:

readme fn = do
  fd <- openFile fn ReadMode
  hGetContents fd

mkTest fn cont = testCase fn $ readme fn >>= cont

testTokenizes fn = mkTest fn $ \contents -> do
  let !xs = show $ scanTokens contents
  return ()

testParses fn = mkTest fn $ \contents -> do
  let !xs = P.parseLayers contents
  return ()

testGrafts fn = mkTest fn $ \contents -> do
  let !xs = P.parseLayers contents
  let !ys = map (grafting xs) xs
  countGrafting ys @?= 0
  return ()

testParsesExp fn expected = mkTest fn $ \contents ->
  expected @=? P.parseLayers contents

test_b2i =
  assertEqual "incorrect binary value"
    (bin2int "0b1010")
    10

test_b2i' =
  assertEqual "incorrect binary value"
    (bin2int "0b000000")
    0

tokenizeSuite = testGroup "tokenization suite"
  [ testTokenizes "examples/empty.flp"
  ]

parseSuite = testGroup "parsing suite"
  [ testParses "examples/immix/layout.flp"
  , testParses "examples/empty.flp"
  , testParses "examples/seq.flp"
  , testParses "examples/enum.flp"
  , testParses "examples/union.flp"
  , testParses "examples/layer.flp"
  , testParses "examples/bits.flp"
  , testParses "examples/arith_id.flp"
  , testParses "examples/arith_power.flp"
  , testParses "examples/bump.flp"
  , testParses "examples/dynamic_prim.flp"
  , testParses "examples/dyn_choice.flp"
  , testParses "examples/dyn_seq.flp"
--  , testNotParses "examples/enum_bad0.flp"
  , testParses "examples/map_bits.flp"
  , testParses "examples/named_ref.flp"
  , testParses "examples/nested.flp"
  , testParses "examples/nested_union.flp"
  , testParses "examples/app.flp"
  , testParsesExp "examples/arith.flp" theLayer
  ]

theLayer =
    [ Layer
      { name      = "Arith"
      , formals   = []
      , magnitude = Just (SizeLit (Just (Lit 7)) Byte)
      , alignment = Nothing
      , magAlign  = Nothing
      , contains  = []
      , rhs = Blob  (SizeLit
                      (Just (Plus
                              (Times  (Div (Lit 4) (Lit 2))
                                      (Lit 3))
                              (Lit 1)))
                      Byte
                    )
      }
    ]

graftSuite = testGroup "grafting suite"
  [ testGrafts "examples/immix/layout.flp"
  , testCase "empty unique" $ [] @=? (checkUniqNames theLayer)
  , mkTest "examples/uniq_fail.flp" $ \c -> (sort ["foo", "Outer"]) @=? (sort $ checkUniqNames (P.parseLayers c))
  ]

testCompiles fn = mkTest fn $ \contents -> do
  let !xs = P.parseLayers contents
  let !ys = map (grafting xs) xs
  countGrafting ys @?= 0
  let !zs = D.traceShowId $ map compile ys
  let !rust = D.traceShowId $ R.genRust zs
  let !_ = D.traceShowId $ pretty rust
  return ()

compileSuite = testGroup "compiling suite"
  [ testCase "enumBytes A" $ enumBytes (take 0   $ repeat "") @?= 0 -- Null enum requires no space.
  , testCase "enumBytes B" $ enumBytes (take 1   $ repeat "") @?= 1 -- Singletone requires space.
  , testCase "enumBytes C" $ enumBytes (take 128 $ repeat "") @?= 1 -- Most enums take 1 bytes.
  , testCase "enumBytes D" $ enumBytes (take 256 $ repeat "") @?= 1 -- Including this one.
  , testCase "enumBytes E" $ enumBytes (take 257 $ repeat "") @?= 2 -- But not this one.
  , testCase "enumBytes F" $ enumBytes (take (256*256) $ repeat "") @?= 2 -- Not this one either.
  , testCase "enumBytes G" $ enumBytes (take (256*256 + 1) $ repeat "") @?= 3 -- And definitely not this one.
  , testCase "enumBytes H" $ enumBytes (take (256*256*256 + 1) $ repeat "") @?= 4 -- And so on.
  , testCompiles "examples/immix/layout.flp"
  , testCase "delta_bytes round small" $ delta_bytes (SizeLit (Just $ Lit $ 8*3-1) Bit) @?= 3
  , testCase "delta_bytes no rounding" $ delta_bytes (SizeLit (Just $ Lit $ 8*3  ) Bit) @?= 3
  , testCase "delta_bytes round large" $ delta_bytes (SizeLit (Just $ Lit $ 8*3+1) Bit) @?= 4
  ]

main :: IO ()
main = defaultMainWithOpts
  [ testCase "Binary conversion" test_b2i
  , testCase "Binary conversion" test_b2i'
  , tokenizeSuite
  , parseSuite
  , graftSuite
  , compileSuite
  ] mempty

