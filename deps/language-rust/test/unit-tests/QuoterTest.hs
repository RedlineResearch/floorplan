{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module QuoterTest (quoterSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Quote
import Language.Rust.Syntax
import Language.Rust.Data.Position (Span(..), Position(..))
import Language.Rust.Parser (parse', inputStreamFromString)
import Language.Rust.Pretty (ResolveFail(..), pretty)
import Language.Rust.Data.Ident ( mkIdent )

import Data.Functor ( ($>) )

makeInt :: Int -> Expr Span
makeInt s = parse' (inputStreamFromString $ show s)

makeFoo :: Int -> String
makeFoo i = "foo_" ++ show i

rustStmt :: Int -> Stmt Span
rustStmt i = [stmt| let ${i| makeFoo i |} = ${e| makeInt i |}; |]

rustItem :: Int -> Item Span
rustItem i = let iN = "bar_" ++ show i in [item| deriveMe!($iN); |]

rustExpr :: Int -> Expr Span
rustExpr i =
  let k = "k_" ++ show i
      j = "j_" ++ show i
  in  [expr| $k + $j |]

mkExpr :: Int -> Expr () -> Test
mkExpr i expect = testCase
  (case pretty expect of
    Left (ResolveFail _ msg) -> error msg
    Right expect' -> show expect')
  $ expect @=? (rustExpr i $> ())

mkItem :: Int -> Item () -> Test
mkItem i expect = testCase
  (case pretty expect of
    Left (ResolveFail _ msg) -> error msg
    Right expect' -> show expect')
  $ expect @=? (rustItem i $> ())

mkMain :: Int -> Stmt () -> Test
mkMain i expect = testCase
  (case pretty expect of
    Left (ResolveFail _ msg) -> error msg
    Right expect' -> show expect')
  $ expect @=? (rustStmt i $> ())

quoterSuite :: Test
quoterSuite = testGroup "quoter suite"
  [ mkMain 5 $ Local (IdentP (ByValue Immutable) "foo_5" Nothing ()) Nothing (Just (Lit [] (Int Dec 5 Unsuffixed ()) ())) [] ()
  , mkExpr 5 $ Binary [] AddOp
                (PathExpr [] Nothing (Path False [PathSegment "k_5" Nothing ()] ()) ())
                (PathExpr [] Nothing (Path False [PathSegment "j_5" Nothing ()] ()) ()) ()
  , mkItem 9 $ MacItem [] Nothing 
                (Mac  (Path False [PathSegment "deriveMe" Nothing ()] ())
                      (Tree (Token (Span (Position 11 27 60) (Position 14 27 63)) (IdentTok $ mkIdent "bar_9")))
                ()) ()
  ]

