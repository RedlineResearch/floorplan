{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable #-}
module Language.Floorplan.Semantics where

import Data.Maybe (fromJust)
import Language.Floorplan.Core.Syntax

data Tree =
    T Tree Tree
  | N String Tree
  | B0 | B1
  deriving (Eq, Ord, Show)

leaves (T t1 t2) = leaves t1 + leaves t2
leaves (N _ t) = leaves t
leaves B0 = 0
leaves B1 = 1

addStep s xs = map (\(r,d) -> (r, s : d)) xs

g p0 p1 p2 p3 = let

    g' a m theta ((:+) e1 e2) =
      let params = \r1 -> (a + leaves r1, m - leaves r1, theta, e2)
      in
        [ (T r1 r2, ("Plus: i = 0.." ++ show m ++ ", Pause (g " ++ show (params r1) ++ ")") : deriv1
            ++ ["Resume (g " ++ show (params r1) ++ ")" ++ ", leaves r1 = " ++ show (leaves r1)] ++ deriv2)
        | (r1, deriv1) <- concat [ addStep ("Plus_r1: Choose i=" ++ show i) $ g a i theta e1 | i <- [0..m]]
        , (r2, deriv2) <- addStep ("Plus_r2: leaves(" ++ show e1 ++ ")=" ++ show (leaves r1)) $ g (a + leaves r1) (m - leaves r1) theta e2
        ]
    g' a m theta ((:||) e1 e2) = let
        r1 = addStep ("Or_e1") (g a m theta e1)
        r2 = addStep ("Or_e2") (g a m theta e2)
      in r1 ++ r2
    g' a m theta (Prim n)
      | m == n    = [(foldl T B0 (take n $ repeat B1), ["Prim: Evaluate " ++ show n ++ "-leaf tree"])]
      | otherwise = []
    g' a m theta (Con n e)
      | m == n    = addStep ("Con: " ++ show n) (g a m theta e)
      | m /= n    = []
    g' a m theta ((:@) e aln)
      | a `mod` aln == 0 = g a m theta e
      | otherwise = []
    g' a m theta ((:::) l e) =
      [ (N l r, ("HasType: " ++ l) : deriv)
      | (r, deriv) <- g a m theta e
      ]
    g' a m theta (Exists f e) = concat
      [ addStep ("Exists: Choose " ++ f ++ "=" ++ show i)
          $ g a m ((f, i) : theta) e | i <- [0 .. m] ]
    g' a m theta ((:#) f e)
      | (Just m) == lookup f theta && m == 0 = [(T B0 B0, ["Pound: m = 0"])]
      | (Just 0) == lookup f theta = []
      | otherwise = let -- Implies lookup theta f > 0
          params = \r1 -> (a + leaves r1, m - leaves r1, (f, (fromJust $ lookup f theta) - 1) : theta, (:#) f e)
          in case lookup f theta of
                Nothing -> error "Nothing"
                Just thetaF -> [ (T r1 r2, ("Pound: theta(" ++ f ++ ")=" ++ show thetaF ++ ", Pause (g " ++ show (params r1)) : deriv1
                                    ++ ["Resume (g " ++ show (params r1) ++ ")"] ++ deriv2)
                               | (r1, deriv1) <- concat [addStep ("Pound_r1: Choose i=" ++ show i) $ g a i theta e | i <- [0..m]]
                               , (r2, deriv2) <- addStep ("Pound_r2: leaves(" ++ show e ++ ")=" ++ show (leaves r1)) $
                                                  g (a + leaves r1) (m - leaves r1) ((f, (fromJust $ lookup f theta) - 1) : theta) ((:#) f e)
                               ]

    addLabel (r, d:ds) = (r, (d ++ "[[g " ++ show (p0, p1, p2, p3) ++ "]] ") : ds)

  in map addLabel (g' p0 p1 p2 p3)

