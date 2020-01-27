{-|
Module      : Language.Rust.Parser.Reversed
Description : Parsing literals
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Datatypes wrapping lists and non-empty lists designed for fast append (as opposed to prepend) 
along with the usual class instances.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ < 800
{-# LANGUAGE FlexibleContexts #-}
#endif
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Rust.Parser.Reversed (
  Reversed(..),
  toNonEmpty,
  unsnoc,
  snoc,
  NonEmpty(..),
) where

import Language.Rust.Data.Position

import Data.Foldable          ( Foldable(toList) )
import Data.Semigroup as Sem  ( Semigroup(..) )

import qualified Language.Rust.Parser.NonEmpty as N
import Language.Rust.Parser.NonEmpty (NonEmpty(..), listCons)
import qualified GHC.Exts as G

import Data.Data (Data(..), Typeable)

-- | Wrap a data type where all the operations are reversed
newtype Reversed f a = Reversed (f a)

instance Functor f => Functor (Reversed f) where
  fmap f (Reversed xs) = Reversed (fmap f xs)

instance Foldable (Reversed []) where
  foldMap f (Reversed xs) = foldMap f (reverse xs)
  toList (Reversed xs) = reverse xs

instance Foldable (Reversed NonEmpty) where
  foldMap f (Reversed xs) = foldMap f (N.reverse xs)
  toList (Reversed xs) = reverse (toList xs)

instance Sem.Semigroup (f a) => Sem.Semigroup (Reversed f a) where
  Reversed xs <> Reversed ys = Reversed (ys Sem.<> xs)

instance Monoid (f a) => Monoid (Reversed f a) where
  mempty = Reversed mempty
  mappend (Reversed xs) (Reversed ys) = Reversed (mappend ys xs)

instance G.IsList (f a) => G.IsList (Reversed f a) where
  type Item (Reversed f a) = G.Item (f a)
  fromList xs = Reversed (G.fromList (reverse xs))
  toList (Reversed xs) = reverse (G.toList xs)

instance Located (f a) => Located (Reversed f a) where
  spanOf (Reversed xs) = spanOf xs

deriving instance (Typeable f, Typeable a, Data (f a)) => Data (Reversed f a)

-- | Convert a reversed 'NonEmpty' back into a normal one.
{-# INLINE toNonEmpty #-}
toNonEmpty :: Reversed NonEmpty a -> NonEmpty a
toNonEmpty (Reversed xs) = N.reverse xs

-- | Remove an element from the end of a non-empty reversed sequence
{-# INLINE unsnoc #-}
unsnoc :: Reversed NonEmpty a -> (Reversed [] a, a)
unsnoc (Reversed xs) = (Reversed $ N.tail xs, N.head xs)

-- | Add an element to the end of a reversed sequence to produce a non-empty
-- reversed sequence
{-# INLINE snoc #-}
snoc :: Reversed [] a -> a -> Reversed NonEmpty a
snoc (Reversed xs) x = Reversed (x `listCons` xs)
