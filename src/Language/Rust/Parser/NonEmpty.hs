{-|
Module      : Language.Rust.Parser.NonEmpty
Description : Non-empty lists
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Datatype wrapping non-empty lists.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ < 800
{-# LANGUAGE FlexibleContexts #-}
#endif
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveFoldable, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveTraversable #-}

module Language.Rust.Parser.NonEmpty (
  NonEmpty(..),
  listCons,
  head, tail,
  reverse, last, init, (<|), (|:), (|>)
) where
import Prelude hiding (head, tail, reverse, init, last)

import qualified Data.List.NonEmpty as N
import Data.Data ( Data(..) )
import Control.DeepSeq    ( NFData )
import Data.Semigroup (Semigroup(..))

-- Support for OverloadedLists of NonEmpty lists:
import GHC.Exts (IsList(..))

newtype NonEmpty a = NonEmpty (N.NonEmpty a)
  deriving (Foldable, Eq, Ord, Show, Functor, Data, NFData, Traversable)

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a

  fromList (a:as) = NonEmpty $ a N.:| as
  fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"

  toList ~(NonEmpty (a N.:| as)) = a : as

instance Semigroup (NonEmpty a) where
  (NonEmpty (a N.:| as)) <> ~(NonEmpty (b N.:| bs)) = NonEmpty $ a N.:| (as ++ b : bs)

listCons :: a -> [a] -> NonEmpty a
listCons a as = NonEmpty $ a N.:| as

head :: NonEmpty a -> a
head (NonEmpty as) = N.head as

tail :: NonEmpty a -> [a]
tail (NonEmpty as) = N.tail as

reverse :: NonEmpty a -> NonEmpty a
reverse (NonEmpty as) = NonEmpty $ N.reverse as

init :: NonEmpty a -> [a]
init (NonEmpty as) = N.init as

last :: NonEmpty a -> a
last (NonEmpty as) = N.last as

(<|) :: a -> NonEmpty a -> NonEmpty a
(<|) a (NonEmpty as) = NonEmpty $ a N.<| as

(|:) :: [a] -> a -> NonEmpty a
[]      |: y = NonEmpty $ y N.:| []
(x:xs)  |: y = NonEmpty $ x N.:| (xs ++ [y])

(|>) :: NonEmpty a -> a -> NonEmpty a
(|>) (NonEmpty (x N.:| xs)) y = NonEmpty $ x N.:| (xs ++ [y])

