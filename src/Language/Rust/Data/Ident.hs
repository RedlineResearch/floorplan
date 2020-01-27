{-|
Module      : Language.Rust.Data.Ident
Description : Identifiers
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Data structure behind identifiers.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Rust.Data.Ident (Ident(..), mkIdent, Name, IdentName(..) ) where

import GHC.Generics       ( Generic )

import Control.DeepSeq    ( NFData )
import Data.Data          ( Data )
import Data.Typeable      ( Typeable )

import Data.List          ( foldl' )
import Data.Char          ( ord )
import Data.String        ( IsString(..) )
--import Data.Semigroup as Sem

-- | An identifier
data Ident
  = Ident { name :: IdentName            -- ^ payload of the identifier
          , raw :: Bool                  -- ^ whether the identifier is raw
          , hash :: {-# UNPACK #-} !Int  -- ^ hash for quick comparision
          } deriving (Data, Typeable, Generic, NFData)

-- | Shows the identifier as a string (for use with @-XOverloadedStrings@)
instance Show Ident where
  show = show . name

instance IsString Ident where
  fromString = mkIdent

-- | Uses 'hash' to short-circuit
instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2 && raw i1 == raw i2
  i1 /= i2 = hash i1 /= hash i2 || name i1 /= name i2 || raw i1 /= raw i2

-- | Uses 'hash' to short-circuit
instance Ord Ident where
  compare i1 i2 = case compare i1 i2 of
                    EQ -> compare (raw i1, name i1) (raw i2, name i2)
                    rt -> rt

-- | "Forgets" about whether either argument was raw
instance Monoid Ident where
  mappend = (<>)
  mempty = mkIdent ""

-- | "Forgets" about whether either argument was raw
-- <<<<<<< HEAD
instance Semigroup Ident where
  Ident (Name n1) _ _ <> Ident (Name n2) _  _               = mkIdent (n1 <> n2)
  Ident (HaskellName n1) _ _ <> Ident (HaskellName n2) _  _ = mkIdent (n1 <> n2)
  Ident (Name n1) _ _ <> Ident (HaskellName n2) _  _        = mkIdent (n1 <> n2)
  Ident (HaskellName n1) _ _ <> Ident (Name n2) _  _        = mkIdent (n1 <> n2)
-- =======
-- instance Sem.Semigroup Ident where
--   Ident n1 _ _ <> Ident n2 _  _ = mkIdent (n1 <> n2)
-- >>>>>>> upstream/master

-- | Smart constructor for making an 'Ident'.
mkIdent :: String -> Ident
mkIdent ('$':'{':'i':'|':ss) = Ident (HaskellName $ init $ init ss) False (hashString $ init $ init ss)
mkIdent s = Ident (Name s) False (hashString s)

-- | Hash a string into an 'Int'
hashString :: String -> Int
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + m
         magic = 0xdeadbeef
         golden = 1013904242

-- | The payload of an identifier
type Name = String

data IdentName =
    Name        Name
  | HaskellName String
  deriving (Eq, Ord, Data, Typeable, Generic, NFData)

instance Show IdentName where
  show (Name n) = show n
  show (HaskellName n) = "${i|" ++ n ++ "|}"

