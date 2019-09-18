module Language.Floorplan.Rust.Common
  where
{- Functions and names applicable to all code gen phases / passes
 -}

import Language.Rust.Parser as P
import Language.Rust.Syntax as R
import Language.Rust.Quote
import Language.Rust.Data.Ident
import Language.Rust.Data.Position

import Language.Floorplan.Core.Syntax

import Data.Functor ( ($>) )
import Data.Ord (comparing)
import Data.List (sortBy, nub)
import Data.Char (toUpper, toLower)

import Language.Floorplan.Rust.Types
import qualified Debug.Trace as D

bytesInWord :: Int
bytesInWord = 8

bitsInByte :: Int
bitsInByte = 8

isPow2 k = elem k $ map (2 ^) [0..k]

-- | Log base `i` of `n` rounded up.
log' :: Int -> Int -> Int
log' i n = length (takeWhile (< n) (iterate (* 2) 1))

log2 :: Int -> Int
log2 = log' 2

-- TODO: Introduce spans based on position in these Haskell files.
fakeSpan = Span (Position 0 0 0) (Position 0 0 0)
fS = fakeSpan

-- TODO: put Language.Rust.* accessors in their own module:
-- | Makes a Rust expression of from an integer
mkIntExp :: Int -> R.Expr Span
mkIntExp i = P.parse' $ P.inputStreamFromString $ show i

mkBin' i
  | i == 0 = []
  | i `mod` 2 == 0 = '0' : mkBin' (i `div` 2)
  | i `mod` 2 == 1 = '1' : mkBin' (i `div` 2)

mkBinU8 i
  | i >= 256 = error $ "Cannot make u8 from i=" ++ show i
  | otherwise = take (8 - (length $ mkBin' i)) (repeat '0') ++ (reverse $ mkBin' i)

mkBinExp :: Int -> R.Expr Span
mkBinExp i = P.parse' $ P.inputStreamFromString $ "0b" ++ mkBinU8 i

offsetName :: NameID -> String
offsetName n
  = allUpper n ++ "_OFFSET_BYTES"

containsFnName :: NameID -> String
containsFnName ns
  = "contains_" ++ firstLower ns

firstLower [] = error "empty NameID!"
firstLower (n:ns) = toLower n : ns

firstUpper [] = error "empty NameID!"
firstUpper (n:ns) = toUpper n : ns

allUpper = map toUpper

initSeqName :: [NameID] -> String
initSeqName _ = "init_canonical_sequence"

bumpAllocName :: NameID -> String
bumpAllocName n
  = "bump_new_" ++ n

bumpValidName :: NameID -> NameID -> String
bumpValidName n1 n2
  = firstLower n1 ++ "_is_validly_before_" ++ firstLower n2

initAfterName :: NameID -> NameID -> String
initAfterName n1 n2
  = "init_" ++ firstLower n2 ++ "_after_" ++ firstLower n1

memsetUntilName :: NameID -> NameID -> String
memsetUntilName n1 n2
  = "memset_" ++ firstLower n1 ++ "_until_" ++ firstLower n2

castFromName :: NameID -> NameID -> String
castFromName from to
  = "cast_" ++ firstLower from ++ "_to_" ++ firstLower to

fromIdxName :: NameID -> String
fromIdxName from
  = firstLower from ++ "_from_idx"

toIdxName :: NameID -> String
toIdxName from
  = firstLower from ++ "_to_idx"

logBytesName :: NameID -> String
logBytesName n
  = "LOG_" ++ bytesName n

bytesName :: NameID -> String
bytesName n
  = "BYTES_IN_" ++ allUpper n

-- | All named entities get an addrName:
addrName xs = firstUpper xs ++ "Addr"

addrEndName n = firstUpper n ++ "AddrEnd"

enumName xs = "__FLP_IDX_" ++ map toUpper xs

-- | Only for (known to be) fixed-width things:
structName xs = firstUpper xs -- ++ "Struct"

getterName xs = firstLower xs

reverseGetterName xs = "from_" ++ firstLower xs

skipperName ns = "skip_bytes_to_" ++ firstUpper ns

jumperName ns = "jump_to_" ++ firstUpper ns

nextName ns = "next_" ++ firstUpper ns

bitsGetterName sz xs
  | sz == 1   = "get_" ++ xs ++ "_bit"
  | otherwise = "get_" ++ xs ++ "_bits"

bitsFromerName _ [] = error "empty NameID!"
bitsFromerName sz (x:xs)
  | sz == 1   = "set_" ++ x : xs ++ "_from_bool"
  | otherwise = "set_" ++ x : xs ++ "_from_u8"

firstGetterName xs = "get_first_" ++ firstLower xs

mkTy :: NameID -> Ty Span
mkTy s = P.parse' $ P.inputStreamFromString s

bytesAlignName n = allUpper n ++ "_BYTES_ALIGN"

logAlignName n = allUpper n ++ "_LOG_BYTES_ALIGN"

fieldPtrGetterName fs = "load_" ++ firstUpper fs ++ "Addr"
fieldPtrSetterName fs = "store_" ++ firstUpper fs ++ "Addr"

fieldStructGetterName fs = "load_" ++ firstUpper fs
fieldStructSetterName fs = "store_" ++ firstUpper fs

-- | Determine all existential-bound names brought into
--   scope by this expression. Alignment and Constrained
--   can be traversed because only a ":#" can refer to names.
findExists :: BaseExp -> [BaseExp]
findExists (Prim{}) = []
findExists (Con _ e) = findExists e
findExists (e :@ _) = findExists e
findExists (_ :+ _) = []
findExists (_ :|| _) = []
findExists (_ ::: _) = []
findExists e@(Exists _ e') = e : findExists e'
findExists (_ :# _) = []
findExists (Attr _ e) = findExists e

