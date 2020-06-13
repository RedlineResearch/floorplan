{-# LANGUAGE ScopedTypeVariables #-}
module Language.Floorplan.Core.Syntax where

import Data.Maybe (maybeToList)

import Language.Floorplan.Syntax (LayerID, FormalID, FlagID, FieldID, SizeArith(..))

type Nat = Int   -- >= 0
type Align = Int -- >= 1

type ExistsID = String -- ^ Formal identifiers (bound by Exists)
type NameID   = String -- ^ Names of bound layers and fields

data Exp a
  = Prim   Nat                  -- ^ Primitive number of bytes
  | Con    Nat        (Exp a)   -- ^ Constrained
  | (:@)   (Exp a)    Align     -- ^ Alignment
  | (:+)   (Exp a)    (Exp a)   -- ^ Sequencing
  | (:||)  (Exp a)    (Exp a)   -- ^ Union / alternation
  | (:::)  NameID     (Exp a)   -- ^ Layer (and field) name binding
  | Exists ExistsID   (Exp a)   -- ^ Formal name binding
  | (:#)   ExistsID   (Exp a)   -- ^ Repetitions
  | Attr   a          (Exp a)   -- ^ Extensible attributes
  deriving (Eq, Ord, Show)

data Attribute ty
  = Contains NameID
  | BaseType ty
  deriving (Eq, Ord, Show)

data BaseType
  = EnumBT [FlagID]
  | BitsBT [(NameID, Int)]
  | PtrBT  NameID
  | SizeBT SizeArith
  deriving (Eq, Ord, Show)

-- | Default core expression type for FLP compiler targeting Rust library
--   (with contains(...) attibutes):
type BaseExp = Exp (Attribute BaseType)

mTL :: Maybe a -> [a]
mTL = maybeToList

accumTopDown' :: [a] -> ([a] -> Exp b -> Maybe a) -> Exp b -> [a]
accumTopDown' as fn e@(Prim{})         = mTL (fn as e)
accumTopDown' as fn e1@(Con _ e2)      = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2
accumTopDown' as fn e1@(e2 :@ _)       = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2
accumTopDown' as fn e1@(e2 :+ e3)      = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2 ++ accumTopDown' (mTL (fn as e1) ++ as) fn e3
accumTopDown' as fn e1@(e2 :|| e3)     = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2 ++ accumTopDown' (mTL (fn as e1) ++ as) fn e3
accumTopDown' as fn e1@(_ ::: e2)      = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2
accumTopDown' as fn e1@(Exists _ e2)   = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2
accumTopDown' as fn e1@(_ :# e2)       = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2
accumTopDown' as fn e1@(Attr _ e2)     = mTL (fn as e1) ++ accumTopDown' (mTL (fn as e1) ++ as) fn e2

-- | Accumulate the results of a function operating over each Demarc in the Demarc IR tree,
--   where the function also takes a list of the results of the same function applied to
--   each of the ancestors of the current Demarc IR node, with the list in order from least
--   ancestor (closest) to greatest ancestor.
accumTopDown = accumTopDown' []

-- | Accumulate the results of applying some function to
--   every node in the Exp AST.
accum :: (Exp a -> Maybe b) -> Exp a -> [b]
accum fn e@(Prim{})         = maybeToList (fn e)
accum fn e1@(Con _ e2)      = maybeToList (fn e1) ++ accum fn e2
accum fn e1@(e2 :@ _)       = maybeToList (fn e1) ++ accum fn e2
accum fn e1@(e2 :+ e3)      = maybeToList (fn e1) ++ accum fn e2 ++ accum fn e3
accum fn e1@(e2 :|| e3)     = maybeToList (fn e1) ++ accum fn e2 ++ accum fn e3
accum fn e1@(_ ::: e2)      = maybeToList (fn e1) ++ accum fn e2
accum fn e1@(Exists _ e2)   = maybeToList (fn e1) ++ accum fn e2
accum fn e1@(_ :# e2)       = maybeToList (fn e1) ++ accum fn e2
accum fn e1@(Attr _ e2)     = maybeToList (fn e1) ++ accum fn e2

countExpNodes :: Exp a -> Int
countExpNodes e = length $ accum (const $ Just ()) e

-- | Call the given function on all subexpressions. Good for fixedpoint
--   functions calling on themselves in recursive case where they don't
--   care which type they see. This is slightly dangerous in the case where
--   something gets added to the core calculus. If this happens, PLEASE
--   check all callers of this function to see if they should handle the
--   new case personally.
callSub :: (Exp a -> [b]) -> Exp a -> [b]
callSub fn (Prim{}) = []
callSub fn (Con _ e) = fn e
callSub fn (e :@ _) = fn e
callSub fn (e1 :+ e2) = fn e1 ++ fn e2
callSub fn (e1 :|| e2) = fn e1 ++ fn e2
callSub fn (_ ::: e) = fn e
callSub fn (Exists _ e) = fn e
callSub fn (_ :# e) = fn e
callSub fn (Attr _ e) = fn e

plus :: Maybe Int -> Maybe Int -> Maybe Int
plus a b = do
  a' <- a
  b' <- b
  return $ a' + b'

-- | Conservatively computes the size of the given expression,
--   returning Nothing when a fixed size isn't easily known.
expSize :: Exp a -> Maybe Int
expSize (Prim n) = return n
expSize (Con n _) = return n
expSize (e :@ _) = expSize e
expSize (e1 :+ e2) = expSize e1 `plus` expSize e2
expSize (e1 :|| e2) =
  let s1 = expSize e1
      s2 = expSize e2
  in  if s1 == s2 then s1 else Nothing
expSize (_ ::: e) = expSize e
expSize (Exists _ e) = expSize e
expSize (_ :# _) = Nothing -- Conservative assumption
expSize (Attr _ e) = expSize e

-- | Just like @accum@, but also tracks the number of bytes of memory
--   that come before the subexpression for which we call the fncn.
l2r :: forall a b. (Maybe Nat -> Exp a -> Maybe b) -> Exp a -> [b]
l2r fn e' = let

    mTL :: Maybe Nat -> Exp a -> [b]
    mTL i = maybeToList . fn i

    lr :: Maybe Nat -> Exp a -> [b]
    lr i e@(Prim{})       = mTL i e
    lr i e1@(Con _ e2)    = mTL i e1 ++ lr i e2
    lr i e1@(e2 :@ _)     = mTL i e1 ++ lr i e2
    lr i e1@(e2 :+ e3)    = mTL i e1 ++ (lr i e2 ++ lr (i `plus` expSize e2) e3)
    lr i e1@(e2 :|| e3)   = mTL i e1 ++ (lr i e2 ++ lr i e3)
    lr i e1@(_ ::: e2)    = mTL i e1 ++ lr i e2
    lr i e1@(Exists _ e2) = mTL i e1 ++ lr i e2
    lr i e1@(_ :# e2)     = mTL i e1 ++ lr Nothing e2
    lr i e1@(Attr _ e2)   = mTL i e1 ++ lr i e2

  in lr (Just 0) e'
