{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveDataTypeable #-}
module Language.Floorplan.Syntax where

import Data.Maybe (maybeToList)

type LayerID  = String -- upper
type FormalID = String -- lower
type FlagID   = String -- upper
type FieldID  = String -- lower

data Primitive = Page | Word | Byte | Bit
  deriving (Eq, Ord, Show)

lexeme2prim "page"  = Just Page
lexeme2prim "pages" = Just Page
lexeme2prim "word"  = Just Word
lexeme2prim "words" = Just Word
lexeme2prim "byte"  = Just Byte
lexeme2prim "bytes" = Just Byte
lexeme2prim "bit"   = Just Bit
lexeme2prim "bits"  = Just Bit
lexeme2prim _       = Nothing

instance Read Primitive where
  readsPrec _ input = case lexeme2prim input of
    Just x  -> [(x,"")]
    Nothing -> []

data Demarc =
    Enum [FlagID]
  | Bits [(FieldID, SizeArith)]
  | Union [Demarc]
  | Seq   [Demarc]
  | PtrF  FieldID
  | PtrL  LayerID
  | Blob  SizeArith
  | Graft (LayerID, [Arg])
  | Field FieldID Demarc
  | Pound Demarc
  | Repetition FormalID Demarc
  | Layer
    { name      :: LayerID
    , formals   :: [FormalID]
    , magnitude :: Maybe SizeArith
    , alignment :: Maybe SizeArith
    , magAlign  :: Maybe SizeArith
    , contains  :: [LayerID]
    , rhs       :: Demarc
    }
  deriving (Eq, Ord, Show)

free_vars :: Demarc -> [FormalID]
free_vars d@(Enum{})         = []
free_vars d@(Bits{})         = []
free_vars d@(Union ds)       = concatMap free_vars ds
free_vars d@(Seq ds)         = concatMap free_vars ds
free_vars d@(PtrF{})         = []
free_vars d@(PtrL{})         = []
free_vars d@(Blob{})         = []
free_vars d@(Graft{})        = []
free_vars f@(Field _ d)      = free_vars d
free_vars p@(Pound d)        = free_vars d
free_vars r@(Repetition f d) = f : free_vars d
free_vars d@(Layer{})        =
  [ fv
  | fv <- free_vars (rhs d)
  , fv `notElem` formals d
  ]

accum :: (Demarc -> Maybe a) -> Demarc -> [a]
accum fn d@(Enum{})         = maybeToList (fn d)
accum fn d@(Bits{})         = maybeToList (fn d)
accum fn d@(Union ds)       = maybeToList (fn d) ++ concatMap (accum fn) ds
accum fn d@(Seq ds)         = maybeToList (fn d) ++ concatMap (accum fn) ds
accum fn d@(PtrF{})         = maybeToList (fn d)
accum fn d@(PtrL{})         = maybeToList (fn d)
accum fn d@(Blob{})         = maybeToList (fn d)
accum fn d@(Graft{})        = maybeToList (fn d)
accum fn f@(Field _ d)      = maybeToList (fn f) ++ accum fn d
accum fn p@(Pound d)        = maybeToList (fn p) ++ accum fn d
accum fn r@(Repetition _ d) = maybeToList (fn r) ++ accum fn d
accum fn d@(Layer{})        = maybeToList (fn d) ++ accum fn (rhs d)

countMatches :: (Demarc -> Bool) -> Demarc -> Int
countMatches fn demarc = length $ (flip accum) demarc $ \d -> if fn d then Just 1 else Nothing

-- | Bool is whether or not anything was changed (so no need to do Eq check in fixed-point transformations)
fmapD :: (Demarc -> (Demarc, Bool)) -> Demarc -> (Demarc, Bool)
fmapD fncn d@(Enum{}) = fncn d
fmapD fncn d@(Bits{}) = fncn d
fmapD fncn d@(Union ds) = let ds'     = map (fmapD fncn) ds
                              (d', b) = fncn (Union $ map fst ds')
                          in  (d', or (b : map snd ds'))
fmapD fncn d@(Seq ds) = let ds'   = map (fmapD fncn) ds
                            (d', b) = fncn (Seq $ map fst ds')
                        in  (d', or (b : map snd ds'))
fmapD fncn d@(PtrF{}) = fncn d
fmapD fncn d@(PtrL{}) = fncn d
fmapD fncn d@(Blob{}) = fncn d
fmapD fncn d@(Graft{}) = fncn d
fmapD fncn   (Field f d) = let (d',  b')   = fmapD fncn d
                               (d'', b'')  = fncn (Field f d')
                           in  (d'', b' || b'')
fmapD fncn   (Pound d) = let (d', b') = fmapD fncn d
                             (d'', b'') = fncn (Pound d')
                         in  (d'', b' || b'')
fmapD fncn   (Repetition f d) = let (d', b') = fmapD fncn d
                                    (d'', b'') = fncn (Repetition f d')
                                in  (d'', b' || b'')
fmapD fncn d@(Layer{}) = let (rhs', b') = fmapD fncn $ rhs d
                             (d'', b'') = fncn (d { rhs = rhs' })
                         in  (d'', b' || b'')

data Arg =
    ArgL Literal 
  | ArgF FormalID
  deriving (Eq, Ord, Show)

type Literal = Int

bin2int :: String -> Int
bin2int ('0':'b':xs) = let
    b2i [] = 0
    b2i (b:bs) = (read [b]) + (2 * b2i bs)
  in b2i $ reverse xs

data LitArith =
    Plus      LitArith LitArith
  | Minus     LitArith LitArith
  | Times     LitArith LitArith
  | Div       LitArith LitArith
  | Exponent  LitArith LitArith
  | Lit       Literal
  deriving (Eq, Ord, Show)

data SizeArith =
    SizePlus  SizeArith SizeArith
  | SizeMinus SizeArith SizeArith
  | SizeLit   (Maybe LitArith) Primitive
  deriving (Eq, Ord, Show)

