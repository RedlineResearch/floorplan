module Language.Floorplan.Core.Compiler where
import Language.Floorplan.Core.Syntax hiding (accum)
import Language.Floorplan.Syntax
import qualified Data.Map.Strict as M
import Data.List (sort)
import Data.Maybe (fromJust, isJust)

-- | Build the dictionary / map of layer (and field) identifiers to their inner Demarc.
--   Todo: warning when multiple layers have the same name (which one `union` picks is undefined behavior)
buildMap :: Demarc -> M.Map String Demarc
buildMap (Enum{})  = M.empty
buildMap (Bits{})  = M.empty
buildMap (Union ds) = M.unions $ map buildMap ds
buildMap (Seq ds) = M.unions $ map buildMap ds
buildMap (PtrF{}) = M.empty
buildMap (PtrL{}) = M.empty
buildMap (Blob{}) = M.empty
buildMap (Graft{}) = M.empty
buildMap f@(Field name d) = M.insert name d (buildMap d)
buildMap (Pound d) = buildMap d
buildMap (Repetition _ d) = buildMap d
buildMap l@(Layer{}) = M.insert (name l) l (buildMap $ rhs l)

getNames :: Demarc -> [String]
getNames = let
    gN (Field n _) = Just n
    gN (Layer { name = n }) = Just n
    gN _ = Nothing
  in accum gN

-- | Grafting pre-processing pass
grafting' :: M.Map String Demarc -> Demarc -> Demarc
grafting' ds demarc = let

    lookup lid =
      case M.lookup lid ds of
        Nothing -> error $ "Undefined symbol '" ++ lid ++ "' during graft pre-processing phase."
        Just d -> d

    -- | Bool is whether or not we changed the input value
    gr (Graft (lid, args)) =
      case lookup lid of
        (Field _ d) -> (d, True)
        l@(Layer{}) -> (l, True) -- TODO: replace formals with args (no partial application)
    gr d = (d, False)

  in case fmapD gr demarc of
      (d, True)   -> grafting' ds d
      (d, False)  -> d

grafting :: [Demarc] -> Demarc -> Demarc
grafting ds demarc = grafting' (M.unions $ map buildMap ds) demarc

-- | This can be done before or after grafting phase. It checks that every @Layer@ and @Field@
--   has a globally unique name, reporting an error when the name is not unique.
checkUniqNames :: [Demarc] -> [String]
checkUniqNames ds =
  let ns = foldr (\a b -> getNames a ++ b) [] ds

      -- Assumes the input list is already sorted (duplicates are adjacent)
      getDups []     = []
      getDups (x:[]) = []
      getDups (x:y:ys)
        | x == y    = y : getDups (dropWhile (== y) ys)
        | otherwise = getDups (y:ys)

  in  getDups $ sort ns

countGrafting :: [Demarc] -> Int
countGrafting = let
    fn (Graft{}) = True
    fn _         = False
  in sum . map (countMatches fn)

-- | How many bytes are required for an Enum containing the given fields.
--   This computes ceiling(log_2(length(flags)) / 8).
enumBytes :: [FlagID] -> Int
enumBytes fs
  | len == 1  = 1
  | otherwise = (bytes 1)
  where len = length fs
        bytes acc
          | len <= acc  = 0
          | otherwise   = 1 + (bytes (acc * 256))

-- | TODO: parametrized delta_prim for architecture-dependent types
delta_prim :: Primitive -> Int
delta_prim Bit = 1
delta_prim Byte = 8
delta_prim Word = 8 * (delta_prim Byte)
delta_prim Page = 4096 * (delta_prim Byte)

pow :: Int -> Int -> Int
pow b e
  | e < 0             = error "exponentiation with negative exponent"
  | e == 0 && b == 0  = error "indeterminate exponent"
  | e == 0            = 1
  | e > 0             = b * (pow b (e - 1))

-- | Computes over arithmetic expressions in terms of literals
delta_lit :: LitArith -> Int
delta_lit (Plus l r) = (+) (delta_lit l) (delta_lit r)
delta_lit (Minus l r) = (-) (delta_lit l) (delta_lit r)
delta_lit (Times l r) = (*) (delta_lit l) (delta_lit r)
delta_lit (Div l r) = quot (delta_lit l) (delta_lit r)
delta_lit (Exponent b e) = pow (delta_lit b) (delta_lit e)
delta_lit (Lit l) = l

-- | Computes number of bits
delta :: SizeArith -> Int
delta (SizePlus l r) = (+) (delta l) (delta r)
delta (SizeMinus l r) = (-) (delta l) (delta r)
delta (SizeLit Nothing p) = delta_prim p
delta (SizeLit (Just lit) p) = (delta_lit lit) * (delta_prim p)

delta_bits = delta

bits2bytesUP v
  | v == 0    = 0
  | v <= 8    = 1
  | otherwise = 1 + bits2bytesUP (v - 8)

-- | Rounds up @SizeArith@ to the nearest whole byte.
delta_bytes :: SizeArith -> Int
delta_bytes sz = bits2bytesUP (delta sz)

fresh :: Demarc -> FormalID
fresh d = head (dropWhile (flip elem $ free_vars d) $ map (("var_" ++) . show) [0..])

compile :: Demarc -> BaseExp
compile (Enum fs) = Attr (BaseType $ EnumBT fs) (Prim $ enumBytes fs)
compile (Bits fs) = let fs' = zip (map fst fs) $ map (delta_bits . snd) fs
                    in  Attr (BaseType $ BitsBT fs') (Prim $ bits2bytesUP $ sum $ map snd fs')
compile (Union []) = Prim 0
compile (Union (d:ds)) = foldl (:||) (compile d) $ map compile ds
compile (Seq []) = Prim 0
compile (Seq (d:ds)) = foldl (:+) (compile d) $ map compile ds
compile (PtrF field) = Attr (BaseType $ PtrBT field) $ compile (Blob $ SizeLit Nothing Word)
compile (PtrL layer) = Attr (BaseType $ PtrBT layer) $ compile (Blob $ SizeLit Nothing Word)
compile (Blob sz) = Attr (BaseType $ SizeBT sz) $ Prim $ delta_bytes sz
compile (Field f d) = (:::) f (compile d)
compile (Pound d) = let f = fresh d
                    in Exists f $ f :# (compile d)
compile (Repetition f d) = f :# (compile d)
compile l@(Layer
  { name      = n
  , formals   = fs
  , magnitude = m
  , alignment = a
  , magAlign  = ma
  , contains  = cs
  , rhs       = d
  }) = n ::: (exists $ mag $ align $ contains $ compile d)
  where exists :: BaseExp -> BaseExp
        exists e
          | null fs   = e
          | otherwise = foldr Exists (Exists (head fs) e) (tail fs)
        mag
          | isJust m  = Con (delta_bytes $ fromJust m)
          | isJust ma = Con (delta_bytes $ fromJust ma)
          | otherwise = id
        align
          | isJust a  = (flip (:@)) (delta_bytes $ fromJust a)
          | isJust ma = (flip (:@)) (delta_bytes $ fromJust ma)
          | otherwise = id
        contains :: BaseExp -> BaseExp
        contains e
          | null cs   = e
          | otherwise = foldr Attr (Attr (Contains $ head cs) e) (map Contains $ tail cs)
compile (Graft _) = error "Grafting is illegal during the compile phase."

