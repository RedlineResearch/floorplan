{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.Rust.Compiler
  ( genRust, writeModule
  ) where

import System.IO (IOMode(..), openFile, hClose)
import Data.Typeable (Typeable(..))

import Language.Rust.Parser as P
import Language.Rust.Syntax as R
import Language.Rust.Quote
import Language.Rust.Data.Ident
import Language.Rust.Data.Position

import Language.Floorplan.Core.Syntax

import Data.Bifunctor ( bimap )
import Data.Functor ( ($>) )
import Data.Ord (comparing)
import Data.List (sortBy, nub, inits)
import Data.Char (toUpper, toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.Bits
import qualified Debug.Trace as D

import Language.Rust.Pretty (writeSourceFile)
import Language.Floorplan.Rust.Types
import Language.Floorplan.Rust.Mapping
import Language.Floorplan.Rust.Common
import Language.Floorplan.Syntax(SizeArith(..), Primitive(..), FlagID(..))

moduleToItems :: RustItem -> [Item Span]
moduleToItems (RustImpl n is outer_is) =
  nub outer_is ++
  [ Impl [] InheritedV Final Normal Positive
      (Generics [] [] (WhereClause [] fS) fS) Nothing (mkTy n) (nub is) fS
  ]
moduleToItems (TopLevel is) = is

--(mkIdent n) (Just $ nub is) fakeSpan

-- | TODO: modules with same name should be merged into one.
mergeImpls :: [RustItem] -> [Item Span]
mergeImpls ms = let

    merge [] = []
    merge (x:[]) = [x]
    merge (x@(RustImpl xN xIS xOuter):y@(RustImpl yN yIS yOuter):ys)
      | xN == yN       = merge $ (RustImpl xN (xIS ++ yIS) (xOuter ++ yOuter)) : ys
      | otherwise      = x : merge (y:ys)
    merge (x@(TopLevel ns):y@(TopLevel ns'):ys) =
      merge $ (TopLevel $ ns ++ ns') : ys
    merge (x:y:ys) = x : merge (y : ys)

  in concatMap moduleToItems $ merge $ sortBy (comparing rustItemComparator) ms

genRust :: [BaseExp] -> SourceFile Span
genRust bes =
  SourceFile Nothing headerAttrs $
        headerItems
    ++  genFixedWidthStructs bes
    ++  (mergeImpls $
              genStaticOffsets    bes
          ++  genEnumAll'         bes
          ++  genFieldPtrGetters  bes
          ++  genMaps             bes
          ++  genContainsMethods  bes
          ++  genGetFirstMethods  bes
          ++  genBitsAccessors    bes
          ++  genDynamicSkippers  bes
          ++  genAddrAddr         bes
          ++  genNeighborParents  bes
          ++  genDistributeContainsAttr bes
          ++  genEnumAccessors    bes
          ++  genShadowAlloc      bes
          ++  genStoreWord        bes
          -- TODO: Check whether a RemainderAddr occurs *after* a LimitAddr, i.e. when the LHS
          -- of two sequential named fields occurs at an address greater than the address of
          -- the thing on the RHS, it's clearly malformed (i.e. the RTS speculatively made
          -- a new address, but realized that it's not allowed - TODO: eventually want some
          -- way to enforce, e.g. with types, the developer turns a "Speculative" address
          -- into a "Real" one, and that speculative addresses get generated for methods here
          -- that can't know for sure they'll produce pointers to valid things, but that
          -- can be "marked" as valid by doing a check).
        )

-- | Must return a nonempty list
findAlignment :: BaseExp -> [Int]
findAlignment (Prim{})      = [1]
findAlignment (Con _ e)     = findAlignment e
findAlignment (_ :@ align)  = [align]
findAlignment (e :+ _)      = findAlignment e -- ^ Alignment of first thing
findAlignment (e1 :|| e2)   =
  let a1 = findAlignment e1
      a2 = findAlignment e2
  in  a1 ++ a2
findAlignment (_ ::: e)     = findAlignment e
findAlignment (Exists _ e)  = findAlignment e
findAlignment (_ :# e)      = findAlignment e -- ^ Alignment of first repetition
findAlignment (Attr _ e)    = findAlignment e

-- | Generate the items associated with an Impl, as necessary for
--   making a new e.g. `FooAddr` type.
genAssocItems :: NameID -> BaseExp -> [Item Span]
genAssocItems n e =
  let nAddr = addrName n
      nAddrEnd = addrEndName n
      -- TODO: Alignment should conservatively be a list, not just the smallest one found
      --       (to detect alignment errors from a pointer as precisely as possible,
      --       though non-power-of-two alignments are rare / uncommon, so choosing
      --       the smallest one is likely sufficient for most use cases):
      bytesAlign = bytesAlignName n
      
      sz          = expSize e
      (Just sz')  = expSize e

      bytes = [items| pub const ${i| bytesName n |} : usize = ${e| mkIntExp sz' |}; |]

      consAlign = foldl mini 0 $ findAlignment e
            
      alignConst  = [items| pub const $bytesAlign : usize = 1 << ${i| logAlignName n |}; // ${e| mkIntExp consAlign |}; |]
      alignConst' = [items| pub const $bytesAlign : usize = ${e| mkIntExp consAlign |}; |]
                      
      logAlign
        | isPow2 consAlign  = [items| pub const ${i| logAlignName n |} : usize = ${e| mkIntExp $ log2 consAlign |}; |] ++ alignConst
        | otherwise         = alignConst'

      logBytes
        | isJust sz && isPow2 sz'   =
              bytes
          ++  [items| pub const ${i| logBytesName n |} : usize = ${e| mkIntExp $ log2 sz' |};
              |]
        | isJust sz = bytes
        | otherwise = []

      -- | Ignore the zero
      mini x 0 = x
      mini 0 x = x
      mini x y = min x y

  in logAlign ++
    [items| #[repr (C)]
            #[derive(Copy, Clone, Eq, Hash)]
            pub struct $nAddr(usize);
            deriveAddr!($nAddr, $bytesAlign);
            
            #[repr (C)]
            #[derive(Copy, Clone, Eq, Hash)]
            pub struct $nAddrEnd(usize);
            deriveAddr!($nAddrEnd, 1);
    |] ++ logBytes

genEnumAll :: (Int, (NameID, BaseExp)) -> RustItem
genEnumAll (i, (n, e)) =
  let nEnum = enumName n
  in RustImpl (addrName n) []
      [items| pub const $nEnum : u8 = ${e| mkIntExp i |};
      |]

mkNameStringExp :: (NameID, BaseExp) -> Expr Span
mkNameStringExp (n, _) = Lit [] (Str (allUpper n) Cooked Unsuffixed fakeSpan) fakeSpan

genFLPArray :: [(NameID, BaseExp)] -> RustItem
genFLPArray nes = TopLevel
  [items| pub const __FLP_IDX_UNMAPPED : u8 = 0;
    pub const __FLP_TYPES: &'static [&'static str] = & ${e| Vec []
      (mkNameStringExp ("UNMAPPED", undefined) : map mkNameStringExp nes) fakeSpan |}; |]

genStoreWord :: [BaseExp] -> [RustItem]
genStoreWord bes = let
    gSW :: BaseExp -> Maybe RustItem
    gSW (n ::: e) = do
      sz <- expSize e
      if sz == bytesInWord then return
        (RustImpl (addrName n)
          (snd [implItems|
            pub fn store_word(&self, val: usize) {
              self.store::<usize>(val);
            }
          |])
          [])
        else Nothing
    gSW rest = Nothing

    genStoreStruct :: BaseExp -> Maybe RustItem
    genStoreStruct (n ::: e) = do
      sz <- expSize e
      let sN = structName n
      let store_struct = fieldStructSetterName n
      let load_struct = fieldStructGetterName n
      return (RustImpl (addrName n)
        (snd [implItems|
          pub fn $store_struct(&self, val: $sN) {
            self.store::<$sN>(val);
          }
          pub fn $load_struct(&self) -> $sN {
            self.load::<$sN>()
          } |]) [])
    genStoreStruct _ = Nothing
  in concatMap (accum genStoreStruct) bes ++ concatMap (accum gSW) bes

genEnumAll' :: [BaseExp] -> [RustItem]
genEnumAll' bes =
  let findNames :: BaseExp -> Maybe (NameID, BaseExp)
      findNames (n ::: e) = Just (n, e)
      findNames _ = Nothing
  in  genFLPArray (nub $ concatMap (accum findNames) bes)
    : map genEnumAll (zip [1 :: Int .. ] (nub $ concatMap (accum findNames) bes))

-- [Item ()]
genStaticOffsets :: [BaseExp] -> [RustItem]
genStaticOffsets bes =
  let
      --(NameID, [ImplItem Span])
      findSO :: BaseExp -> Maybe RustItem
      findSO (n ::: e2) = Just $ RustImpl (addrName n) (concat $ l2r (mkN n) e2) (genAssocItems n e2)
      findSO _          = Nothing

      -- Finds all "neighboring" named subexpressions that are a static
      -- distance from the beginning of the subexpression.
      mkN :: NameID -> Maybe Nat -> BaseExp -> Maybe [ImplItem Span]
      mkN _ Nothing _ = Nothing
      mkN nOuter (Just k) (n ::: _) = Just $
        let aN = addrName n
            oN = offsetName n
            gN = getterName n
            rev_gN = reverseGetterName n
            aNOuter = addrName nOuter in
        -- TODO: [items| |] quasiquoter for multiple items
        snd [implItems|
            pub const ${i|oN|}: usize = ${e| mkIntExp k |};
            pub fn $gN(&self) -> $aN {
              self.plus::<$aN>($aNOuter :: $oN)
            }
            pub fn $rev_gN(addr: $aN) -> $aNOuter {
              addr.sub::<$aNOuter>($aNOuter :: $oN)
            }
          |]
        --[item| const FOO: usize = 22; |]
      mkN _ _ _ = Nothing

  in  concatMap (accum findSO) bes

genFieldPtrGetters :: [BaseExp] -> [RustItem]
genFieldPtrGetters bes = let

    findFieldPtr :: BaseExp -> Maybe RustItem
    findFieldPtr (n ::: (Attr (BaseType (PtrBT field)) blob)) =
      let aN = addrName n
          get_field = fieldPtrGetterName field
          set_field = fieldPtrSetterName field
          newAddr = addrName field in
      Just $ RustImpl (addrName n)
        (snd $ [implItems|
          pub fn $get_field(self) -> $newAddr {
            let ret = self.load::<$newAddr>();
            //backtraceHere!(ret);
            ret
          }
          pub fn $set_field(self, val : $newAddr) {
            //backtraceHere!(val);
            self.store::<$newAddr>(val)
          }
        |])
        []
    findFieldPtr _ = Nothing

  in concatMap (accum findFieldPtr) bes

-- | Distribute the 'contains(...)' attributes on layers onto all of the named fields
--   (and other layers) one-level-deep (don't traverse the AST through names), such that
--   we get out conversion functions to/from the "contained" thing and the inner names found.
--   These casting functions get associated with the container itself, not the addresses
--   being cast so as to keep how the contains() attribute works simple to understand.
--
--   TODO: We can do *all* sorts of assertions here, such as checking alignment constraints
--   e.g. if you cast an immix Cell to an immix Line, then that Cell *should* have started
--   on an aligned-Line. If this was not the case, either there was an error, or we should
--   provide support for optional types that fail when alignment fails. It's probably better
--   for now though to just require that the system developer check alignment constraints
--   on there own and have FLP print out an error message when alignments are broken, that way
--   we can figure out what to do with bad alignments when we have an actual example.
genDistributeContainsAttr :: [BaseExp] -> [RustItem]
genDistributeContainsAttr bes = let

    findNamed :: BaseExp -> Maybe RustItem
    findNamed (n ::: e) = Just $ RustImpl (structName n) (mkDistr e) []
    findNamed _ = Nothing

    mkDistr :: BaseExp -> [ImplItem Span]
    mkDistr (Attr (Contains nCont) e) = distribute nCont e ++ mkDistr e -- Distribute 'contains()' things
    mkDistr (n ::: _) = [] -- Only go one level of naming deep.
    mkDistr e = callSub mkDistr e -- Recursive call on subexpressions

    distribute :: NameID -> BaseExp -> [ImplItem Span]
    distribute nCont (nInner ::: eInner) = let
        cast_from1 = castFromName nCont nInner
        cast_from2 = castFromName nInner nCont
        nContA = addrName nCont
        nInnerA = addrName nInner
      in (snd $
        [implItems|
          pub fn $cast_from1(addr : $nContA) -> $nInnerA { $nInnerA::from_usize(addr.as_usize()) }
          pub fn $cast_from2(addr : $nInnerA) -> $nContA { $nContA::from_usize(addr.as_usize()) }
        |]) ++ distribute nCont eInner
    distribute nCont e = callSub (distribute nCont) e

  in concatMap (accum findNamed) bes

genContainsMethods :: [BaseExp] -> [RustItem]
genContainsMethods bes = let

    findNamed :: BaseExp -> Maybe RustItem
    findNamed (n ::: e) = Just $ RustImpl (addrName n) (concat $ l2r (mkContains n) e) []
    findNamed _ = Nothing

    -- Make the 'name_contains()' methods for appropriate subexpressions
    mkContains :: NameID -> Maybe Nat -> BaseExp -> Maybe [ImplItem Span]
    mkContains nOuter sz (nInner ::: _) = Just $
      let containsFn = containsFnName nInner
          innerAddr = addrName nInner
          outerAddrEnd = addrEndName nOuter
      in snd $
        [implItems|  pub fn $containsFn(&self, addr: $innerAddr, end: $outerAddrEnd) -> bool {
                          addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
                      }
         |]
    mkContains _ _ _ = Nothing

  in concatMap (accum findNamed) bes

genBitsAccessors :: [BaseExp] -> [RustItem]
genBitsAccessors bes = let

    -- overall size (bytes) -> (curr offset, accumulated) -> (Field Name, Field Size (bits)) -> Result
    --                    (b                      -> a             -> b)
    singleField :: Nat -> (Int, [ImplItem Span]) -> (NameID, Int) -> (Int, [ImplItem Span])
    singleField sz (curr_offset, accum) (fieldName, bits)
      | bits == 0 = (curr_offset, accum) -- No bits - continue on (no accessor).
      | bits < bitsInByte && sz == 1 = -- 1-7 bits, no byte-offsets required (because sz==1):
        let getter = bitsGetterName bits fieldName
            fieldNameLow = fieldName ++ "_LOW_BIT"
            fieldNameBits = fieldName ++ "_NUM_BITS"
            fieldNameMask = fieldName ++ "_MASK"
            fromer = bitsFromerName bits fieldName
            getterItem
              | bits == 1 =
                  [implItems| pub fn $getter(&self) -> bool { ((self.0[0]) & Self::$fieldNameMask) > 0 }
                              pub fn $fromer(b : bool) -> Self { Self([(b as u8) << Self::$fieldNameLow]) }
                  |]
              | otherwise =
                  [implItems| pub fn $getter(&self) -> u8   {  (self.0[0]) & Self::$fieldNameMask      }
                              pub fn $fromer(v : u8) -> Self { Self([v]) }
                  |]
        in (curr_offset + bits, (snd $
          [implItems|
            pub const $fieldNameLow  : usize = ${e| mkIntExp curr_offset |};
            pub const $fieldNameBits : usize = ${e| mkIntExp bits |};
            pub const $fieldNameMask : u8 = ${e| mkBinExp $ ((1 `shiftL` (curr_offset + bits)) - 1) .&. (complement ((1 `shiftL` curr_offset) - 1)) |};
          |]) ++ snd getterItem ++ accum)
      | bits == bitsInByte && sz == 1 = -- No offset required, whole byte:
        let getter = bitsGetterName bits fieldName
            fromer = bitsFromerName bits fieldName
            fieldNameBits = fieldName ++ "_NUM_BITS"
        in (curr_offset + bits, (snd $
          [implItems| pub fn $getter(&self) -> u8 { self.0[0] }
                      pub fn $fromer(v : u8) -> Self { Self([v]) }
                      pub const $fieldNameBits : usize = ${e| mkIntExp bits |};
          |]) ++ accum)
      | otherwise = error $ "TODO: sz=" ++ show sz ++ ", bits=" ++ show bits

    -- Returns ([inside the impl], [outside the impl])
    findBitsExp :: BaseExp -> Maybe ([ImplItem Span], [Item Span])
    findBitsExp (Con _ e) = findBitsExp e
    findBitsExp (e :@ _) = findBitsExp e
    findBitsExp (Exists _ e) = findBitsExp e
    findBitsExp (Attr (BaseType (BitsBT fs)) (Prim sz))
      | sz > 0 = Just $ (snd $ foldl (singleField sz) (0, []) fs, [])
      | otherwise = Nothing
    findBitsExp _ = Nothing

    findNamedBitsExps (name ::: e) = (uncurry $ RustImpl (structName name)) <$> findBitsExp e
    findNamedBitsExps _ = Nothing

  in concatMap (accum findNamedBitsExps) bes

genEnumAccessors :: [BaseExp] -> [RustItem]
genEnumAccessors bes = let

    findEnums :: BaseExp -> Maybe [RustItem]
    findEnums (n ::: (Attr (BaseType (EnumBT fs)) (Prim sz))) = Just $ [ RustImpl (structName n) (mkEnumFns fs sz) [] ]
    findEnums _ = Nothing

    mkEnumFns :: [FlagID] -> Nat -> [ImplItem Span]
    mkEnumFns fs sz = let
        valType
          | sz == 1 = "u8"
          | sz == 2 = "u16"
          | sz == 4 = "u32"
          | sz == 8 = "u64"
          | otherwise = error $ "Unhandled size=" ++ show sz
      in snd
        [implItems|
          #[inline(always)]
          pub fn from_enum(val : $valType) -> Self { Self([val]) }
          #[inline(always)]
          pub fn to_enum(&self) -> $valType { self.0[0] }
        |]

  in concat $ concatMap (accum findEnums) bes
-- pub fn from_enum(val : u8) -> Self { LineMark([val]) }

genDynamicSkippers :: [BaseExp] -> [RustItem]
genDynamicSkippers bes = let

    findDynamic :: BaseExp -> Maybe [RustItem]
    findDynamic (_ :# e) = Just $ iterators (expSize e) (names e) (names e)
    findDynamic _ = Nothing

    names (e1 :|| e2) = names e1 ++ names e2
    names (n ::: e) = n : names e
    names (Con _ e) = names e
    names (e :@ _) = names e
    names (e1 :+ e2)
      | expSize e2 == Just 0 = names e1
      | otherwise            = []
    names (Exists _ e) = names e
    names (_ :# _) = []
    names (Attr _ e) = names e
    names (Prim{}) = []

    iterators _ _ [] = []
    iterators s@Nothing ns (n':ns') = let
        skipper name =
          let addrN = addrName name
              skName = skipperName name
          in  snd $ [implItems| pub fn $skName(&self, bytes : usize) -> $addrN { self.plus(bytes) } |]
      in (RustImpl (addrName n') (concatMap skipper ns) []) : (iterators s ns ns')
    iterators s@(Just sz) ns (n':ns') = let
        jumper name =
          let addrN = addrName name
              jmpName = jumperName name
              bytesInThing = bytesName n'
              logBytesInThing = logBytesName n'
          in  snd $
            if isPow2 sz
              then  [implItems|
                      pub fn $jmpName(&self, count : usize) -> $addrN {
                        self.plus(count << $logBytesInThing)
                      } |]
              else  [implItems|
                      pub fn $jmpName(&self, count : usize) -> $addrN {
                        self.plus($bytesInThing * count)
                      } |]
      in (RustImpl (addrName n') (concatMap jumper ns) []) : (iterators s ns ns')

    
    findNamed :: BaseExp -> Maybe [RustItem]
    findNamed (n ::: e) = innerPound n e
    findNamed _ = Nothing
   
    innerPound :: NameID -> BaseExp -> Maybe [RustItem]
    innerPound n (Exists _ e) = innerPound n e
    innerPound n (_ :# e) = Just $ [ RustImpl (addrName n) (fixedSzIters e) [] ]
    innerPound _ _ = Nothing

    fixedSzIters :: BaseExp -> [ImplItem Span]
    fixedSzIters (Attr (BaseType (PtrBT ptrT)) _) =
      let aName   = addrName ptrT
          aaName  = addrName aName
          nxtName = nextName ptrT
          firstName = firstGetterName aaName
      in  snd $
          [implItems| pub fn $nxtName(ptr: $aaName) -> $aaName { ptr.plus(BYTES_IN_WORD) }
                      pub fn $firstName(&self) -> $aaName { $aaName(self.as_usize()) }
          |]
    fixedSzIters (e1 :|| e2) = fixedSzIters e1 ++ fixedSzIters e2
    fixedSzIters (n ::: _) = []
    fixedSzIters (Con _ e) = fixedSzIters e
    fixedSzIters (e1 :+ e2)
      | expSize e2 == Just 0 = fixedSzIters e1
      | otherwise            = []
    fixedSzIters (Exists _ e) = fixedSzIters e
    fixedSzIters (_ :# _) = []
    fixedSzIters (Prim {}) = []
    fixedSzIters (Attr _ e) = fixedSzIters e

  in (concat $ concatMap (accum findDynamic) bes) ++ (concat $ concatMap (accum findNamed) bes)

genAddrAddr :: [BaseExp] -> [RustItem]
genAddrAddr bes = let

    findPtrBT (Attr (BaseType (PtrBT ptrT)) _) =
      let aName = addrName ptrT
          aaName = addrName aName
          bytesAlign = bytesAlignName aName
          getter = getterName aName
      in  Just $ RustImpl aaName
            (snd $ [implItems|
              pub fn $getter(&self) -> $aName { self.load() }
            |])
            [items| #[repr (C)]
                    #[derive(Copy, Clone, Eq, Hash)]
                    pub struct $aaName(usize);
                    pub const $bytesAlign : usize = 1;
                    deriveAddr!($aaName, $bytesAlign); |]
    findPtrBT _ = Nothing

  in concatMap (accum findPtrBT) bes

-- | Finds all the NameIDs for which the first (possibly only) instance of some NameID
--   occurs flush with the beginning of the top-level expression (zero bytes distance
subexpFirsts :: BaseExp -> [NameID]
subexpFirsts be = let

    -- INVARIANT of sF: never call on a subexpression that doesn't *start* at the
    -- beginning of the outer-most expression 'be'.
    sF (n ::: e) = n : sF e
    sF (Attr (Contains n) e) = n : sF e
    sF (Attr (BaseType (SizeBT (SizeLit Nothing p))) (Prim{})) = [show p]
    sF (Prim{}) = []
    sF (Con _ e) = sF e
    sF (e :@ _) = sF e
    sF (e1 :+ e2)
      | expSize e1 == Just 0 = sF e1 ++ sF e2
      | otherwise            = sF e1 -- Conservative approximation (ignore things in e2 even though expSize is conservative)
    sF (e1 :|| e2) = sF e1 ++ sF e2
    sF (Exists _ e) = sF e
    sF (_ :# e) = sF e -- First instances of 'e' is in 'subexpFirsts be'
    sF (Attr _ e) = sF e

  in sF be

genGetFirstMethods :: [BaseExp] -> [RustItem]
genGetFirstMethods bes = let

    findNamed :: BaseExp -> Maybe RustItem
    findNamed (n ::: e) = Just $ RustImpl (addrName n) (concatMap mkGetFirst $ subexpFirsts e) []
    findNamed _ = Nothing

    mkGetFirst n = let
        get_first = firstGetterName n
        aN = addrName n
      in snd [implItems|
      pub fn $get_first(self) -> $aN {
        $aN::from_usize(self.as_usize())
      } |]

  in concatMap (accum findNamed) bes

type NamedE = (NameID, BaseExp)
type Neighbors = (NamedE, NamedE)

-- | Makes a plain-vanilla pass-by-value paramument with the given type:
mkParam :: BindingMode -> String -> String -> Arg Span
mkParam mode var ty = Arg (Just $ IdentP mode (mkIdent var) Nothing fS) (PathTy Nothing (Path False [PathSegment (mkIdent ty) Nothing fS] fS) fS) fS

mkSelf = SelfRegion Nothing Immutable fS

-- | Makes a plain-vanilla type from a name
mkRustTy :: String -> Ty Span
mkRustTy name = PathTy Nothing (Path False [PathSegment (mkIdent name) Nothing fS] fS) fS

-- List is /all/ the ancestral parents in bottom-up order. NamedE is the child who
-- has the ancesters.
type Ancestry = ([NamedE], NamedE)

genShadowAlloc :: [BaseExp] -> [RustItem]
genShadowAlloc bes = let

    findParentList :: BaseExp -> [Ancestry]
    findParentList be = let
        -- acc is list of parents accumulator along the path from the root to the given BaseExp
        fPL :: [NamedE] -> BaseExp -> [Ancestry]
        fPL acc (prnt ::: e) = (acc, (prnt, e)) : (fPL ((prnt,e) : acc) e)
        fPL acc e = callSub (fPL acc) e
      in fPL [] be
    
    filterAncestry :: [Ancestry] -> [Ancestry]
    filterAncestry [] = []
    filterAncestry ((prnts, (cN, cE)) : rest) = let
        
        matchingRest :: [Ancestry]
        matchingRest = filter ((== cN) . fst . snd) rest

        notMatchingRest :: [Ancestry]
        notMatchingRest = filter ((/= cN) . fst . snd) rest
        
        allPrnts = (prnts ++ (concatMap fst matchingRest))
      in (nub allPrnts, (cN, cE)) : filterAncestry notMatchingRest

    mkShadowAlloc :: Ancestry -> [RustItem]
    mkShadowAlloc (prnts, child) = let
        nBytesIn = bytesName (fst child)
        flp_IDX_NAME = enumName (fst child)
        mkRustName :: NameID -> Expr Span
        mkRustName n = [expr| $n |]
        mkInner =
          -- Static u8 (indices / IDXs) array of the parents of this child
          snd [implItems|
            pub const __FLP_IDX : u8 = $flp_IDX_NAME;
            pub const __FLP_PARENTS : &'static [u8] = & ${e| Vec []
              (map mkRustName $ (map (enumName . fst) prnts) ++ [enumName "UNMAPPED"]) fakeSpan |}; |]
          ++ (case expSize (snd child) of
                Nothing -> -- Variable-size child - caller needs to indicate size 
                  snd [implItems|
                    pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) {
                      //assert!(Self::__FLP_PARENTS.contains(&IDX));
                      //for i in 0 .. bytes {
                      //  let offset = self.as_usize() + i;
                      //  check_expect_pc!(offset, &vec_IDX);
                      //}
                    }
                  |]
                Just sz -> if sz == 0 then [] -- Cannot actually shadow allocate something of zero size (no-op)
                  else -- Fixed-size child: caller need not indicate the size
                    snd [implItems|
                      pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) {
                        //assert!(Self::__FLP_PARENTS.contains(&IDX));
                        //for i in 0 .. $nBytesIn {
                        //  let offset = self.as_usize() + i;
                        //  check_expect_pc!(offset, &vec_IDX);
                        //}
                      } |])

      in [RustImpl (addrName $ fst child) mkInner [] ]

  in concatMap mkShadowAlloc (filterAncestry $ concatMap findParentList bes)
  -- in undefined -- concat $ concatMap (accum findParentList) bes

-- | Generate all things pertaining to neighboring names, along with their common parent name.
genNeighborParents :: [BaseExp] -> [RustItem]
genNeighborParents bes = let

    findParent :: BaseExp -> Maybe [RustItem]
    findParent (prnt ::: e) = Just $ mkItems prnt (findNeighbors e)
                                  ++ fromMaybe [] (mkSeqFns prnt <$> (findSeqs False e))
                                  ++ mkAddrEndDiff prnt
                                  ++ fromMaybe [] (mkAllocShadow prnt <$> (findSeqs True e))
    findParent _ = Nothing

    -- Find the canonical sequence of named things
    findSeqs :: Bool -> BaseExp -> Maybe [NamedE]
    findSeqs unwrapAll be = let
        findS :: BaseExp -> Maybe [NamedE]
        findS (Con _ e) = findS e
        findS (e :@ _) = findS e
        findS (Exists _ e) = findS e
        findS (Attr _ e) = findS e
        findS (e1 :+ e2) = findS e1 >>= \a -> findS e2 >>= \b -> pure (a ++ b)
        findS (n ::: e) =  Just $ [ (n,e) ]
        findS (e1 :|| e2)
          | unwrapAll = findS e1 >>= \a -> findS e2 >>= \b -> pure (a ++ b)
          | otherwise = Nothing
        findS (_ :# e)
          | unwrapAll = findS e
          | otherwise = Nothing
        findS (Prim{}) = Nothing
      in findS be

    -- | Individually named shadow_alloc_from_*() functions, i.e. for each parent.
    mkAllocShadow :: NameID -> [NamedE] -> [RustItem]
    mkAllocShadow prnt nes = let

        shadow_alloc_fn = "shadow_alloc_from_" ++ firstLower prnt
    
        mkImpl ne = RustImpl (addrName $ fst ne) (inner ne) []

        prntConstFLP = enumName prnt

        inner (n, e) =
          let nConstFLP = enumName n
              nBytesIn = bytesName n
          in case expSize e of
            Nothing ->
              [implItem|
                pub fn $shadow_alloc_fn(&self, bytes: usize) {
                  //for i in 0..bytes {
                  //  let offset = self.as_usize() + i;
                  //  //check_expect_pc!(offset, vec![$prntConstFLP]);
                  //}
                } |] : []
            Just sz -> if sz == 0 then [] else
              [implItem|
                pub fn $shadow_alloc_fn(&self) {
                  //for i in 0..$nBytesIn {
                  //  let offset = self.as_usize() + i;
                  //  //check_expect_pc!(offset, vec![$prntConstFLP]);
                  //}
                } |] : []
      
      in map mkImpl nes

    -- Functions pertaining to sequences of named things within a parent
    mkSeqFns :: NameID -> [NamedE] -> [RustItem]
    mkSeqFns prnt nes = let
       
        init_sequence = initSeqName (map fst nes)

        paramNames = map (\i -> "a" ++ show i) [1 .. length nes]
        paramTypes = map (const "usize") (map fst nes)

        prntEndName = addrEndName prnt

        plusE :: [(String, NamedE)] -> Expr Span
        plusE [] = [expr| self |]
        plusE ((paramName, (fieldName, coreExpr)):ns) =
          [expr| ${e| plusE ns |}.plus::<$fieldName>($paramName) |]

        newParams   = mkSelf : map (uncurry $ mkParam (ByValue Immutable)) (zip paramNames paramTypes)
        newRetTy    = TupTy (map (mkRustTy . addrName . fst) nes
                          ++ [mkRustTy prntEndName]) fS
        newStmts    = let
            exprs = (map (plusE . reverse) $ tail $ inits $ zip ("a0" : paramNames) ((map (bimap addrName id)) nes))
        
            exprLast = [expr| ${e| last exprs |}.plus::<$prntEndName>(${i| last $ paramNames |}) |]

          in  [stmts|
                let a0 : usize = 0;
                let ret = ${e| TupExpr [] (exprs ++ [exprLast]) fS |};
                return ret;
              |]

        -- (FnDecl [Param a] (Maybe (Ty a)) Bool a)
        bareFn = case [implItem| pub fn $init_sequence() -> (Int,) { (3,) } |] of
          (MethodI  a1 a2 a3 a4 a5 (MethodSig c1 c2 c3 (FnDecl params    (Just ty)       bool spn)) (Block stmts    b2 b3) a8) ->
            MethodI a1 a2 a3 a4 a5 (MethodSig c1 c2 c3 (FnDecl newParams (Just newRetTy) bool spn)) (Block newStmts b2 b3) a8
          _ -> error "Compiler incompatibility with your version of language-rust package."

        mSF = [ bareFn ]

      in  [ RustImpl (addrName prnt) mSF []
          ]

    -- parent name -> [neighbors] -> rust items
    mkItems :: NameID -> [Neighbors] -> [RustItem]
    mkItems prnt ns =
      let !_ = D.traceShowId (prnt, ns)
      in  ( RustImpl (structName prnt) (concatMap mkNeighborFns ns) []
          ) : concatMap mkNeighborAddrEndFns ns

    mkNeighborAddrEndFns :: Neighbors -> [RustItem]
    mkNeighborAddrEndFns ((n1, e1), (n2, e2)) = let

        n1EndA = addrEndName n1
        toEndN = "to_" ++ firstUpper n1EndA
        fromEndN = "from_" ++ firstUpper n1EndA
        n2A = addrName n2

        mkN2 = snd $ [implItems|
            pub fn $toEndN(self) -> $n1EndA { $n1EndA::from_usize(self.as_usize()) }
            pub fn $fromEndN(ptr : $n1EndA) -> $n2A { $n2A::from_usize(ptr.as_usize()) }
          |]

        toN2FnN = "to_" ++ firstUpper n2A
        fromN2FnN = "from_" ++ firstUpper n2A

        mkN1End = snd $ [implItems|
            pub fn $toN2FnN(&self) -> $n2A { $n2A::from_usize(self.as_usize()) }
            pub fn $fromN2FnN(ptr : $n2A) -> $n1EndA { $n1EndA::from_usize(ptr.as_usize()) }
          |]

      in  [ RustImpl n1EndA mkN1End []
          , RustImpl n2A    mkN2    []
          ]

    mkAddrEndDiff :: NameID -> [RustItem]
    mkAddrEndDiff n = let
        nEndA = addrEndName n
        nA    = addrName n
      in
      [ RustImpl nA
          (snd [implItems| pub fn size_of(&self, end : $nEndA) -> usize {
            debug_assert!(end.as_usize() >= self.as_usize());
            end.diff(*self)
          } |]) []
      ]

    mkNeighborFns :: Neighbors -> [ImplItem Span]
    mkNeighborFns ((n1, e1), (n2, e2)) = let
        
        n1A = addrName n1
        n2A = addrName n2

        init_after = initAfterName n1 n2

        -- | Looks for all (depth = 1) named entities that consume the entire space of the BaseExp
        --   (can't dig into ':+' expressions):
        findWholeChunks :: BaseExp -> [NameID]
        findWholeChunks be = let
            fWC (n ::: _) = [n] -- Don't recurse because depth=1
            fWC (e1 :+ e2)
              | expSize e2 == Just 0 = fWC e1
              | expSize e1 == Just 0 = fWC e2
              | otherwise            = []      -- Doesn't consume the entire space
            fWC (_ :# e) = [] -- Doesn't consume the entire space
            fWC e = callSub fWC e -- All other recursive cases
          in fWC be

        poundLeftBumpFn n =
          let bump_new_thing = bumpAllocName n
              aN = addrName n
          in  snd $
              [implItems| pub fn $bump_new_thing(rhs : $n2A, bytes : usize) -> ($aN, $n2A) {
                            (rhs.plus(0), rhs.plus(bytes))
                          }
              |]
        
        bump_valid = bumpValidName n1 n2
        bumpValid = snd $
          [implItems| pub fn $bump_valid(p1 : $n1A, p2 : $n2A) -> bool { p1.lte(p2) } |]

        -- Checks that the LHS (e1) was exactly a field containing an array of things, and
        -- makes the function that allows you to bump-allocate a new entry in that array
        -- past the *current* end of the array.
        poundLeftBumpFns = case e1 of
          (Exists _ (_ :# e)) -> concatMap poundLeftBumpFn (findWholeChunks e)
          _ -> []
        
        -- The right-hand neighbor is null-sized
        --emptyRight = case expSize e2 of
        --  (Just 0) ->
        --  _ -> []
    
        memset_until = memsetUntilName n1 n2
        n1Bytes = bytesName n1

        memsetRange = case expSize e1 of
          Just sz | sz > 0 -> snd [implItems|
              pub fn $memset_until(val : u8, base : $n1A) {
                base.memset(val, $n1Bytes)
              } |]
                  | otherwise -> []
          Nothing -> snd [implItems|
              pub fn $memset_until(val : u8, base : $n1A, mx : $n2A) {
                debug_assert!(mx.greater(base));
                base.memset(val, mx.diff(base))
              } |]
      
        initAfterFn = (snd $ case expSize e1 of
            Just sz | sz == 0   ->  [implItems| pub fn $init_after(p1 : $n1A) -> $n2A { $n2A::from_usize(p1.as_usize()) } |]
                    | otherwise ->  [implItems| pub fn $init_after(p1 : $n1A) -> $n2A { p1.plus($n1Bytes) } |]
            Nothing             ->  [implItems| pub fn $init_after(p1 : $n1A, bytes : usize) -> $n2A { p1.plus(bytes) } |])
        
      in memsetRange ++ poundLeftBumpFns ++ initAfterFn ++ bumpValid

    findNeighbors :: BaseExp -> [Neighbors]
    findNeighbors be = let

        most :: ((BaseExp, BaseExp) -> BaseExp) -> BaseExp -> [NamedE]
        most fn = let
            most' (n ::: e) = [(n,e)] -- We don't find nested-names (names are boundaries)
            most' (e1 :+ e2) = most' (fn (e1,e2))
            most' (Attr _ e) = most' e
            most' (Prim{}) = []
            most' (Con _ e) = most' e
            most' (e :@ _) = most' e
            most' (e1 :|| e2) = []
            most' (Exists _ e) = most' e
            most' (_ :# e) = []
          in most'
        
        left_most :: BaseExp -> [NamedE]
        left_most = most fst
        right_most :: BaseExp -> [NamedE]
        right_most = most snd

        fN :: [NamedE] -> BaseExp -> [NamedE] -> [Neighbors]
        fN left (e1 :+ e2) right = fN left e1 (left_most e2) ++ fN (right_most e1) e2 right
        fN left (n ::: e)  right = (zip left (repeat (n,e))) ++ (zip (repeat (n,e)) right)
        fN l (Attr _ e) r = fN l e r
        fN l (Prim{}) r = []
        fN l (Con _ e) r = fN l e r
        fN l (e :@ _) r = fN l e r
        fN l (e1 :|| e2) r = fN [] e1 [] ++ fN [] e2 [] -- We throw away neighbors at unions (for simplicity)
        fN l (Exists _ e) r = fN l e r
        fN l (_ :# e) r = fN [] e [] -- Throw away neighbors at repetitions, because not every instance inside '#' is left-most or right-most
      in nub $ fN [] be []

  in concat $ concatMap (accum findParent) bes

-- | TODO: Remove allow(dead_code) and allow(unused_imports)
(SourceFile _ headerAttrs headerItems) = [sourceFile|
  #![allow(non_camel_case_types)]
  #![allow(non_snake_case)]
  #![allow(dead_code)]
  #![allow(unused_imports)]
  #![allow(unused_variables)]
  extern crate libc;
  extern crate flp_framework;
  use std::cmp;
  use std::fmt;
  use std::mem::size_of as size_of;
  use self::libc::size_t as size_t;
  
  pub use self::flp_framework::*;
|]
--  #[macro_use]
--  pub mod address;
--  use self::address::*;

-- writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
writeModule :: (Monoid a, Typeable a) => FilePath -> SourceFile a -> IO ()
writeModule outdir sf = do
  fd <- openFile outdir WriteMode
  writeSourceFile fd sf
  hClose fd

