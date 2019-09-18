{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.Rust.Mapping
  where
{- Code generator for mappings due to same-named repetitions in a FLP spec.
 -}

import Language.Rust.Parser as P
import Language.Rust.Syntax as R hiding (Byte)
import Language.Rust.Quote
import Language.Rust.Data.Ident
import Language.Rust.Data.Position

import Language.Floorplan.Syntax(SizeArith(..), Primitive(..))
import Language.Floorplan.Core.Syntax
import Language.Floorplan.Rust.Types
import Language.Floorplan.Rust.Common

import Data.Functor ( ($>) )
import Data.Ord (comparing)
import Data.List (sortBy, nub)
import Data.Char (toUpper, toLower)
import qualified Debug.Trace as D

genFixedWidthStructs :: [BaseExp] -> [Item Span]
genFixedWidthStructs bes = let

    gFWS (n ::: e) = let sN = structName n in Just $ case expSize e of
      Nothing -> [items| #[derive(Copy, Clone)]
                         pub struct $sN( () );   // Unit type!!!
                 |]
      Just sz -> [items|
                            #[derive(Copy, Clone)]
                            pub struct $sN([u8; ${e| mkIntExp sz |}]);

                            impl PartialEq for $sN {
                            #[inline(always)] fn eq(&self, other: &$sN) -> bool {
                                for i in 0..(size_of::<$sN>()) {
                                  if self.0[i] != other.0[i] { return false; }
                                }
                                return true;
                            }
                            #[inline(always)] fn ne(&self, other: &$sN) -> bool {
                                for i in 0..(size_of::<$sN>()) {
                                  if self.0[i] != other.0[i] { return true; }
                                }
                                return false;
                            }
                        } 
                     |]
    gFWS _ = Nothing

  in nub $ concat $ concatMap (accum gFWS) bes

genMaps :: [BaseExp] -> [RustItem]
genMaps bes =
  let findExistBind :: BaseExp -> Maybe [RustItem]
      findExistBind (outerName ::: e) = 
        Just $ concatMap (mkMapFns outerName) (findExists e)
      findExistBind _ = Nothing

      -- [ImplItem Span]
      mkMapFns :: NameID -> BaseExp -> [RustItem]
      mkMapFns oN (Exists n e) = let

          nameOf Byte = "Byte" -- "u8" TODO: replace all bytes with 'u8'
          nameOf x    = show x

          -- TODO: mkMF should be a more principled pre-processing pass (find pairs that work):
          mkMF (Attr (BaseType (SizeBT (SizeLit Nothing p1))) e1
              , Attr (BaseType (SizeBT (SizeLit Nothing p2))) e2) = mkMF' (nameOf p1) e1 (nameOf p2) e2
          mkMF (Attr (BaseType (SizeBT (SizeLit Nothing p1))) e1, n2 ::: e2) = mkMF' (nameOf p1) e1 n2 e2
          mkMF (n1 ::: e1, (Attr (BaseType (SizeBT (SizeLit Nothing p2))) e2)) = mkMF' n1 e1 (nameOf p2) e2
          mkMF (Attr _ e1, e2) = mkMF (e1, e2)
          mkMF (e1, Attr _ e2) = mkMF (e1, e2)
          mkMF (n1 ::: e1, n2 ::: e2) = mkMF' n1 e1 n2 e2
          mkMF _ = []
          
          mkMF' n1 e1 n2 e2 = let

              (Just sz1) = expSize e1
              (Just sz2) = expSize e2

              tsfm = n1 ++ "2" ++ n2
              startNameAddr = addrName n1
              toNameAddr    = addrName n2
              toName        = structName n2

              outerAddr = addrName oN

              idxExpr
                | isPow2 sz1  = [expr| (elem.as_usize() - base.as_usize()) >> ${e| mkIntExp $ log2 sz1 |} |]
                | otherwise   = [expr| (elem.as_usize() - base.as_usize()) /  ${e| mkIntExp sz1 |} |]
              
              tsfmExpr
                | isPow2 sz1  = [expr| (idx << ${e| mkIntExp $ log2 sz1 |}) |]
                | otherwise   = [expr| (idx * sz1) |]
              idxTsfm =
                [implItem|
                  #[inline(always)]
                  pub fn $thing_from_idx(base: $startNameAddr, idx: usize) -> $startNameAddr {
                    let result = $startNameAddr::from_usize(base.as_usize() + ${e| tsfmExpr |});
                    result
                  }
                |]

              assocItems = [items|
                  pub struct $tsfm
                    { pub fromStart : $startNameAddr
                    , pub toStart   : $toNameAddr
                    }
                |]

              thing_from_idx = fromIdxName n1
              idx_from_thing = toIdxName n1

              innerItems = idxTsfm : snd [implItems|
                      #[inline(always)]
                      pub fn new(fromStart: $startNameAddr, toStart: $toNameAddr) -> $tsfm {
                        $tsfm { fromStart, toStart }
                      }

                      #[inline(always)]
                      pub fn set(&self
                                , fromAddr  : $startNameAddr
                                , value     : $toName) { Self::map_set(self.fromStart, self.toStart, fromAddr, value) }
                      
                      #[inline(always)]
                      pub fn getAddr(&self
                                , fromAddr  : $startNameAddr
                                ) -> $toNameAddr { Self::map_getAddr(self.fromStart, self.toStart, fromAddr) }
                      
                      #[inline(always)]
                      pub fn get(&self
                                , fromAddr  : $startNameAddr
                                ) -> $toName { Self::map_get(self.fromStart, self.toStart, fromAddr) }

                      #[inline(always)]
                      pub fn idx(base : $startNameAddr, elem : $startNameAddr) -> usize {
                        debug_assert!(elem >= base);
                        //LET Sz1 = ${e| mkIntExp sz1 |};
                        // This can be checked on addresses themselves to enforce alignment as-is:
                        //debug_assert!(((elem.as_usize() - base.as_usize()) % sz1) == 0);
                        let result = ${e| idxExpr |};
                        result
                      }

                      #[inline(always)]
                      pub fn map_set( fromStart : $startNameAddr
                                , toStart   : $toNameAddr
                                , fromAddr  : $startNameAddr
                                , value     : $toName) {
                        let idxV = $tsfm::idx(fromStart, fromAddr);
                        toStart.offset::<$toName, $toNameAddr>(idxV).store(value)
                      }
                      
                      #[inline(always)]
                      pub fn map_set_idx(
                                  toStart   : $toNameAddr
                                , idxV      : usize
                                , value     : $toName) {
                        toStart.offset::<$toName, $toNameAddr>(idxV).store(value)
                      }
                      
                      #[inline(always)]
                      pub fn map_get_idx(
                                  toStart   : $toNameAddr
                                , idxV      : usize) -> $toName {
                        toStart.offset::<$toName, $toNameAddr>(idxV).load()
                      }
                      
                      #[inline(always)]
                      pub fn map_getAddr( fromStart : $startNameAddr
                                    , toStart   : $toNameAddr
                                    , fromAddr  : $startNameAddr
                                    ) -> $toNameAddr {
                        let idxV = $tsfm::idx(fromStart, fromAddr);
                        toStart.offset::<$toName, $toNameAddr>(idxV)
                      }
                      
                      #[inline(always)]
                      pub fn map_get( fromStart : $startNameAddr
                                , toStart   : $toNameAddr
                                , fromAddr  : $startNameAddr
                                ) -> $toName {
                        let idxV = $tsfm::idx(fromStart, fromAddr);
                        toStart.offset::<$toName, $toNameAddr>(idxV).load()
                      }
                    |]
                    {- #[inline(always)]
                      let result = 3; //${e|idxExpr|};
                      debug_assert!(((elem.as_usize() - base.as_usize()) % (${e| mkIntExp sz1 |})) == 0);
                      result
                    } -}
              in [ RustImpl tsfm innerItems assocItems ]

        in concatMap mkMF $ D.traceShowId $ allPairs n e

      allPairs :: NameID -> BaseExp -> [(BaseExp, BaseExp)]
      allPairs n e = let
          
          -- Find references to a repetition with the given name,
          -- without crossing shadowed scope boundaries:
          findRep (Prim{})     = []
          findRep (Con _ e2)   = findRep e2
          findRep (e2 :@ _)    = findRep e2
          findRep (e2 :+ e3)   = findRep e2 ++ findRep e3
          findRep (e2 :|| e3)  = findRep e2 ++ findRep e3
          findRep (_ ::: e2)   = findRep e2
          findRep (Exists n2 e2)
            | n == n2   = []
            | otherwise = findRep e2
          findRep e2@(n2 :# e3)
            | n == n2   = e3 : findRep e3
            | otherwise = findRep e3
          findRep (Attr _ e2)  = findRep e2
          
          xs = findRep e
        in  [ (x,y)
            | (x,i) <- zip xs [1..length xs]
            , (y,j) <- zip xs [1..length xs]
            , expSize x /= Nothing
            , expSize y /= Nothing
            , i /= j
            ]

  in  concat $ concatMap (accum findExistBind) bes

