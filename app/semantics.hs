import Data.Maybe (fromJust)
import Text.Pretty.Simple (pPrint)
import System.Environment
import System.IO hiding (putStr, putStrLn, hGetLine)
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (assert)

--import Language.Rust.Syntax (SourceFile(..))
--import Language.Rust.Data.Position (Span(..))
--import Language.Rust.Pretty (pretty')
--
--import Language.Floorplan
--import qualified Language.Floorplan.Parser as P
--import qualified Language.Floorplan.Syntax as S
--import qualified Language.Floorplan.Core.Compiler as CC
--import qualified Language.Floorplan.Core.Syntax as CS

type LayerID = String
type FormalID = String
type FieldID = String
type FlagID = String

ex_code :: Demarc
ex_code = Layer "K" ["n"] (Just $ PrimOp (Just $ LitInt 5) BytesTy) Nothing $
  Seq [ Field "hd" $ Count "n" (Size $ PrimOp (Just $ LitInt 1) BytesTy)
      , Field "tl" $ Count "n" $
        Seq [ Field "lft" $ Size $ PrimOp (Just $ LitInt 1) BytesTy
            , Field "rgt" $ Hash (Size $ PrimOp Nothing BytesTy)
            ]
      ]

data DemarcVal =
    Hash DemarcVal
  | Count FormalID DemarcVal
  | Seq [Demarc]
  | Union [Demarc]
  | Ptr (Either LayerID FieldID)
  | Enum [FlagID]
  | Bits [BitsExp]
  | Size SizeArith
  deriving (Eq, Ord, Show)

type Mag = SizeArith
type Align = SizeArith

data Demarc =
    Field FieldID DemarcVal
  | Layer LayerID [FormalID] (Maybe Mag) (Maybe Align) DemarcVal
  | DV DemarcVal
  deriving (Eq, Ord, Show)

type BitsExp = (FieldID, SizeArith)

data PrimTy = BitsTy | BytesTy | WordsTy | PagesTy
  deriving (Eq, Ord, Show)

data SizeArith =
    PrimOp (Maybe LitArith) PrimTy
  | SizePlus SizeArith SizeArith
  | SizeMinus SizeArith SizeArith
  deriving (Eq, Ord, Show)

data LitArith =
    LitBin Int
  | LitInt Int
  | LitPlus LitArith LitArith
  | LitMinus LitArith LitArith
  | LitTimes LitArith LitArith
  | LitDiv LitArith LitArith
  | LitExponent LitArith LitArith
  deriving (Eq, Ord, Show)

data Exp =
    Plus Exp Exp
  | Or   Exp Exp
  | Prim Int
  | Con Int Exp
  | HasType String Exp
  | Exists String Exp
  | Pound String Exp
  | Align Exp Int
  deriving (Eq, Ord, Show)

data Tree =
    T Tree Tree
  | N String Tree
  | B0 | B1
  deriving (Eq, Ord, Show)

-- Compilation rules. TODO: f_0!!!
cV :: DemarcVal -> Exp
cV (Hash dv) = Exists "f_0" (Pound "f_0" (cV dv))
cV (Count f_0 dv) = Pound f_0 (cV dv)
cV (Seq (d:ds)) = foldl Plus (c d) (map c ds)
cV (Union (d:ds)) = foldl Or (c d) (map c ds)
cV (Ptr id) = Prim 8
cV (Enum fs) = undefined
cV (Bits bes) = undefined
cV (Size sa) = Prim 1 -- TODO: Prim 1!

formals :: [FormalID] -> Exp -> Exp
formals (f:fs) e = Exists f (formals fs e)
formals [] e = e

-- TODO: Con 5! Align error!
c :: Demarc -> Exp
c (Field f dv) = HasType f (cV dv)
c (Layer lid fs Nothing Nothing dv)       = HasType lid $ formals fs $ cV dv
c (Layer lid fs (Just mag) Nothing dv)    = HasType lid $ formals fs $ Con 5 $ cV dv
c (Layer lid fs Nothing (Just aln) dv)    = HasType lid $ formals fs $ Align (cV dv) (error "TODO")
c (Layer lid fs (Just mag) (Just aln) dv) = HasType lid $ formals fs $ Con 5 $ Align (cV dv) (error "TODO")
c (DV dv) = cV dv

d0 = g 0 5 [] (Prim 5)
d1 = addStep ("Choose m=5") $ g 0 5 [] $
  HasType "K" $ Exists "n" $ Con 5 $ Plus
    (HasType "hd" $ Pound "n" $ Prim 1)
    $ HasType "tl" $ Pound "n" $ Plus
        (HasType "lft" (Prim 1))
        (HasType "rgt" (Exists "f_1" $ Pound "f_1" $ Prim 1))

leaves (T t1 t2) = leaves t1 + leaves t2
leaves (N _ t) = leaves t
leaves B0 = 0
leaves B1 = 1

addStep s xs = map (\(r,d) -> (r, s : d)) xs

-- Gamma
g p0 p1 p2 p3 = let
  g' a m theta (Plus e1 e2) =
    let params = \r1 -> (a + leaves r1, m - leaves r1, theta, e2)
    in
      [ (T r1 r2, ("Plus: i = 0.." ++ show m ++ ", Pause (g " ++ show (params r1) ++ ")") : deriv1
          ++ ["Resume (g " ++ show (params r1) ++ ")" ++ ", leaves r1 = " ++ show (leaves r1)] ++ deriv2)
      | (r1, deriv1) <- concat [ addStep ("Plus_r1: Choose i=" ++ show i) $ g a i theta e1 | i <- [0..m]]
      , (r2, deriv2) <- addStep ("Plus_r2: leaves(" ++ show e1 ++ ")=" ++ show (leaves r1)) $ g (a + leaves r1) (m - leaves r1) theta e2
      ]
  g' a m theta (Or _ _) = error "nope"
  g' a m theta (Prim n)
    | m == n    = [(foldl T B0 (take n $ repeat B1), ["Prim: Evaluate " ++ show n ++ "-leaf tree"])]
    | otherwise = []
  g' a m theta (Con n e)
    | m == n    = addStep ("Con: " ++ show n) (g a m theta e)
    | m /= n    = []
  g' a m theta (Align e aln) = undefined
  g' a m theta (HasType l e) =
    [ (N l r, ("HasType: " ++ l) : deriv)
    | (r, deriv) <- g a m theta e
    ]
  g' a m theta (Exists f e) = concat
    [ addStep ("Exists: Choose " ++ f ++ "=" ++ show i)
        $ g a m ((f, i) : theta) e | i <- [0 .. m] ]
  g' a m theta (Pound f e)
    | (Just m) == lookup f theta && m == 0 = [(T B0 B0, ["Pound: m = 0"])]
    | (Just 0) == lookup f theta = []
    | otherwise = let -- Implies lookup theta f > 0
        params = \r1 -> (a + leaves r1, m - leaves r1, (f, (fromJust $ lookup f theta) - 1) : theta, Pound f e)
        in case lookup f theta of
              Nothing -> error "Nothing"
              Just thetaF -> [ (T r1 r2, ("Pound: theta(" ++ f ++ ")=" ++ show thetaF ++ ", Pause (g " ++ show (params r1)) : deriv1
                                  ++ ["Resume (g " ++ show (params r1) ++ ")"] ++ deriv2)
                             | (r1, deriv1) <- concat [addStep ("Pound_r1: Choose i=" ++ show i) $ g a i theta e | i <- [0..m]]
                             , (r2, deriv2) <- addStep ("Pound_r2: leaves(" ++ show e ++ ")=" ++ show (leaves r1)) $
                                                g (a + leaves r1) (m - leaves r1) ((f, (fromJust $ lookup f theta) - 1) : theta) (Pound f e)
                             ]

  addLabel (r, d:ds) = (r, (d ++ "[[g " ++ show (p0, p1, p2, p3) ++ "]] ") : ds)

  in map addLabel (g' p0 p1 p2 p3)

main = do
  args <- getArgs
  case args of
    (flpFile : rest) -> undefined
    _ -> pPrint d1

