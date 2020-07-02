{-# LANGUAGE QuasiQuotes, TemplateHaskell, BangPatterns #-}
module Language.Floorplan.C.Types where

import Language.Floorplan.Syntax -- e.g. BoolExpr(..)
import Language.Floorplan.Core.Syntax
import qualified Data.Map.Lazy as M
import Data.List (sortBy, nub, inits, intersperse)

import Control.Monad.State.Lazy

type MapID = Int
type TypeID = String
type AddressExp = MacroExp
type SizeExp = MacroExp

-- | 'C' appendices indicate "calculation"
data MacroExp =
    ExpID   String
  | QExpID  [String]               -- Specialized ExpID where listed names are concatenated together with '_' inbetween.
  | PlusC   MacroExp MacroExp      -- Addition
  | MinusC  MacroExp MacroExp      -- Subtraction
  | IntC    Int                    -- Integer calculation
  | CastC   String MacroExp        -- Casting
  | TernaryIf BoolExpr MacroExp MacroExp -- Ternary if expression.
  | ExpNoArg -- "Empty" expression (literally zero characters) for use in calls to macros
  deriving (Eq, Ord, Show)

data MacroStmt =
    IfM       BoolExpr [MacroStmt]
  | ElifM     BoolExpr [MacroStmt]
  | PSet MapID AddressExp TypeID SizeExp
  | CallM     String   [MacroExp] -- Call another function or macro.
  | NameListM [String] -- A comma-separated list of names, without any surrounding brackets
  | BlockStmt [MacroStmt]
  | PlusEq    String MacroExp -- s += exp;
  | InitStmt  String String (Maybe MacroExp) -- e.g., 'unsigned int x = 0;' or just 'unsigned int x;'
  deriving (Eq, Ord, Show)

data MacroBody =
    StmtsBody [MacroStmt]
  | ExpBody   Bool MacroExp -- Bool is whether or not has enclosing parentheses
  deriving (Eq, Ord, Show)

data MacroDef = MacroDef String [String] MacroBody
  deriving (Eq, Ord, Show)

printMacros :: [MacroDef] -> String
printMacros ms = concat $ intersperse "\n" $ map printMacro ms

printMacro (MacroDef name [] body)
  = "#define " ++ name ++ " " ++ printBody 2 body
printMacro (MacroDef name args body)
  = "#define " ++ name ++ "(" ++ concat (intersperse "," args) ++ ") " ++ printBody 2 body

printBody :: Int -> MacroBody -> String
printBody c (StmtsBody []) = ""
printBody c (StmtsBody [s@(BlockStmt _)]) = " \\\n" ++ printStmt c s -- Single block stmt indentation better when on a separate line
printBody c (StmtsBody [stmt]) = printStmt c stmt
printBody c (StmtsBody stmts) =
  " \\\n" ++ (concat $ intersperse " \\\n" $ map (printTabbed printStmt c) stmts)
printBody c (ExpBody True e) = "(" ++ printExp e ++ ")"
printBody c (ExpBody False e) = printExp e

printTabbed :: (Int -> a -> String) -> Int -> (a -> String)
printTabbed fncn cnt = ((replicate cnt ' ') ++) . fncn cnt

printTabbedS :: Int -> (String -> String)
printTabbedS cnt = ((replicate cnt ' ') ++)

printStmt :: Int -> MacroStmt -> String
printStmt c (IfM be ms) =
  "if (" ++ printBoolExpr be ++ ") { \\\n" ++ concat (intersperse " \\\n" $ map (printTabbed printStmt $ c+2) ms) ++ " }"
printStmt c (ElifM be ms) = "else if (" ++ printBoolExpr be ++ ") { \\\n" ++ concat (intersperse " \\\n" $ map (printTabbed printStmt $ c+2) ms) ++ "  }"
printStmt c (PSet mid addr typ sz) = "PSet(" ++ show mid ++ ", " ++ printExp addr ++ ", " ++ typ ++ ", " ++ printExp sz ++ ");"
printStmt c (CallM fncn args) = fncn ++ "(" ++ concat (intersperse "," $ map printExp args) ++ ");"
printStmt c (NameListM ns) = concat $ intersperse ", " ns
printStmt c (BlockStmt []) = error $ "Empty BlockStmt is suspect."
printStmt c (BlockStmt [stmt]) = printTabbedS c "{ " ++ printStmt c stmt ++ " }"
printStmt c (BlockStmt (s:ss))  = printTabbedS c "{ " ++ printStmt c s ++ " \\\n" ++ concat (intersperse " \\\n" $ map (printTabbed printStmt $ c+2) ss) ++ printTabbedS c "}"
printStmt c (PlusEq lhs rhs) = printTabbedS c $ lhs ++ " += " ++ printExp rhs ++ ";"
printStmt c (InitStmt typ varID Nothing) = printTabbedS c $ typ ++ " " ++ varID ++ ";"
printStmt c (InitStmt typ varID (Just rhs)) = printTabbedS c $ typ ++ " " ++ varID ++ " = " ++ printExp rhs ++ ";"

printExp :: MacroExp -> String
printExp (QExpID []) = error "Empty QExpID found"
printExp (QExpID xs) = concat $ intersperse "_" $ reverse xs
printExp (ExpID s) = s
printExp (PlusC e1 e2) = printExpSafe e1 ++ " + " ++ printExpSafe e2
printExp (MinusC e1 e2) = printExpSafe e1 ++ " - " ++ printExpSafe e2
printExp (IntC i) = show i
printExp (CastC typ e) = "(" ++ typ ++ ")" ++ printExpSafe e
printExp (TernaryIf bool_e e1 e2) = "(" ++ printBoolExpr bool_e ++ ") ? " ++ printExpSafe e1 ++ " : " ++ printExpSafe e2
printExp ExpNoArg = ""

-- Safely parenthesize things.
printExpSafe (ExpID s) = s
printExpSafe (IntC i) = show i
printExpSafe other = "(" ++ printExp other ++ ")"

macro_name (MacroDef n _ _) = n
macro_args (MacroDef _ a _) = a
macro_body (MacroDef _ _ b) = b

data LabelEntry = LabelEntry -- Attribute for labels in the FLPS.Exp
  { is_explicit_ts' :: Bool
  , explicit_bools' :: [(Int, BoolExpr)]
  , attr_sz'        :: Maybe Int
  , distinct_sizes' :: [Maybe Int]
  , num_free_fills' :: Int
  , sequences'      :: [(Maybe NameID, BaseExp)]
  , subtypes'       :: [([NameID], BaseExp)]   -- Names that can be found as subexpressions of the expression.
  , base_ref_params' :: [String]          -- parameters to the 'base_transition_...' macro of the expression.
  , is_filtered_out' :: Bool -- Is the qualified name associated with this entry filtered out by one of the regular expressions.
  , exp_size'        :: Maybe Int
  , exp_sizes'       :: [Maybe Int]
  , trans_params'    :: [String]    -- The parameters to the base transition function for the associated name/expression
  , alt_sequences'   :: [ (Maybe NameID, [ (Maybe NameID, BaseExp) ]) ]
  , fixed_size'      :: Bool -- Same as exp_size' is not Nothing.
  , requires_size_arg' :: Bool -- Same as ((not is_explicit_ts) && expSize == Nothing), or equivalently ("size_in_bytes" `elem` (name `trans_params` mp))
  , size_calc' :: Maybe MacroExp -- Expression computing the size consumed by a call to `base_transition_...` for this label/type.
  }
  deriving (Eq, Ord, Show)

type LabelMap = M.Map NameID LabelEntry

le_lookup :: (LabelEntry -> a) -> NameID -> LabelMap -> a
le_lookup fncn n mp =
  case n `M.lookup` mp of
    Nothing -> error $ "Fatal Error: Name '" ++ n ++ "' was not found in the label entry map."
    Just r -> fncn r

is_explicit_ts    = le_lookup is_explicit_ts'
explicit_bools    = le_lookup explicit_bools'
attr_sz           = le_lookup attr_sz'
distinct_sizes    = le_lookup distinct_sizes'
num_free_fills    = le_lookup num_free_fills'
sequences         = le_lookup sequences'
subtypes          = le_lookup subtypes'
base_ref_params   = le_lookup base_ref_params'
is_filtered_out   = le_lookup is_filtered_out'
exp_size          = le_lookup exp_size'
exp_sizes         = le_lookup exp_sizes'
trans_params      = le_lookup trans_params'
alt_sequences     = le_lookup alt_sequences'
fixed_size        = le_lookup fixed_size'
requires_size_arg = le_lookup requires_size_arg'
size_calc         = le_lookup size_calc'

data CompilerEnv = CompilerEnv
  { gen_counter :: Int -- For generating new names
  }

-- Get the generator counter as a String and increment at the same time.
newName :: String -> State CompilerEnv String
newName s = do
  i <- gen_counter <$> get
  put $ CompilerEnv (i + 1)
  return $ s ++ show i
