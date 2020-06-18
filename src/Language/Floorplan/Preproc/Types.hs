module Language.Floorplan.Preproc.Types where
import Language.Floorplan.Core.Syntax hiding (accum)
import Language.Floorplan.Syntax
import qualified Data.Map.Strict as M
import Data.List (sort)
import Data.Maybe (fromJust, isJust)
import Debug.Trace as D

data PreprocError =
    Recursive [String]                  -- ^ String is a grafting ID that's part of a recursive path.
  | UndefinedSymbol String              -- ^ String is the symbol that's undefined.
  | DuplicateAttribute [ScopeAttribute] -- ^ The list of attributes contains unnecessary duplicates.
  | UnbalancedScope Int                 -- ^ %begin / %end are unbalanced.
  | RegexError String                   -- ^ A regex was unable to be compiled.
  | InvalidAttribute String             -- ^ String is an error message.
  | DuplicateTransitionNum String Int   -- ^ Must reference unique alternatives in a transition union case
  | IdenticalBools String BoolExpr BoolExpr
  deriving (Eq, Ord, Show)

type Transition = (String, [(Int, BoolExpr)])
type Transitions = [Transition]
