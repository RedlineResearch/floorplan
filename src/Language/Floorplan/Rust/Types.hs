module Language.Floorplan.Rust.Types
  where
import Language.Rust.Parser as P
import Language.Rust.Syntax as R
import Language.Rust.Quote
import Language.Rust.Data.Ident
import Language.Rust.Data.Position

import Language.Floorplan.Core.Syntax

-- | Stripped-down Rust Impl item with just the pieces we
--   care about (name and item entries) so that we can merge lists of impls
--   together by NameID equality.
data RustItem =
    RustImpl
      NameID          -- ^ e.g. "CellAddr"
      [ImplItem Span] -- ^ Contents of the Impl
      [Item     Span] -- ^ Associated Items that go *outside* the `impl`
  | TopLevel [Item Span]
  deriving (Show)

rustItemComparator (RustImpl n _ _) = n
rustItemComparator (TopLevel _) = "______________________FLP__" -- TODO

