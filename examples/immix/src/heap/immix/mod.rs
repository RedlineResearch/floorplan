mod immix_space;
mod immix_mutator;

pub use self::immix_space::ImmixSpace;
pub use self::immix_mutator::ImmixMutatorLocal;
pub use self::immix_mutator::ImmixMutatorGlobal;
pub use self::immix_space::LineMarkTable as ImmixLineMarkTable;
pub use self::immix_mutator::MUTATORS;
pub use self::immix_mutator::N_MUTATORS;

pub use heap::layout::*;

use std::sync::Arc;
use std::sync::RwLock;

lazy_static!{
    pub static ref SHARED_SPACE : Option<Arc<RwLock<ImmixSpace>>> = None;
}

pub const BYTES_IN_LINE      : usize = (1 << LOG_BYTES_IN_LINE);
pub const BYTES_IN_BLOCK     : usize = (1 << LOG_BYTES_IN_BLOCK); 
pub const LINES_IN_BLOCK     : usize = (1 << (LOG_BYTES_IN_BLOCK - LOG_BYTES_IN_LINE));

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum LineMark {
    Free = 4,        // Marked upon initialization and during sweep upon finding non-Live / non-ConservLive line.
    Live = 3,        // Marked as such upon allocation
    FreshAlloc = 2,  // try_alloc_from_local() found an "available" line and allocated it
    ConservLive = 1  // Next line of an allocation marked as such 
}

impl From<u8> for LineMark {
    fn from(val : u8) -> LineMark {
        if val >= 5 || val == 0 {
            eprintln!("Invalid LineMark::from({})", val);
            LineMark::ConservLive
        }
        // Duplicate ConservLive below is on purpose:
        else { [LineMark::ConservLive, LineMark::ConservLive, LineMark::FreshAlloc, LineMark::Live, LineMark::Free][val as usize] }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum BlockMark {
    Usable,
    Full
}
