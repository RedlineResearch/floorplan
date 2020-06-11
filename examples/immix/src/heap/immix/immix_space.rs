use heap::immix;
use heap::gc;

use heap::layout::*;

extern crate std;
extern crate memmap;
extern crate libc;

use std::*;
use std::collections::LinkedList;
use std::sync::Mutex;
use std::sync::Arc;
use self::memmap::MmapOptions;

// this table will be accessed through unsafe raw pointers. since Rust doesn't provide a data structure for such guarantees:
// 1. Non-overlapping segments of this table may be accessed parallelly from different mutator threads
// 2. One element may be written into at the same time by different gc threads during tracing

#[derive(Clone)]
pub struct LineMarkTable {
    first_line  : LineAddr,              // e.g. first line of the entire space (space_start)
    ptr         : *mut immix::LineMark,
    len         : usize,
}

impl LineMarkTable {
    pub fn new(space_start: SpaceAddr, space_end: SpaceAddrEnd) -> LineMarkTable {
        let line_mark_table_len = space_start.size_of(space_end) >> immix::LOG_BYTES_IN_LINE;
        let line_mark_table = {
            let ret = unsafe {libc::malloc((mem::size_of::<immix::LineMark>() * line_mark_table_len) as libc::size_t)} as *mut immix::LineMark;

            for idx in 0..line_mark_table_len {
                Self::mark_me_idx(ret, idx, immix::LineMark::Free as u8);
            }

            ret
        };

        LineMarkTable{first_line: space_start.get_first_line(), ptr: line_mark_table, len: line_mark_table_len}
    }

    pub fn take_slice(&mut self, start_idx: usize, len: usize) -> LineMarkTable {
        LineMarkTable
            { ptr: LineMarkAddr::from_ptr(self.ptr).jump_to_LineMark(start_idx).to_ptr_mut::<immix::LineMark>()
            , first_line : Line2LineMark::line_from_idx(self.first_line, start_idx)
            , len: len 
            }
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> immix::LineMark { //u8 { //immix::LineMark {
        debug_assert!(index <= self.len);
        //Line2LineMark::map_get_idx(LineMarkAddr::from_ptr(self.ptr), index).to_enum()
        let idx = Line2LineMark::map_get_idx(LineMarkAddr::from_ptr(self.ptr), index);
        immix::LineMark::from(LineMark::to_enum(&idx))
    }
    #[inline(always)]
    pub fn get_line(&self, index: usize) -> LineAddr {
        debug_assert!(index <= self.len);
        Line2LineMark::line_from_idx(self.first_line, index)
//        immix::LineMark::from(LineMark::to_enum(&Line2LineMark::map_get_idx(LineMarkAddr::from_ptr(self.ptr), index)))
    }

    #[inline(always)]
    pub fn set(&mut self, idx: usize, value: immix::LineMark) {
        debug_assert!(idx <= self.len);
        Line2LineMark::map_set_idx( LineMarkAddr::from_ptr(self.ptr)
                                  , idx
                                  , LineMark::from_enum(value as u8));
    }
    
    #[inline(always)]
    fn mark_me_idx(base_ptr : *mut immix::LineMark, idx : usize, val : u8) {
        Line2LineMark::map_set_idx( LineMarkAddr::from_ptr(base_ptr)
                                  , idx
                                  , LineMark::from_enum(val))
    }
    
    #[inline(always)]
    fn mark_me(line_start: LineAddr, base_ptr : *mut immix::LineMark, addr : CellAddr, val : u8) -> usize {
        let idx = Line2LineMark::idx(line_start, Block::cast_cell_to_line(addr));
        Line2LineMark::map_set_idx( LineMarkAddr::from_ptr(base_ptr), idx, LineMark::from_enum(val));
        idx
//
//            line_start
//                                  , LineMarkAddr::from_ptr(base_ptr)
//                                  , Block::cast_cell_to_line(addr)
//                                  , LineMark::from_enum(val));
    }

    #[inline(always)]
    pub fn mark_line_live(&self, obj_addr: ObjectAddr, _alloc_map: RefBitsAddr, _immix_start: SpaceAddr) {
        let cell_addr = CellAddr::from_object(obj_addr);
        //let idx = Line2LineMark::idx(line_start, Block::cast_cell_to_line(addr));
        let idx = Self::mark_me(self.first_line, self.ptr, cell_addr, immix::LineMark::Live as u8);
        if idx < self.len - 1 {
            Self::mark_me_idx(self.ptr, idx + 1, immix::LineMark::ConservLive as u8);
        }
    }

    #[inline(always)]
    pub fn mark_line_live2(&self, line_start: LineAddr, obj: ObjectAddr) {
        //for i in 0..BYTES_IN_LINE {
        //    check_expect_pc!(obj.as_usize() + i, vec![__FLP_IDX_LLNODE, __FLP_IDX_QNODE, __FLP_IDX_DEQUEUE, __FLP_IDX_GARBAGE]);
        //}
        let cell_addr = CellAddr::from_object(obj);
        let idx = Self::mark_me(line_start, self.ptr, cell_addr, immix::LineMark::Live as u8);
        if idx < self.len - 1 {
            Self::mark_me_idx(self.ptr, idx + 1, immix::LineMark::ConservLive as u8);
        }
    }
    
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }
    
    pub fn dump(&self) {
        for i in 0..self.len {
            eprintln!("0x{:X} = {:?}", self.get_line(i), self.get(i));
        }
    }
}

#[repr(C)]
pub struct ImmixSpace {
    start            : SpaceAddr,
    end              : SpaceAddrEnd,

    // these maps are writable at allocation, read-only at collection
    pub alloc_map        : Arc<Word2RefBits>, //AddressMap<SpaceAddr, RefBits>>,

    // these maps are only for collection
    pub trace_map        : Arc<Word2MarkBits>, //AddressMap<SpaceAddr, MarkBits>>,

    // this table will be accessed through unsafe raw pointers. since Rust doesn't provide a data structure for such guarantees:
    // 1. Non-overlapping segments of this table may be accessed parallelly from different mutator threads
    // 2. One element may be written into at the same time by different gc threads during tracing
    pub line_mark_table : LineMarkTable,

    total_blocks     : usize,  // for debug use

    #[allow(dead_code)]
    mmap             : memmap::MmapMut,
    usable_blocks    : Mutex<LinkedList<Box<ImmixBlock>>>,
    used_blocks      : Mutex<LinkedList<Box<ImmixBlock>>>,
}

pub struct ImmixBlock {
    id    : usize,
    state : immix::BlockMark,
    start : BlockAddr,

    // a segment of the big line mark table in ImmixSpace
    line_mark_table_slice : LineMarkTable
}

const SPACE_ALIGN : usize = 1 << 19;

impl ImmixSpace {
    pub fn new(space_size : usize) -> ImmixSpace {
        // acquire memory through mmap
        let trace_map_size = mem::size_of::<MarkBits>() * (space_size >> LOG_BYTES_IN_WORD);
        let alloc_map_size = mem::size_of::<RefBits>()  * (space_size >> LOG_BYTES_IN_WORD);
        let line_mark_table_size = mem::size_of::<LineMark>() * (space_size >> LOG_BYTES_IN_LINE);
        let total = SPACE_ALIGN + space_size + line_mark_table_size + trace_map_size + alloc_map_size;
        
        let anon_mmap : memmap::MmapMut = match MmapOptions::new().len(total).map_anon() {
        //let anon_mmap : memmap::Mmap = match memmap::Mmap::anonymous(total, memmap::Protection::ReadWrite) {
            Ok(m) => m,
            Err(_) => panic!("failed to call mmap"),
        };
        let mptr = anon_mmap.as_ptr();
//        unsafe {
//            layout::pc::permcheck_init_maps(1, ptr::null());
//            layout::pc::set_bits(0, mptr as *const libc::c_void, 0b11110011);
//            debug_assert!(layout::pc::get_bits(0, mptr as *const libc::c_void, 0) == 0b11110011);
//            //println!("get_bits: {}", layout::pc::get_bits(0, mptr as *const libc::c_void, 0));
//            layout::pc::set_bits(0, mptr as *const libc::c_void, 0b00000000);
//        }
        let start : RegionAddr = RegionAddr::from_ptr::<u8>(mptr).align_up(SPACE_ALIGN);
        start.shadow_alloc_from(total - SPACE_ALIGN, vec![ __FLP_IDX_UNMAPPED ]);

        // space, line marks, reference map, mark bits map
        println!("Before init_canonical()");
        let (space, lms, refs, mks, _region_end)
            = start.init_canonical_sequence(space_size, line_mark_table_size, trace_map_size, alloc_map_size);
        println!("After init_canonical()");
        let space_end = lms.to_SpaceAddrEnd();
        
        space.shadow_alloc_from_region(space_size);
        lms.shadow_alloc_from_region(line_mark_table_size);
        refs.shadow_alloc_from_region(trace_map_size);
        mks.shadow_alloc_from_region(alloc_map_size);

        let line_mark_table = Line2LineMark::new(start.get_first_line(), lms.get_first_lineMark());
        let trace_map       = Word2MarkBits::new(start.get_first_word(), mks.get_first_markBits());
        let alloc_map       = Word2RefBits::new( start.get_first_word(), refs.get_first_refBits());

        let line_mark_table_len = line_mark_table_size;
        for idx in 0..line_mark_table_len {
            line_mark_table.set(start.get_first_line().jump_to_Line(idx), LineMark::from_enum(immix::LineMark::Free as u8));
        }

        let mut ret = ImmixSpace {
            start: space,
            end: space_end,
            mmap: anon_mmap,

            line_mark_table: LineMarkTable { first_line: space.get_first_line(), ptr: line_mark_table.toStart.to_ptr_mut(), len: line_mark_table_len },
            trace_map: Arc::new(trace_map),
            alloc_map: Arc::new(alloc_map),
            usable_blocks: Mutex::new(LinkedList::new()),
            used_blocks: Mutex::new(LinkedList::new()),
            total_blocks: 0
        };

        ret.init_blocks();

        ret
    }

    fn init_blocks(&mut self) -> () {
        let mut id = 0;
        let mut block_start : BlockAddr = self.start.get_first_block();
        let mut line = 0;

        let mut usable_blocks_lock = self.usable_blocks.lock().unwrap();

        loop {
            usable_blocks_lock.push_back(Box::new(ImmixBlock {
                id : id,
                state: immix::BlockMark::Usable,
                start: block_start,
                line_mark_table_slice: self.line_mark_table.take_slice(line, immix::LINES_IN_BLOCK)
            }));

            block_start.shadow_alloc_from(vec![ __FLP_IDX_SPACE ]);
            //eprintln!("Initialized block #{} starting at {} of size {}", id, block_start, BYTES_IN_BLOCK);

            id += 1;
            block_start = block_start.jump_to_Block(1);
            line += immix::LINES_IN_BLOCK;

            if !(self.start.contains_block(block_start, self.end)) { break }
        }
        eprintln!("Initialized {} blocks", id);

        self.total_blocks = id;
    }

    pub fn return_used_block(&self, old : Box<ImmixBlock>) {
        // Unsafe and raw pointers are used to transfer ImmixBlock to/from each Mutator.
        // This avoids explicit ownership transferring
        // If we explicitly transfer ownership, the function needs to own the Mutator in order to move the ImmixBlock out of it (see ImmixMutatorLocal.alloc_from_global()),
        // and this will result in passing the Mutator object as value (instead of a borrowed reference) all the way in the allocation
        self.used_blocks.lock().unwrap().push_front(old);
    }

    #[allow(unreachable_code)]
    pub fn get_next_usable_block(&self) -> Option<Box<ImmixBlock>> {
        let res_new_block : Option<Box<ImmixBlock>> = {
            self.usable_blocks.lock().unwrap().pop_front()
        };
        if res_new_block.is_none() {
            // should unlock, and call GC here
            gc::trigger_gc();

            None
        } else {
            res_new_block
        }    	
    }

    #[allow(unused_variables)]
    pub fn sweep(&self) {
        let mut free_lines = 0;
        let mut usable_blocks = 0;
        let mut full_blocks = 0;

        let mut used_blocks_lock = self.used_blocks.lock().unwrap();
        let mut usable_blocks_lock = self.usable_blocks.lock().unwrap();

        let mut live_blocks : LinkedList<Box<ImmixBlock>> = LinkedList::new();

        while !used_blocks_lock.is_empty() {
            let mut block = used_blocks_lock.pop_front().unwrap();

            let mut has_free_lines = false;

            {
                let cur_line_mark_table = block.line_mark_table_mut();
                for i in 0..cur_line_mark_table.len() {
                    let line_mark = cur_line_mark_table.get(i);
                    let line_addr : LineAddr = cur_line_mark_table.get_line(i);
                    //eprintln!("Line 0x{:X} is {:?}", line_addr, line_mark);
                    if line_mark != immix::LineMark::Live && line_mark != immix::LineMark::ConservLive {
                        has_free_lines = true;
                        
                        // ----------------------------------
//                        let space_start : SpaceAddr = self.start;
//                        let alloc_map : RefBitsAddr = self.alloc_map.toStart;
//                        for j in 0..BYTES_IN_LINE {
//                            let ref_bits_a : RefBitsAddr =
//                                Word2RefBits::map_getAddr(
//                                      space_start.get_first_word()
//                                    , alloc_map
//                                    , WordAddr::from_usize(line_addr.as_usize() + j));
//                            check_expect_pc!(line_addr.as_usize() + j, vec![__FLP_IDX_CELL_SIZE, __FLP_IDX_FREECELL, __FLP_IDX_GARBAGE, __FLP_IDX_CELL]
//                                , |_val| {
//                                    eprintln!("I marked line 0x{:X} as free, but byte #{} is acting up.", line_addr.as_usize(), j);
//                                    eprintln!("RefBits for that byte is at 0x{:X}.", ref_bits_a.as_usize());
//                                    eprintln!("RefBits is set to: 0b{:08b}", ref_bits_a.load_RefBits().get_REF_bits());
//                                    cur_line_mark_table.dump();
//                                    dump_map!();
//                                });
//                            unsafe { layout::pc::set_bits(0, (line_addr.as_usize() + j) as *const libc::c_void, __FLP_IDX_FREECELL); }
//                            
//                            check_expect_pc!(ref_bits_a.as_usize(), vec![__FLP_IDX_REFS, __FLP_IDX_REFBITS]);
//                            unsafe { layout::pc::set_bits(0, ref_bits_a.as_usize() as *const libc::c_void, __FLP_IDX_REFS); }
//
//                            // null-out so it doesn't get re-scanned (this was a memory leak /
//                            // bug in the original version).
//                            ref_bits_a.store_RefBits(RefBits::set_REF_from_u8(0b00000000));
//                        }
                        // ----------------------------------
                        
                        cur_line_mark_table.set(i, immix::LineMark::Free);

                        free_lines += 1;
                    } else {
                        // THIS WAS A BUG: we need to reset "Live" and "ConservLive" entries of
                        // the line-mark table to appear as if those lines were /still/ freshly
                        // allocated (FreshAlloc) so that the next GC's mark-sweep knows that
                        // they contain hold-over objects (assuming they aren't killed by the
                        // mutator).
                        cur_line_mark_table.set(i, immix::LineMark::FreshAlloc);
                    }
                }

                // release the mutable borrow of 'block'
            }

            if has_free_lines {
                block.set_state(immix::BlockMark::Usable);
                usable_blocks += 1;

                usable_blocks_lock.push_front(block);
            } else {
                block.set_state(immix::BlockMark::Full);
                full_blocks += 1;
                live_blocks.push_front(block);
            }
        }

        used_blocks_lock.append(&mut live_blocks);

        if cfg!(debug_assertions) {
            println!("free lines    = {} of {} total", free_lines, self.total_blocks * immix::LINES_IN_BLOCK);
            println!("usable blocks = {}", usable_blocks);
            println!("full blocks   = {}", full_blocks);
        }

        if full_blocks == self.total_blocks {
            println!("Out of memory in Immix Space");
            std::process::exit(1);
        }

        debug_assert!(full_blocks + usable_blocks == self.total_blocks);
    }

    pub fn start(&self) -> SpaceAddr {
        self.start
    }
    pub fn end(&self) -> SpaceAddrEnd {
        self.end
    }

    pub fn line_mark_table(&self) -> &LineMarkTable {
        &self.line_mark_table
    }
}

impl ImmixBlock {
    pub fn get_next_available_line(&self, cur_line : usize) -> Option<usize> {
        let mut i = cur_line;
        while i < self.line_mark_table_slice.len {
            match self.line_mark_table_slice.get(i) {
                immix::LineMark::Free => {
//                    let line_addr : LineAddr = self.line_mark_table_slice.get_line(i);
//                    //eprintln!("Got Free Line 0x{:X}", line_addr);
//                    for k in 0..BYTES_IN_LINE {
//                        check_expect_pc!(line_addr.as_usize() + k, vec![__FLP_IDX_BLOCK, __FLP_IDX_FREECELL, __FLP_IDX_GARBAGE, __FLP_IDX_CELL_SIZE]
//                            , |_val| {
//                                eprintln!("Byte {} in line 0x{:X} was {:?} when it should be something free-like.", k, line_addr, __FLP_TYPES[_val as usize]);
//                                dump_map!();
//                            });
//                    }
                    return Some(i);
                },
                _ => {i += 1;},
            }
        }
        None
    }

    pub fn get_next_unavailable_line(&self, cur_line : usize) -> usize {
        let mut i = cur_line;
        while i < self.line_mark_table_slice.len {
            match self.line_mark_table_slice.get(i) {
                immix::LineMark::Free => {
//                    let line_addr : LineAddr = self.line_mark_table_slice.get_line(i);
//                    for k in 0..BYTES_IN_LINE {
//                        check_expect_pc!(line_addr.as_usize() + k, vec![__FLP_IDX_BLOCK, __FLP_IDX_FREECELL, __FLP_IDX_GARBAGE, __FLP_IDX_CELL_SIZE]
//                            , |_val| {
//                                eprintln!("Byte {} in line 0x{:X} was {:?} when it should be something free-like.", k, line_addr, __FLP_TYPES[_val as usize]);
//                                dump_map!();
//                            });
//                    }
                    i += 1;
                }
                _ => {
                    return i;
                },
            }
        }
        i
    }

    pub fn id(&self) -> usize {
        self.id
    }
    pub fn start(&self) -> BlockAddr {
        self.start
    }
    pub fn set_state(&mut self, mark: immix::BlockMark) {
        self.state = mark;
    }
    #[inline(always)]
    pub fn line_mark_table(&self) -> &LineMarkTable {
        &self.line_mark_table_slice
    }
    #[inline(always)]
    pub fn line_mark_table_mut(&mut self) -> &mut LineMarkTable {
        &mut self.line_mark_table_slice
    }
}

/// Using raw pointers forbid the struct being shared between threads
/// we ensure the raw pointers won't be an issue, so we allow Sync/Send on ImmixBlock
unsafe impl Sync for ImmixBlock {}
unsafe impl Send for ImmixBlock {}
unsafe impl Sync for ImmixSpace {}
unsafe impl Send for ImmixSpace {}

impl fmt::Display for ImmixSpace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ImmixSpace\n").unwrap();
        write!(f, "range={:#X} ~ {:#X}\n", self.start, self.end).unwrap();

        // print table by vec
//        write!(f, "table={{\n").unwrap();
//        for i in 0..self.line_mark_table_len {
//            write!(f, "({})", i).unwrap();
//            write!(f, "{:?},", unsafe{*self.line_mark_table.offset(i as isize)}).unwrap();
//            if i % immix::BYTES_IN_LINE == immix::BYTES_IN_LINE - 1 {
//                write!(f, "\n").unwrap();
//            }
//        }
//        write!(f, "\n}}\n").unwrap();


        write!(f, "t_ptr={:?}\n", self.line_mark_table.ptr).unwrap();
//        write!(f, "usable blocks:\n").unwrap();
//        for b in self.usable_blocks.iter() {
//            write!(f, "  {}\n", b).unwrap();
//        }
//        write!(f, "used blocks:\n").unwrap();
//        for b in self.used_blocks.iter() {
//            write!(f, "  {}\n", b).unwrap();
//        }
        write!(f, "done\n")
    }
}

impl fmt::Display for ImmixBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ImmixBlock#{}(state={:?}, address={:#X}, line_table={:?}", self.id, self.state, self.start, self.line_mark_table_slice.ptr).unwrap();

        write!(f, "[").unwrap();
        for i in 0..immix::LINES_IN_BLOCK {
            write!(f, "{:?},", self.line_mark_table_slice.get(i)).unwrap();
        }
        write!(f, "]")
    }
}
