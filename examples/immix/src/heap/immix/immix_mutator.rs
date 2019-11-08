use heap::immix;
use heap::immix::ImmixSpace;
use heap::immix::immix_space::ImmixBlock;
use heap::gc;
use objectmodel;

use heap::layout::*;

use std::*;
use std::sync::Arc;
use std::sync::RwLock;
use std::sync::atomic::{AtomicBool, Ordering};

const MAX_MUTATORS : usize = 1024;
lazy_static! {
    pub static ref MUTATORS : RwLock<Vec<Option<Arc<ImmixMutatorGlobal>>>> = {
        let mut ret = Vec::with_capacity(MAX_MUTATORS);
        for _ in 0..MAX_MUTATORS {
            ret.push(None);
        }
        RwLock::new(ret)
    };

    pub static ref N_MUTATORS : RwLock<usize> = RwLock::new(0);
}

#[repr(C)]
pub struct ImmixMutatorLocal {
    id        : usize,

    // use raw pointer here instead of AddressMapTable
    // to avoid indirection in fast path
    alloc_map : RefBitsAddr, //*mut u8,
    space_start: SpaceAddr,
    trace_map : MarkBitsAddr,

    curr_unmarked_state : MarkBits,

    // cursor might be invalid, but Option<RemainderAddr> is expensive here
    // after every GC, we set both cursor and limit
    // to RemainderAddr::zero() so that alloc will branch to slow path
    cursor    : RemainderAddr,
    limit     : LimitAddr,
    line      : usize,

    // globally accessible per-thread fields
    pub global    : Arc<ImmixMutatorGlobal>,

    space     : Arc<ImmixSpace>,
    block     : Option<Box<ImmixBlock>>,
}

pub struct ImmixMutatorGlobal {
    take_yield : AtomicBool,
    still_blocked : AtomicBool
}

impl ImmixMutatorLocal {
    pub fn reset(&mut self) -> () {
        // should not use RemainderAddr::zero() other than initialization
        self.cursor = RemainderAddr::zero();
        self.limit = LimitAddr::zero();

        self.line = immix::LINES_IN_BLOCK;

        self.block = None;

        // This relies on the fact that gc::gc() flips the mark state
        // before this reset() fncn gets called:
        eprintln!("Setting mark state of mutator {} to {}", self.id, objectmodel::get_curr_MARK_STATE().get_MARK_bits());
        self.curr_unmarked_state = objectmodel::get_curr_UNMARKED_STATE();
    }

    pub fn new(space : Arc<ImmixSpace>) -> ImmixMutatorLocal {
        let global = Arc::new(ImmixMutatorGlobal::new());

        let mut id_lock = N_MUTATORS.write().unwrap();
        {
            let mut mutators_lock = MUTATORS.write().unwrap();
            mutators_lock.remove(*id_lock);
            mutators_lock.insert(*id_lock, Some(global.clone()));
        }

        let ret = ImmixMutatorLocal {
            id : *id_lock,
            cursor: RemainderAddr::zero(), limit: LimitAddr::zero(), line: immix::LINES_IN_BLOCK,
            block: None,
            alloc_map: space.alloc_map.toStart,
            trace_map: space.trace_map.toStart,
            space_start: space.start(),
            global: global,
            space: space,
            curr_unmarked_state: objectmodel::get_curr_UNMARKED_STATE()
        };
        *id_lock += 1;

        ret
    }

    pub fn destroy(&mut self) {
        {
            self.return_block();
        }

        let mut mutator_count_lock = N_MUTATORS.write().unwrap();

        let mut mutators_lock = MUTATORS.write().unwrap();
        mutators_lock.push(None);
        mutators_lock.swap_remove(self.id);

        *mutator_count_lock = *mutator_count_lock - 1;

        if cfg!(debug_assertions) {
            println!("destroy mutator. Now live mutators = {}", *mutator_count_lock);
        }
    }

    #[inline(always)]
    pub fn yieldpoint(&mut self) {
        if self.global.take_yield() {
            self.yieldpoint_slow();
        }
    }

    #[inline(never)]
    pub fn yieldpoint_slow(&mut self) {
        trace!("Mutator{}: yieldpoint triggered, slow path", self.id);
        gc::sync_barrier(self);
    }

    #[inline(always)]
    pub fn alloc(&mut self, obj_size: usize, align: usize) -> ObjectAddr {
        // println!("Fastpath allocation");
        // BYTES_IN_CELL_SIZE--> speculatively reserve space for the cell_size. This particular
        // operation needs to be done with .plus() and .sub() because Floorplan doesn't yet
        // support extensive alignment checks and aligned allocations.
        let cursor = self.cursor.plus::<RemainderAddr>(BYTES_IN_CELL_SIZE).align_up::<RemainderAddr>(align).sub::<RemainderAddr>(BYTES_IN_CELL_SIZE);
        let (cell_start, new_cursor) = Block::bump_new_Cell(cursor, obj_size + BYTES_IN_CELL_SIZE);

        // println!("cursor = {:#X}, after align = {:#X}", c, cell_start);

        if !(Block::remainder_is_validly_before_limit(new_cursor, self.limit)) {

            let sz = self.limit.diff(self.cursor);
            if sz > 0 {
                let (free_cell, _) = Block::bump_new_FreeCell(self.cursor, sz);
                free_cell.shadow_alloc_from(sz, vec![ __FLP_IDX_BLOCK, __FLP_IDX_FREECELL ]);
            }

            self.try_alloc_from_local(obj_size, align)
        } else {
//            fill_alignment_gap(self.cursor, cell_start);
            self.cursor = new_cursor;

//            let ref_bits_a : RefBitsAddr = self.get_ref_bits_addr(cell_start);
//            check_expect_pc!(ref_bits_a.as_usize(), vec![__FLP_IDX_REFS]
//                , |_val| {
//                    println!("Was about to successfully allocate CellAddr 0x{:X}", cell_start);
//                    dump_map!();
//                });

            // From freshly allocated BLOCK, or previously freed FREECELL (recycled)
            cell_start.shadow_alloc_from(BYTES_IN_CELL_SIZE + obj_size, vec![ __FLP_IDX_BLOCK, __FLP_IDX_FREECELL ]);
            self.init_obj_size(cell_start, obj_size);

            // Placing this *only* here relies on the fact that alloc_from_{local,global}()
            // always call self.alloc() after fixing-up the blocks to make space:
            self.init_mark_bit(cell_start);
            let obj_addr = cell_start.object();
            obj_addr
        }
    }

    pub fn get_ref_bits_addr(&self, addr: CellAddr) -> RefBitsAddr {
        Word2RefBits::map_getAddr(
            self.space_start.get_first_word(),
            self.alloc_map,
            WordAddr::from_usize(addr.as_usize()))
    }
    /*
    pub fn get_ObjSizeAddr(&self, addr: CellAddr) -> ObjSizeAddr {
        self.get_ref_bits_addr(addr).get_ObjSize_after()
    }*/

    #[inline(always)]
    fn init_mark_bit(&mut self, obj_addr: CellAddr) {
        Word2MarkBits::map_set(
            self.space_start.get_first_word(),
            self.trace_map,
            obj_addr.get_first_word(),
            self.curr_unmarked_state);
    }

    #[inline(always)]
    pub fn init_obj_size(&mut self, addr: CellAddr, size: usize) {
        let cell_size : Cell_sizeAddr = addr.cell_size();
//        for i in 0..BYTES_IN_WORD {
//            check_expect_pc!(cell_size.as_usize() + i, vec![__FLP_IDX_CELL]);
//            unsafe { layout::pc::set_bits(0, (cell_size.as_usize() + i) as *const libc::c_void, __FLP_IDX_CELL_SIZE); } // Mark this memory as containing a Cell_size
//        }
        cell_size.store_word(size);
    }

    #[inline(always)]
    pub fn init_object(&mut self, obj_addr: ObjectAddr, encode: u8) {
//        let cell_addr = CellAddr::from_object(obj_addr);
//        let ref_bits_a = self.get_ref_bits_addr(cell_addr);
//        check_expect_pc!(ref_bits_a.as_usize(), vec![__FLP_IDX_REFS]
//            , |_val| { // Triggers when check_expect fails
//                println!("I was looking at ObjectAddr {}, CellAddr {}", obj_addr, cell_addr);
//                dump_map!();
//            });
//        unsafe { layout::pc::set_bits(0, ref_bits_a.as_usize() as *const libc::c_void, __FLP_IDX_REFBITS); } // Mark this (single byte) as containing a RefBits
        objectmodel::set_ref_byte(self.alloc_map, self.space_start, obj_addr, RefBits::set_REF_from_u8(encode));
        //Word2RefBits::map_set(self.space_start.get_first_word(), self.alloc_map, cell_addr.get_first_word(), RefBits::set_REF_from_u8(encode))
    }

    #[inline(never)]
    pub fn init_object_no_inline(&mut self, addr: ObjectAddr, encode: u8) {
        self.init_object(addr, encode);
    }

    #[inline(never)]
    pub fn try_alloc_from_local(&mut self, size : usize, align: usize) -> ObjectAddr {
        // println!("Trying to allocate from local");

        if self.line < immix::LINES_IN_BLOCK {
            let opt_next_available_line = {
                let cur_line = self.line;
                self.block().get_next_available_line(cur_line)
            };

            match opt_next_available_line {
                Some(next_available_line) => {

                    // we can alloc from local blocks
                    let end_line = self.block().get_next_unavailable_line(next_available_line);
                    //eprintln!("next available line is {}", next_available_line);
                    //eprintln!("next unavailable line is {}", end_line);

                    let block_first_line = self.block().start().get_first_line();
                    // println!("next unavailable line is {}", end_line);
                    self.cursor = Block::cast_line_to_remainder(block_first_line.jump_to_Line(next_available_line));
                    self.limit  = Block::cast_line_to_limit(block_first_line.jump_to_Line(end_line));
                    self.line   = end_line;

                    // println!("{}", self);

                    Block::memset_remainder_until_limit(0, self.cursor, self.limit);

                    for line in next_available_line..end_line {
                        let block = self.block();
                        let table = block.line_mark_table_mut();
//                        let line_addr = table.get_line(line);
                        // Instrumented:
//                        for k in 0..BYTES_IN_LINE {
//                            //eprintln!("k={}, BYTES_IN_LINE={}", k, BYTES_IN_LINE);
//                            check_expect_pc!(line_addr.as_usize() + k, vec![__FLP_IDX_BLOCK, __FLP_IDX_CELL_SIZE, __FLP_IDX_FREECELL, __FLP_IDX_GARBAGE]
//                                , |val| {
//                                    eprintln!("About to freshly allocate line 0x{:X} but byte #{} has type {:?}",
//                                        line_addr,
//                                        k,
//                                        __FLP_TYPES[val as usize]);
//                                    dump_map!();
//                                });
//                        }

                        //debug_assert!(BYTES_IN_LINE == 256);
                        table.set(line, immix::LineMark::FreshAlloc);
                        //debug_assert!(table.get(line) == immix::LineMark::FreshAlloc);
                    }

                    self.alloc(size, align)
                },
                None => {
                    // println!("no available line in current block");
                    self.alloc_from_global(size, align)
                }
            }
        } else {
            // we need to alloc from global space
            self.alloc_from_global(size, align)
        }
    }

    fn alloc_from_global(&mut self, size: usize, align: usize) -> ObjectAddr {
        trace!("Mutator{}: slowpath: alloc_from_global", self.id);

        self.return_block();

        loop {
            // check if yield
            self.yieldpoint();

            let new_block : Option<Box<ImmixBlock>> = self.space.get_next_usable_block();

            match new_block {
                Some(b) => {
                    self.block    = Some(b);
                    self.cursor   = Block::init_remainder_after_cells(self.block().start().cells(), 0);
                    self.limit    = Block::init_limit_after_remainder(self.cursor, 0);
                    self.line     = 0;

                    return self.alloc(size, align);
                },
                None => {continue; }
            }
        }
    }

    pub fn prepare_for_gc(&mut self) {
        self.return_block();
    }

    pub fn id(&self) -> usize {
        self.id
    }

    fn return_block(&mut self) {
        if self.block.is_some() {
            self.space.return_used_block(self.block.take().unwrap());
            debug_assert_eq!(self.block.is_none(), true);
        }
    }
    fn block(&mut self) -> &mut ImmixBlock {
        self.block.as_mut().unwrap()
    }

    pub fn print_object(&self, obj: ObjectAddr, length: usize) {
        ImmixMutatorLocal::print_object_static(obj, length);
    }

    pub fn print_object_static(obj: ObjectAddr, length: usize) {
        println!("===Object {:#X} size: {} bytes===", obj, length);
        let mut cur_addr = obj;
        let mx = obj.plus::<ObjectAddr>(length);
        while cur_addr < mx {
            println!("Address: {:#X}   {:#X}", cur_addr, cur_addr.ptr_0().load_ObjectAddr());
            cur_addr = cur_addr.plus::<ObjectAddr>(BYTES_IN_WORD); //skip_bytes_to_Cell(BYTES_IN_WORD);
        }
        println!("----");
        println!("=========");
    }
}

impl ImmixMutatorGlobal {
    pub fn new() -> ImmixMutatorGlobal {
        ImmixMutatorGlobal {
            take_yield: AtomicBool::new(false),
            still_blocked: AtomicBool::new(false)
        }
    }

    #[inline(always)]
    pub fn is_still_blocked(&self) -> bool {
        self.still_blocked.load(Ordering::SeqCst)
    }
    pub fn set_still_blocked(&self, b : bool) {
        self.still_blocked.store(b, Ordering::SeqCst);
    }

    pub fn set_take_yield(&self, b : bool) {
        self.take_yield.store(b, Ordering::SeqCst);
    }
    #[inline(always)]
    pub fn take_yield(&self) -> bool{
        self.take_yield.load(Ordering::SeqCst)
    }
}

impl fmt::Display for ImmixMutatorLocal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.cursor.is_zero() {
            write!(f, "Mutator (not initialized)")
        } else {
            write!(f, "Mutator:\n").unwrap();
            write!(f, "cursor= {:#X}\n", self.cursor).unwrap();
            write!(f, "limit = {:#X}\n", self.limit).unwrap();
            write!(f, "line  = {}\n", self.line).unwrap();
            write!(f, "block = {}", self.block.as_ref().unwrap())
        }
    }
}
