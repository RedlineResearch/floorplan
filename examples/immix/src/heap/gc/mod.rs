use heap::immix::MUTATORS;
use heap::immix::N_MUTATORS;
use heap::immix::ImmixMutatorLocal;
use heap::immix::ImmixSpace;
use heap::immix::ImmixLineMarkTable;
use heap::freelist::FreeListSpace;
use objectmodel;

use heap::flp::*;

use std::sync::atomic::{AtomicUsize, AtomicIsize, Ordering};
use std::sync::{Arc, Mutex, Condvar, RwLock};

extern crate crossbeam;

#[cfg(feature = "mt-trace")]
use self::crossbeam::sync::chase_lev::*;
#[cfg(feature = "mt-trace")]
use std::sync::mpsc;
#[cfg(feature = "mt-trace")]
use std::sync::mpsc::channel;
#[cfg(feature = "mt-trace")]
use std::thread;

use std::sync::atomic;

lazy_static! {
    static ref STW_COND : Arc<(Mutex<usize>, Condvar)> = {
        Arc::new((Mutex::new(0), Condvar::new()))
    };

    static ref GET_ROOTS : RwLock<Box<Fn()->Vec<ObjectAddr> + Sync + Send>> = RwLock::new(Box::new(get_roots));

    static ref GC_CONTEXT : RwLock<GCContext> = RwLock::new(GCContext{immix_space: None, lo_space: None});

    static ref ROOTS : RwLock<Vec<ObjectAddr>> = RwLock::new(vec![]);
}

static CONTROLLER : AtomicIsize = AtomicIsize::new(0);
const  NO_CONTROLLER : isize    = -1;

pub struct GCContext {
    immix_space : Option<Arc<ImmixSpace>>,
    lo_space    : Option<Arc<RwLock<FreeListSpace>>>
}

fn get_roots() -> Vec<ObjectAddr> {
    vec![]
}

pub fn init(immix_space: Arc<ImmixSpace>, lo_space: Arc<RwLock<FreeListSpace>>) {
    CONTROLLER.store(NO_CONTROLLER, Ordering::SeqCst);
    let mut gccontext = GC_CONTEXT.write().unwrap();
    gccontext.immix_space = Some(immix_space);
    gccontext.lo_space = Some(lo_space);
}

pub fn init_get_roots(get_roots: Box<Fn()->Vec<ObjectAddr> + Sync + Send>) {
    *GET_ROOTS.write().unwrap() = get_roots;
}

pub fn trigger_gc() {
    println!("Triggering GC...");

    for m in MUTATORS.write().unwrap().iter_mut() {
        if m.is_some() {
            m.as_mut().unwrap().set_take_yield(true);
        }
    }
}

extern crate libc;
#[cfg(target_arch = "x86_64")]
#[link(name = "gc_clib_x64")]
extern "C" {
    pub fn malloc_zero(size: libc::size_t) -> *const libc::c_void;
    fn immmix_get_stack_ptr() -> StkAddr;
    pub fn set_low_water_mark();
    fn get_low_water_mark() -> LowWaterAddr;
    fn get_registers() -> RegistersAddr;
    fn get_registers_count() -> i32;
}

#[inline(always)]
pub fn is_valid_object(obj_addr_in: ObjectAddr, start: SpaceAddr, end: SpaceAddrEnd, live_map: &Word2RefBits) -> (bool, ObjectAddr) {
    if obj_addr_in.as_usize() % BYTES_IN_WORD != 0 {
        return (false, obj_addr_in);
    }
    let obj_addr : ObjectAddr = obj_addr_in;
    if !start.contains_object(obj_addr, end) {
        return (false, obj_addr);
    }
    let cell_addr : CellAddr = CellAddr::from_object(obj_addr);
    if !start.contains_cell(cell_addr, end) {
        return (false, obj_addr);
    }
    (live_map.get(cell_addr.get_first_word()).get_OBJ_START_bit(), obj_addr)
}

#[inline(always)]
pub fn is_valid_object2(obj_addr_in: ObjectAddr, start: SpaceAddr, end: SpaceAddrEnd, live_map: RefBitsAddr) -> (bool, ObjectAddr) {
    if obj_addr_in.as_usize() % BYTES_IN_WORD != 0 {
        return (false, obj_addr_in);
    }
    let obj_addr : ObjectAddr = obj_addr_in;
    if !start.contains_object(obj_addr, end) {
        return (false, obj_addr);
    }
    let cell_addr : CellAddr = CellAddr::from_object(obj_addr);
    if !start.contains_cell(cell_addr, end) {
        return (false, obj_addr);
    }
    let val = Word2RefBits::map_get(start.get_first_word(), live_map, cell_addr.get_first_word());
    (val.get_OBJ_START_bit(), obj_addr)
}

pub fn stack_scan() -> Vec<ObjectAddr> {
    let stack_ptr : StkAddr = unsafe {immmix_get_stack_ptr()};
    let low_water_mark : LowWaterAddr = unsafe {get_low_water_mark()};

    let mut cursor : ObjectAddrAddr = stack_ptr.get_first_stack().get_first_objectAddrAddr();
    let mut ret = vec![];

    let gccontext = GC_CONTEXT.read().unwrap();
    let immix_space = gccontext.immix_space.as_ref().unwrap();

    while cursor.less(low_water_mark) {
        let obj_addr = cursor.objectAddr();
        let (b, value) = is_valid_object(obj_addr, immix_space.start(), immix_space.end(), &immix_space.alloc_map); if b {
            ret.push(value); //CellAddr::from_object(value)); // Point to beginning of object (ptr_0) not the cell (cell_size)
        }
        cursor = StackAddr::next_Object(cursor);
    }

    let roots_from_stack = ret.len();

    let registers_count = unsafe {get_registers_count()};
    let registers : RegistersAddr = unsafe {get_registers()};

    let mut cursor : ObjectAddrAddr = registers.regs().get_first_objectAddrAddr();
    for _i in 0..registers_count {
        let (b, value) = is_valid_object(cursor.objectAddr(), immix_space.start(), immix_space.end(), &immix_space.alloc_map); if b {
            ret.push(value);
        }
        cursor = RegsAddr::next_Object(cursor);
    }

    let roots_from_registers = ret.len() - roots_from_stack;

    println!("roots: {} from stack, {} from registers", roots_from_stack, roots_from_registers);

    ret.sort();
    ret.dedup();
    println!("unique roots: {}", ret.len());
//    for root in ret.iter() { unsafe {
//        // CellAddr
//        let typ = flp::pc::get_bits(0, root.as_usize() as *const libc::c_void, 0);
//        println!("root(0x{:X}) : {}", root, __FLP_TYPES[typ as usize]);
//    }}
    ret
}

#[inline(never)]
pub fn sync_barrier(mutator: &mut ImmixMutatorLocal) {
    let controller_id = CONTROLLER.compare_and_swap(-1, mutator.id() as isize, Ordering::SeqCst);

    println!("Mutator{} saw the controller is {}", mutator.id(), controller_id);

    // prepare the mutator for gc - return current block (if it has)
    mutator.prepare_for_gc();

    // scan its stack
    let mut thread_roots = stack_scan();
    ROOTS.write().unwrap().append(&mut thread_roots);

    // user thread call back to prepare for gc
//    USER_THREAD_PREPARE_FOR_GC.read().unwrap()();

    if controller_id != NO_CONTROLLER {
        // this thread will block
        block_current_thread(mutator);

        // reset current mutator
        mutator.reset();
    } else {
        // this thread is controller
        // other threads should block

        // wait for all mutators to be blocked
        let &(ref lock, ref cvar) = &*STW_COND.clone();
        let mut count = 0;

        println!("expect {} mutators to park", *N_MUTATORS.read().unwrap() - 1);
        while count < *N_MUTATORS.read().unwrap() - 1 {
            let new_count = {*lock.lock().unwrap()};
            if new_count != count {				
                count = new_count;
                trace!("count = {}", count);
            }
        }

        trace!("everyone stopped, gc will start");

        // roots->trace->sweep
        gc();

        // mutators will resume
        CONTROLLER.store(NO_CONTROLLER, Ordering::SeqCst);
        for t in MUTATORS.write().unwrap().iter_mut() {
            if t.is_some() {
                let t_mut = t.as_mut().unwrap();
                t_mut.set_take_yield(false);
                t_mut.set_still_blocked(false);
            }
        }
        // every mutator thread will reset themselves, so only reset current mutator here
        mutator.reset();

        // resume
        {
            let mut count = lock.lock().unwrap();
            *count = 0;
            cvar.notify_all();
        }
    }
}

fn block_current_thread(mutator: &mut ImmixMutatorLocal) {
    trace!("Mutator{} blocked", mutator.id());

    let &(ref lock, ref cvar) = &*STW_COND.clone();
    let mut count = lock.lock().unwrap();
    *count += 1;

    mutator.global.set_still_blocked(true);

    while mutator.global.is_still_blocked() {
        count = cvar.wait(count).unwrap();
    }

    trace!("Mutator{} unblocked", mutator.id());
}

pub static GC_COUNT : atomic::AtomicUsize = AtomicUsize::new(0);

fn gc() {
    GC_COUNT.store(GC_COUNT.load(atomic::Ordering::SeqCst) + 1, atomic::Ordering::SeqCst);

    //println!("GC starts");
    unsafe { EDGES_PROCESSED = 0; }

    // creates root deque
    let mut roots : &mut Vec<ObjectAddr> = &mut ROOTS.write().unwrap();

    // mark & trace
    {
        let gccontext = GC_CONTEXT.read().unwrap();
        let (immix_space, lo_space) = (gccontext.immix_space.as_ref().unwrap(), gccontext.lo_space.as_ref().unwrap());

        start_trace(&mut roots, immix_space.clone(), lo_space.clone());
    }

    //println!("{} edges processed.", unsafe { EDGES_PROCESSED });
    //println!("trace done");

    // sweep
    {
        let mut gccontext = GC_CONTEXT.write().unwrap();
        let immix_space = gccontext.immix_space.as_mut().unwrap();

        immix_space.sweep();
    }

    objectmodel::flip_mark_state();
    println!("GC finishes");
}

pub const MULTI_THREAD_TRACE_THRESHOLD : usize = 10;

pub const PUSH_BACK_THRESHOLD : usize = 50;
pub static GC_THREADS : atomic::AtomicUsize = AtomicUsize::new(0);

#[allow(unused_variables)]
#[inline(never)]
#[cfg(feature = "mt-trace")]
pub fn start_trace(work_stack: &mut Vec<ObjectAddr>, immix_space: Arc<ImmixSpace>, lo_space: Arc<RwLock<FreeListSpace>>) {
    // creates root deque
    let (mut worker, stealer) = deque();

    while !work_stack.is_empty() {
        worker.push(work_stack.pop().unwrap());
    }

    loop {
        let (sender, receiver) = channel::<ObjectAddr>();

        let mut gc_threads = vec![];
        for _ in 0..GC_THREADS.load(atomic::Ordering::SeqCst) {
            let new_immix_space = immix_space.clone();
            let new_lo_space = lo_space.clone();
            let new_stealer = stealer.clone();
            let new_sender = sender.clone();
            let t = thread::spawn(move || {
                start_steal_trace(new_stealer, new_sender, new_immix_space, new_lo_space);
            });
            gc_threads.push(t);
        }

        // only stealers own sender, when all stealers quit, the following loop finishes
        drop(sender);

        loop {
            let recv = receiver.recv();
            match recv {
                Ok(obj) => worker.push(obj),
                Err(_) => break
            }
        }

        match worker.try_pop() {
            Some(obj_ref) => worker.push(obj_ref),
            None => break
        }
    }
}

#[allow(unused_variables)]
#[inline(never)]
#[cfg(not(feature = "mt-trace"))]
pub fn start_trace(local_queue: &mut Vec<ObjectAddr>, immix_space: Arc<ImmixSpace>, lo_space: Arc<RwLock<FreeListSpace>>) {
    let mark_state = objectmodel::get_curr_MARK_STATE();

    //eprintln!("start_trace() on local_queue={:?}", local_queue);
    while !local_queue.is_empty() {
        trace_object(local_queue.pop().unwrap(), local_queue, immix_space.alloc_map.toStart,
            immix_space.trace_map.toStart, &immix_space.line_mark_table, immix_space.start(),
            immix_space.end(), mark_state);
    }
}

#[allow(unused_variables)]
#[cfg(feature = "mt-trace")]
fn start_steal_trace(stealer: Stealer<ObjectAddr>, job_sender:mpsc::Sender<ObjectAddr>, immix_space: Arc<ImmixSpace>, lo_space: Arc<RwLock<FreeListSpace>>) {

    let mut local_queue = vec![];

    let line_mark_table = &immix_space.line_mark_table;
    let (alloc_map, trace_map) = (immix_space.alloc_map.toStart, immix_space.trace_map.toStart);
    let (space_start, space_end) = (immix_space.start(), immix_space.end());
    let mark_state = MarkBits::set_MARK_from_u8(objectmodel::MARK_STATE.load(Ordering::SeqCst) as u8);

    loop {
        let work = {
            if !local_queue.is_empty() {
                local_queue.pop().unwrap()
            } else {
                let work = stealer.steal();
                match work {
                    Steal::Empty => return,
                    Steal::Abort => continue,
                    Steal::Data(obj) => obj
                }
            }
        };

        steal_trace_object(work, &mut local_queue, &job_sender, alloc_map, trace_map, line_mark_table, space_start, space_end, mark_state, &lo_space);
    }
}

#[inline(always)]
#[cfg(feature = "mt-trace")]
pub fn steal_trace_object
        ( obj_addr: ObjectAddr
        , local_queue: &mut Vec<ObjectAddr>
        , job_sender: &mpsc::Sender<ObjectAddr>
        , alloc_map: RefBitsAddr
        , trace_map: MarkBitsAddr
        , line_mark_table: &ImmixLineMarkTable
        , immix_start: SpaceAddr
        , immix_end: SpaceAddrEnd
        , mark_state: MarkBits
        , _lo_space: &Arc<RwLock<FreeListSpace>>) {

    if immix_start.contains_object(obj_addr, immix_end) {
        objectmodel::mark_as_traced(trace_map, immix_start, obj_addr, mark_state);
        //Word2MarkBits::map_set(immix_start.get_first_word(), trace_map, obj_addr.get_first_word(), mark_state);
        line_mark_table.mark_line_live(obj_addr, alloc_map, immix_start);
    } else {
        // freelist mark
    }

    let mut base = obj_addr;
    loop {
        let value : RefBits = objectmodel::get_ref_byte(alloc_map, immix_start, obj_addr);
        let (ref_bits, short_encode) = (value.get_REF_bits(), value.get_SHORT_ENCODE_bit());
        match ref_bits {
            0b000000_00 => { }
            0b000001_00 => {
                steal_process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
            },
            0b000011_00 => {
                steal_process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
                steal_process_edge(base.ptr_1().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
            },
            0b000111_00 => {
                steal_process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
                steal_process_edge(base.ptr_1().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
                steal_process_edge(base.ptr_2().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
            },
            0b001111_00 => {
                steal_process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
                steal_process_edge(base.ptr_1().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
                steal_process_edge(base.ptr_2().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
                steal_process_edge(base.ptr_3().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, job_sender, mark_state);
            },
            _ => {
                panic!("unexpected ref_bits patterns: {:b}", ref_bits);
            }
        }

        //assert!(short_encode);
        if short_encode {
            return;
        } else {
            //base = base.plus::<ObjectAddr>((RefBits::REF_NUM_BITS * BYTES_IN_WORD) as usize);
        }
    }
}

#[inline(always)]
#[cfg(feature = "mt-trace")]
pub fn steal_process_edge
        ( obj_addr: ObjectAddr
        , local_queue: &mut Vec<ObjectAddr>
        , trace_map: MarkBitsAddr
        , immix_start: SpaceAddr
        , immix_end: SpaceAddrEnd
        , alloc_map: RefBitsAddr
        , job_sender: &mpsc::Sender<ObjectAddr>
        , mark_state: MarkBits) {

    let (b, _) = is_valid_object2(obj_addr, immix_start, immix_end, alloc_map); if !b {
        return;
    }

    if !obj_addr.is_zero() && !objectmodel::is_traced(trace_map, immix_start, obj_addr, mark_state) {
        if local_queue.len() >= PUSH_BACK_THRESHOLD {
            job_sender.send(obj_addr).unwrap();
        } else {
            local_queue.push(obj_addr);
        }
    }
}

#[inline(always)]
pub fn trace_object
        ( obj_addr: ObjectAddr
        , local_queue: &mut Vec<ObjectAddr>
        , alloc_map: RefBitsAddr
        , trace_map: MarkBitsAddr
        , line_mark_table: &ImmixLineMarkTable
        , immix_start: SpaceAddr
        , immix_end: SpaceAddrEnd
        , mark_state: MarkBits) {

    if immix_start.contains_object(obj_addr, immix_end) {
        objectmodel::mark_as_traced(trace_map, immix_start, obj_addr, mark_state);
        //Word2MarkBits::map_set(immix_start.get_first_word(), trace_map, obj_addr.get_first_word(), mark_state);
        line_mark_table.mark_line_live(obj_addr, alloc_map, immix_start);
    } else {
        // freelist mark
    }

    eprintln!("Tracing object 0x{:X} ... ", obj_addr);

    let base = obj_addr;
    let value = objectmodel::get_ref_byte(alloc_map, immix_start, obj_addr);
    let (ref_bits, obj_start, _short_encode) = (value.get_REF_bits(), value.get_OBJ_START_bit(), value.get_SHORT_ENCODE_bit());
    if !obj_start {
        backtraceHere!(obj_addr);
        eprintln!("Unexpected pointer to start of non-object: obj_addr=0x{:X}, ref_bits=0b{:08b}", obj_addr, ref_bits);
        //check_expect_pc!(obj_addr.as_usize(), vec![__FLP_IDX_UNMAPPED]);
    }

    loop {
        let value = objectmodel::get_ref_byte(alloc_map, immix_start, obj_addr);
        let (ref_bits, short_encode) = (value.get_REF_bits(), value.get_SHORT_ENCODE_bit());
        //eprintln!("ref_bits=0b{:08b}, short_encode={:?}, value=0b{:08b}", ref_bits, short_encode, value.get_REF_bits());
        match ref_bits {
            0b000000_00 => { }
            0b000001_00 => {
                process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
            },
            0b000011_00 => {
                process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
                process_edge(base.ptr_1().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
            },
            0b000111_00 => {
                process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
                process_edge(base.ptr_1().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
                process_edge(base.ptr_2().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
            },
            0b001111_00 => {
                process_edge(base.ptr_0().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
                process_edge(base.ptr_1().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
                process_edge(base.ptr_2().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
                process_edge(base.ptr_3().load_ObjectAddr(), local_queue, trace_map, immix_start, immix_end, alloc_map, mark_state);
            },
            _ => {
                panic!("unexpected ref_bits patterns: {:b} for address 0x{:X}", ref_bits, obj_addr);
            }
        }

        //debug_assert!(short_encode);
        if short_encode {
            return;
        } else {
            //base = base.plus::<ObjectAddr>((RefBits::REF_NUM_BITS * BYTES_IN_WORD) as usize);
        }
    }
}

pub static mut EDGES_PROCESSED : usize = 0;

#[inline(always)]
pub fn process_edge
        ( obj_addr: ObjectAddr
        , local_queue:&mut Vec<ObjectAddr>
        , trace_map: MarkBitsAddr
        , space_start: SpaceAddr
        , _space_end: SpaceAddrEnd
        , _alloc_map: RefBitsAddr
        , mark_state: MarkBits) {

    if !space_start.contains_object(obj_addr, _space_end) {
        return; // Address was either set to null, a garbage value, or never initialized.
    }
    if objectmodel::is_traced(trace_map, space_start, obj_addr, mark_state) {
        return;
    }
    local_queue.push(obj_addr);
}
