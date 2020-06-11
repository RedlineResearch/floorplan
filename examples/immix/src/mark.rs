extern crate time;

use heap;
use heap::layout::*;
use heap::immix::ImmixMutatorLocal;
use heap::immix::ImmixSpace;
use heap::freelist::FreeListSpace;

use std::sync::RwLock;
use std::sync::{Arc};
use std::sync::atomic::Ordering;

use exhaust::OBJECT_SIZE;
use exhaust::OBJECT_ALIGN;
use exhaust::ALLOCATION_TIMES;

const MARK_TIMES : usize = ALLOCATION_TIMES;

#[allow(unused_variables)]
pub fn alloc_mark() {
    let shared_space : Arc<ImmixSpace> = {
        let space : ImmixSpace = ImmixSpace::new(heap::IMMIX_SPACE_SIZE.load(Ordering::SeqCst));
        
        Arc::new(space)
    };
    let lo_space : Arc<RwLock<FreeListSpace>> = {
        let space : FreeListSpace = FreeListSpace::new(heap::LO_SPACE_SIZE.load(Ordering::SeqCst));
        Arc::new(RwLock::new(space))
    };
    heap::gc::init(shared_space.clone(), lo_space.clone());

    let mut mutator = ImmixMutatorLocal::new(shared_space.clone());
    
    println!("Trying to allocate 1 object of (size {}, align {}). ", OBJECT_SIZE, OBJECT_ALIGN);
    const ACTUAL_OBJECT_SIZE : usize = OBJECT_SIZE;
    println!("Considering header size of {}, an object should be {}. ", 0, ACTUAL_OBJECT_SIZE);
    
    println!("Trying to allocate {} objects, which will take roughly {} bytes", MARK_TIMES, MARK_TIMES * ACTUAL_OBJECT_SIZE);
    let mut objs : Vec<ObjectAddr> = vec![];
    for _ in 0..MARK_TIMES {
        let res = mutator.alloc(ACTUAL_OBJECT_SIZE, OBJECT_ALIGN);
        mutator.init_object(res, 0b000011_11);
        
        objs.push(res);
    }
    
    mark_loop(objs, &shared_space);
}

#[inline(never)]
fn mark_loop(objs: Vec<ObjectAddr>, shared_space: &Arc<ImmixSpace>) {
    use objectmodel;
    
    println!("Start marking");
    let t_start = time::Instant::now();
    
    let mark_state : MarkBits = MarkBits::set_MARK_from_u8(objectmodel::MARK_STATE.load(Ordering::SeqCst) as u8);
    
    let line_mark_table = shared_space.line_mark_table();
    let (space_start, space_end) = (shared_space.start(), shared_space.end());
    
    let trace_map = shared_space.trace_map.toStart;
    
    for i in 0..objs.len() {
        let obj_addr = unsafe {*objs.get_unchecked(i)};
            
        // mark the object as traced
        objectmodel::mark_as_traced(trace_map, space_start, obj_addr, mark_state);
        
        // mark meta-data
        if space_start.contains_object(obj_addr, space_end) {
            line_mark_table.mark_line_live2(space_start.get_first_line(), obj_addr);
        } 
    }
    
    let t_end = time::Instant::now();
    
    println!("time used: {} msec", (t_end - t_start).whole_milliseconds());
}
