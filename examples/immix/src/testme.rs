#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(unused_assignments)]

use heap;
use heap::flp::*;
use heap::immix::ImmixMutatorLocal;
use heap::immix::ImmixSpace;
use heap::freelist::FreeListSpace;
use std::mem::size_of as size_of;
use std::boxed::Box;

pub struct Node {
    fwd : *mut Node,
    bwd : *mut Node,
    val : usize
}

static mut mutator: *mut ImmixMutatorLocal = std::ptr::null::<ImmixMutatorLocal>() as *mut ImmixMutatorLocal;
fn alloc<Type>(numP: usize, typ: u8) -> *mut Type {
    let sz = size_of::<Type>();
    let obj_addr : ObjectAddr = unsafe { (*mutator).alloc(sz, 8) };
    unsafe { (*mutator).init_object(obj_addr, match numP {
        0 => 0b000000_11,
        1 => 0b000001_11,
        2 => 0b000011_11,
        3 => 0b000111_11,
        4 => 0b001111_11,
        _ => 0b000000_11
    }); }
    let ret = obj_addr.to_ptr_mut::<Type>();
    //for i in 0..sz {
    //    check_expect_pc!(obj_addr.as_usize() + i, vec![__FLP_IDX_CELL]);
    //    unsafe { flp::pc::set_bits(0, (obj_addr.as_usize() + i) as *const libc::c_void, typ); }
    //}
    ret
}

fn alloc_node() -> *mut Node {
    let ret = alloc::<Node>(2, __FLP_IDX_LLNODE);
    eprintln!("Allocated {} bytes at 0x{:X}", size_of::<Node>(), ret as usize);
    ret
}

pub const NUM_BLOCKS : usize = 1; // 1 Block (65536 bytes) fits 2048 Nodes (3 words + 1 cell_size word per Node)
pub const LO_SIZE : usize = 1 << 20; // Also 1 megabyte (large-object space)

pub fn fwd(n: *mut Node, val: *mut Node) { unsafe {
    (*n).fwd = val;
}}
pub fn bwd(n: *mut Node, val: *mut Node) { unsafe {
    (*n).bwd = val;
}}
pub fn get_fwd(n: *mut Node) -> *mut Node { unsafe {
    (*n).fwd
}}
pub fn get_bwd(n: *mut Node) -> *mut Node { unsafe {
    (*n).bwd
}}

pub fn trash_it(n: *mut Node) {
    //for i in 0 .. size_of::<Node>() {
    //    unsafe { flp::pc::set_bits(0, ((n as usize) + i) as *const libc::c_void, __FLP_IDX_GARBAGE); }
    //}
}

pub fn start() {
    use std::sync::{Arc, RwLock};
    unsafe {heap::gc::set_low_water_mark();}
    let immix_space : Arc<ImmixSpace> = {
        let space : ImmixSpace = ImmixSpace::new(NUM_BLOCKS * (1 << LOG_BYTES_IN_BLOCK));
        Arc::new(space)
    };
    let lo_space : Arc<RwLock<FreeListSpace>> = {
        let space : FreeListSpace = FreeListSpace::new(LO_SIZE);
        Arc::new(RwLock::new(space))
    };
    heap::gc::init(immix_space.clone(), lo_space.clone());
    unsafe { mutator = Box::into_raw(Box::new(ImmixMutatorLocal::new(immix_space.clone()))); }

    {
        let mut tmp = 0x0 as *mut Node;
        let orig = alloc_node();
        fwd(orig, orig);
        bwd(orig, orig);
        let mut curr = orig;
        for i in 0 .. (2048 - 1) {
            tmp = alloc_node();
            fwd(curr, tmp);
            bwd(tmp, curr);
            curr = tmp;
        }
        fwd(curr, curr);
        curr = get_fwd(orig);
        loop {
            tmp = get_fwd(curr);
            bwd(curr, 0xdeadbeef as *mut Node);
            fwd(curr, 0xdeadbeef as *mut Node);
            trash_it(curr);
            if tmp == curr { break; }
            curr = tmp;
        }
        fwd(orig, orig);
        bwd(orig, orig);
        curr = orig;
        tmp = orig;
        // Everything except the original node is marked as garbage now, and no longer accessible
        // via the Rust stack (i.e. orig and curr both point to orig, which is /not/ marked as
        // garbage)
        tmp = alloc_node();
        
        // Now trash that node, and move on to next allocation cycle:
        fwd(tmp, 0xdeaddead as *mut Node);
        bwd(tmp, 0xdeaddead as *mut Node);
        trash_it(tmp);
        for i in 0 .. (2024 - 1) {
            tmp = alloc_node();
            fwd(curr, tmp);
            bwd(tmp, curr);
            curr = tmp;
        }
        fwd(curr, curr);
        
        // Delete everything again, except the original:
        curr = get_fwd(orig);
        loop {
            tmp = get_fwd(curr);
            bwd(curr, 0xdeadeeee as *mut Node);
            fwd(curr, 0xdeadeeee as *mut Node);
            trash_it(curr);
            if tmp == curr { break; }
            curr = tmp;
        }
        fwd(orig, orig);
        bwd(orig, orig);
        curr = orig;
        tmp = orig;
        
        // Trigger GC?:
        tmp = alloc_node();

    }
    println!("Finished!");
    //flp::dump_map()
}

