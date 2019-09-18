#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused_variables)]

use heap;
use heap::flp::*;
use heap::immix::ImmixMutatorLocal;
use heap::immix::ImmixSpace;
use heap::freelist::FreeListSpace;
use std::mem::size_of as size_of;
use std::boxed::Box;
use std::io::Write;
use std::fs::File;

extern crate time;

pub const PRINT : bool = true;

pub struct LLNode {
    fwd : *mut LLNode,
    bwd : *mut LLNode,
    val : usize
}

pub struct QNode {
    nxt : *mut QNode,
    contents : *mut LLNode
}

pub struct Dequeue {
    fst : *mut QNode,
    lst : *mut QNode
}

fn dump_llnode(f: &mut File, ll: *mut LLNode) {
    write!(f, "0x{:X},{} -> fwd=0x{:X}\n", ll as usize, __FLP_IDX_LLNODE, ll.fwd() as usize).unwrap();
    if ll.fwd() == ll { return; }
    dump_llnode(f, ll.fwd());
}

fn dump_qnode(f: &mut File, qn: *mut QNode) {
    write!(f, "0x{:X},{} -> contents=0x{:X}\n", qn as usize, __FLP_IDX_QNODE, qn.contents() as usize).unwrap();
    write!(f, "0x{:X},{} -> nxt=0x{:X}\n", qn as usize, __FLP_IDX_QNODE, qn.nxt() as usize).unwrap();
    dump_llnode(f, qn.contents());
    if qn.nxt() == qn { return; }
    dump_qnode(f, qn.nxt());
}

pub static mut main_dequeue : *mut Dequeue = 0x0 as *mut Dequeue;

pub fn dump_dequeue(count: usize) { unsafe {
    use std::fmt::Write;
    let mut filename = String::new();
    write!(&mut filename, "data/live-heap-{:04}.csv", count).unwrap();
    let mut f = File::create(filename).expect("Unable to create file");
    write!(f, "# Live Heap\n").unwrap();
    write!(f, "0x{:X},{} -> fst=0x{:X}\n", main_dequeue as usize, __FLP_IDX_DEQUEUE, main_dequeue.fst() as usize).unwrap();
    dump_qnode(&mut f, main_dequeue.fst());
}}

fn len_llnode(node: *mut LLNode) -> usize {
    let mut curr = node;
    let mut count : usize = 0;
    loop {
        count += 1;
        let tmp = curr.fwd();
        if tmp == curr { break; }
        curr = tmp;
    }
    count
}

fn len_qnode(node: *mut QNode) -> usize {
    let mut curr = node;
    let mut count : usize = 0;
    loop {
        count += 1;
        let tmp = curr.nxt();
        if tmp == curr { break; }
        curr = tmp;
    }
    count
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
    if PRINT { eprint!("Allocated 0x{:X}", obj_addr.as_usize()); }
    //if obj_addr.as_usize() == 0x7FFFF73F0208 {
    //    eprintln!("Allocated 0x7FFFF73F0208 as {}", typ);
    //}
    ret
}

fn alloc_llnode() -> *mut LLNode {
    let ret = alloc::<LLNode>(2, __FLP_IDX_LLNODE);
    if PRINT { eprintln!(" as LLNODE"); }
    ret
}

fn alloc_qnode()  -> *mut QNode  {
    let ret = alloc::<QNode>(2, __FLP_IDX_QNODE);
    if PRINT { eprintln!(" as QNODE"); }
    ret
}

fn alloc_dequeue() -> *mut Dequeue {
    let ret = alloc::<Dequeue>(2, __FLP_IDX_DEQUEUE);
    if PRINT { eprintln!(" as DEQUEUE"); }
    ret
}

trait LLNodeAccess {
    fn check(&self);
    fn set_fwd(&self, r: *mut LLNode);
    fn set_bwd(&self, r: *mut LLNode);
    fn set_val(&self, r: usize);
    fn fwd(&self) -> *mut LLNode;
    fn bwd(&self) -> *mut LLNode;
    fn val(&self) -> usize;
}

impl LLNodeAccess for *mut LLNode {
    fn check(&self) { } //check_expect(*self as usize, vec![__FLP_IDX_LLNODE]); }
    fn set_fwd(&self, r: *mut LLNode) { self.check(); unsafe { (**self).fwd = r; } }
    fn set_bwd(&self, r: *mut LLNode) { self.check(); unsafe { (**self).bwd = r; } }
    fn set_val(&self, r: usize) { self.check(); unsafe { (**self).val = r; } }
    fn fwd(&self) -> *mut LLNode { self.check(); unsafe { (**self).fwd } }
    fn bwd(&self) -> *mut LLNode { self.check(); unsafe { (**self).bwd } }
    fn val(&self) -> usize { self.check(); unsafe { (**self).val } }
}

trait QNodeAccess {
    fn check(&self);
    fn set_nxt(&self, r: *mut QNode);
    fn set_contents(&self, r: *mut LLNode);
    fn nxt(&self) -> *mut QNode;
    fn contents(&self) -> *mut LLNode;
}

impl QNodeAccess for *mut QNode {
    fn check(&self) { } //check_expect(*self as usize, vec![__FLP_IDX_QNODE]); }
    fn set_nxt(&self, r: *mut QNode) { unsafe { self.check(); (**self).nxt = r; }}
    fn set_contents(&self, r: *mut LLNode) { unsafe { self.check(); (**self).contents = r; }}
    fn nxt(&self) -> *mut QNode { self.check(); unsafe { (**self).nxt } }
    fn contents(&self) -> *mut LLNode { self.check(); unsafe { (**self).contents } }
}

trait DequeueAccess {
    fn check(&self);
    fn set_fst(&self, r: *mut QNode);
    fn set_lst(&self, r: *mut QNode);
    fn fst(&self) -> *mut QNode;
    fn lst(&self) -> *mut QNode;
}

impl DequeueAccess for *mut Dequeue {
    fn check(&self) { } //check_expect(*self as usize, vec![__FLP_IDX_DEQUEUE]); }
    fn set_fst(&self, r: *mut QNode) { unsafe { self.check(); (**self).fst = r; }}
    fn set_lst(&self, r: *mut QNode) { unsafe { self.check(); (**self).lst = r; }}
    fn fst(&self) -> *mut QNode  { unsafe { self.check(); (**self).fst }}
    fn lst(&self) -> *mut QNode  { unsafe { self.check(); (**self).lst }}
}

pub fn cleanup_llnode(node: *mut LLNode) {
    let mut curr = node;
    let mut count = 0;
    loop {
        if PRINT { eprintln!("Marked LLNode 0x{:X} as GARBAGE", curr as usize); }
        count += 1;
        let tmp = curr.fwd();
        curr.set_fwd(0xdeadbeef as *mut LLNode);
        curr.set_bwd(0xdeadbabe as *mut LLNode);
        curr.set_val(0xdeadFAFA as usize);
        
//        for i in 0..(size_of::<LLNode>()) {
//            unsafe { flp::pc::set_bits(0, ((curr as usize) + i) as *const libc::c_void, __FLP_IDX_GARBAGE); }
//        }
        
        if tmp == curr { break; }
        curr = tmp;
    }
    debug_assert!(count == 3001);
}

pub fn cleanup_qnode(node: *mut QNode) {
    let mut curr = node;
    let mut count = 0;
    loop {
        if PRINT { eprintln!("Marked QNode 0x{:X} as GARBAGE", curr as usize); }
        count += 1;
        debug_assert!(len_llnode(curr.contents()) == 3001);
        cleanup_llnode(curr.contents());
        let tmp = curr.nxt();
        nxt(curr, 0x0 as *mut QNode);

        //for i in 0..(size_of::<QNode>()) {
        //    unsafe { flp::pc::set_bits(0, ((curr as usize)+ i) as *const libc::c_void, __FLP_IDX_GARBAGE); }
        //}

        if tmp == curr { break; }
        curr = tmp;
    }
    debug_assert!(count == 1);
}

pub fn pop(queue: *mut Dequeue) -> *mut QNode {
    let tmp = queue.fst(); // grab the first thing
    //if (tmp as usize) < 0xffffff {
        //flp::dump_map();
    //}
    //if (tmp as usize) > 0xffffff && ((*(*queue).fst).nxt as usize) < 0xffffff {
    //    dump_map!();
    //}

    //println!("tmp = {:?}", tmp);
    let x = tmp.nxt();
    println!("queue.fst = {:?}", x);
    queue.set_fst(queue.fst().nxt());
    tmp.set_nxt(tmp); // unlink it from the list so it can't see anything else
    //check_expect(tmp as usize, vec![__FLP_IDX_QNODE]);
    debug_assert!(len_qnode(tmp) == 1);
    tmp
}

pub fn push(queue: *mut Dequeue, val: *mut QNode) {
    //println!("push val = {:?}", val);
    queue.lst().set_nxt(val);
    //unsafe { nxt((*queue).lst, val); }
    queue.set_lst(val);
}
/*
pub fn fwd(l: *mut LLNode, r: *mut LLNode) { unsafe { (*l).fwd = r; } }
pub fn bwd(l: *mut LLNode, r: *mut LLNode) { unsafe { (*l).bwd = r; } }
pub fn val(l: *mut LLNode, r: usize) {
    unsafe { (*l).val = r; }
}*/

pub fn nxt(l: *mut QNode, r: *mut QNode) {
    //println!("nxt => {:?}", r);
    l.set_nxt(r);
}
//pub fn contents(l: *mut QNode, r: *mut LLNode) { unsafe { (*l).contents = r; } }

pub fn build_ll(min: usize, max: usize) -> *mut LLNode {
    let mut i : usize = min;
    let root = alloc_llnode();
    root.set_fwd(root);
    root.set_bwd(root);
    root.set_val(0);
    
    let mut curr = root;
    let mut next = root;
    while i < max {
        curr.set_val(i);
        next = alloc_llnode();
        curr.set_fwd(next);
        next.set_bwd(curr);
        i += 1;
        curr = next;
    }
    next.set_val(max);
    next.set_fwd(next);
    debug_assert!(len_llnode(root) == (max - min + 1));
    root
}

pub fn mk_qnode(cont: *mut LLNode) -> *mut QNode {
    let n : *mut QNode = alloc_qnode();
    n.set_nxt(n);
    n.set_contents(cont);
    n
}

pub fn start() {
    use std::sync::{Arc, RwLock};

    unsafe {heap::gc::set_low_water_mark();}

    let immix_space : Arc<ImmixSpace> = {
        let space : ImmixSpace = ImmixSpace::new(10 << 20);
        Arc::new(space)
    };
    let lo_space : Arc<RwLock<FreeListSpace>> = {
        let space : FreeListSpace = FreeListSpace::new(3 << 20);
        Arc::new(RwLock::new(space))
    };
    heap::gc::init(immix_space.clone(), lo_space.clone());
    unsafe { mutator = Box::into_raw(Box::new(ImmixMutatorLocal::new(immix_space.clone()))); }

    println!("Leak GC test");
    let tStart = time::now_utc();
    let mut i = 0;
    let ll = build_ll(6363, 6363 + 3000);
    let first : *mut QNode = mk_qnode(ll);
    let queue : *mut Dequeue = alloc_dequeue();
    unsafe { main_dequeue = queue; }
    queue.set_fst(first);
    queue.set_lst(first);

    while i < 10 {
        let qn : *mut QNode = mk_qnode(build_ll(6363, 6363 + 3000));
        push(queue, qn);
        i += 1;
    }
    while i < 10000 {
        //dump_dequeue(i);
        let qn : *mut QNode = mk_qnode(build_ll(6363, 6363 + 3000));
        push(queue, qn);
        if i % 2 == 0 {
            let qn_dead = pop(queue);
            cleanup_qnode(qn_dead);
        }
        i += 1;
    }
    println!("Finished!");
}

