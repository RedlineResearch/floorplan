#![allow(non_snake_case)]
use std::sync::atomic;
use std::sync::atomic::Ordering;

pub static MARK_STATE : atomic::AtomicUsize = atomic::AtomicUsize::new(0);

use heap::flp::*;

pub fn init() {
    MARK_STATE.store(1, atomic::Ordering::SeqCst);
}

pub fn get_curr_MARK_STATE() -> MarkBits {
    MarkBits::set_MARK_from_u8(MARK_STATE.load(Ordering::SeqCst) as u8)
}

pub fn get_curr_UNMARKED_STATE() -> MarkBits {
    let mark_state = MARK_STATE.load(Ordering::SeqCst) as u8;
    if mark_state == 0 {
        MarkBits::set_MARK_from_u8(0b00000001 as u8)
    } else {
        MarkBits::set_MARK_from_u8(0b00000000 as u8)
    }
}

pub fn flip_mark_state() {
    let mark_state = MARK_STATE.load(atomic::Ordering::SeqCst);
    if mark_state == 0 {
        MARK_STATE.store(1, atomic::Ordering::SeqCst);
    } else {
        MARK_STATE.store(0, atomic::Ordering::SeqCst);
    }
}

#[inline(always)]
pub fn mark_as_traced(trace_map: MarkBitsAddr, space_start: SpaceAddr, obj: ObjectAddr, mark_state: MarkBits) {
    let cell = CellAddr::from_object(obj);
    Word2MarkBits::map_set(space_start.get_first_word(), trace_map, cell.get_first_word(), mark_state)
}

#[inline(always)]
pub fn is_traced(trace_map: MarkBitsAddr, space_start: SpaceAddr, obj: ObjectAddr, mark_state: MarkBits) -> bool {
    let cell = CellAddr::from_object(obj);
    let state = Word2MarkBits::map_get(space_start.get_first_word(), trace_map, cell.get_first_word());
    //println!("state = {}, (state == mark_state) = {}", state.get_MARK_bits(), state == mark_state);
    state == mark_state
}

#[inline(always)]
pub fn get_ref_byte(alloc_map: RefBitsAddr, space_start: SpaceAddr, obj: ObjectAddr) -> RefBits {
    let cell = CellAddr::from_object(obj);
    Word2RefBits::map_get(space_start.get_first_word(), alloc_map, cell.get_first_word())
}

#[inline(always)]
pub fn set_ref_byte(alloc_map: RefBitsAddr, space_start: SpaceAddr, obj: ObjectAddr, value: RefBits) {
    let cell_addr = CellAddr::from_object(obj);
    Word2RefBits::map_set(space_start.get_first_word(), alloc_map, cell_addr.get_first_word(), value)
}

