#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
extern crate libc;

use std::cmp;

use std::fmt;

use std::mem::size_of as size_of;

use self::libc::size_t as size_t;

use super::*;

#[derive(Copy, Clone)]
pub struct Region(());

#[derive(Copy, Clone)]
pub struct Space(());

#[derive(Copy, Clone)]
pub struct FreeBlock([u8; 65536]);

impl PartialEq for FreeBlock {
  #[inline(always)]
  fn eq(&self, other: &FreeBlock) -> bool {
    for i in 0 .. (size_of::<FreeBlock>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &FreeBlock) -> bool {
    for i in 0 .. (size_of::<FreeBlock>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Block([u8; 65536]);

impl PartialEq for Block {
  #[inline(always)]
  fn eq(&self, other: &Block) -> bool {
    for i in 0 .. (size_of::<Block>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Block) -> bool {
    for i in 0 .. (size_of::<Block>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Cells(());

#[derive(Copy, Clone)]
pub struct FreeCell(());

#[derive(Copy, Clone)]
pub struct Cell(());

#[derive(Copy, Clone)]
pub struct Cell_size([u8; 8]);

impl PartialEq for Cell_size {
  #[inline(always)]
  fn eq(&self, other: &Cell_size) -> bool {
    for i in 0 .. (size_of::<Cell_size>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Cell_size) -> bool {
    for i in 0 .. (size_of::<Cell_size>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Object(());

#[derive(Copy, Clone)]
pub struct Ptr_0([u8; 8]);

impl PartialEq for Ptr_0 {
  #[inline(always)]
  fn eq(&self, other: &Ptr_0) -> bool {
    for i in 0 .. (size_of::<Ptr_0>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Ptr_0) -> bool {
    for i in 0 .. (size_of::<Ptr_0>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Ptr_1([u8; 8]);

impl PartialEq for Ptr_1 {
  #[inline(always)]
  fn eq(&self, other: &Ptr_1) -> bool {
    for i in 0 .. (size_of::<Ptr_1>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Ptr_1) -> bool {
    for i in 0 .. (size_of::<Ptr_1>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Ptr_2([u8; 8]);

impl PartialEq for Ptr_2 {
  #[inline(always)]
  fn eq(&self, other: &Ptr_2) -> bool {
    for i in 0 .. (size_of::<Ptr_2>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Ptr_2) -> bool {
    for i in 0 .. (size_of::<Ptr_2>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Ptr_3([u8; 8]);

impl PartialEq for Ptr_3 {
  #[inline(always)]
  fn eq(&self, other: &Ptr_3) -> bool {
    for i in 0 .. (size_of::<Ptr_3>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Ptr_3) -> bool {
    for i in 0 .. (size_of::<Ptr_3>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Payload(());

#[derive(Copy, Clone)]
pub struct Remainder(());

#[derive(Copy, Clone)]
pub struct Limit([u8; 0]);

impl PartialEq for Limit {
  #[inline(always)]
  fn eq(&self, other: &Limit) -> bool {
    for i in 0 .. (size_of::<Limit>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Limit) -> bool {
    for i in 0 .. (size_of::<Limit>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Line([u8; 256]);

impl PartialEq for Line {
  #[inline(always)]
  fn eq(&self, other: &Line) -> bool {
    for i in 0 .. (size_of::<Line>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Line) -> bool {
    for i in 0 .. (size_of::<Line>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Lms(());

#[derive(Copy, Clone)]
pub struct LineMark([u8; 1]);

impl PartialEq for LineMark {
  #[inline(always)]
  fn eq(&self, other: &LineMark) -> bool {
    for i in 0 .. (size_of::<LineMark>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &LineMark) -> bool {
    for i in 0 .. (size_of::<LineMark>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Refs(());

#[derive(Copy, Clone)]
pub struct RefBits([u8; 1]);

impl PartialEq for RefBits {
  #[inline(always)]
  fn eq(&self, other: &RefBits) -> bool {
    for i in 0 .. (size_of::<RefBits>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &RefBits) -> bool {
    for i in 0 .. (size_of::<RefBits>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Mks(());

#[derive(Copy, Clone)]
pub struct MarkBits([u8; 1]);

impl PartialEq for MarkBits {
  #[inline(always)]
  fn eq(&self, other: &MarkBits) -> bool {
    for i in 0 .. (size_of::<MarkBits>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &MarkBits) -> bool {
    for i in 0 .. (size_of::<MarkBits>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Stk(());

#[derive(Copy, Clone)]
pub struct Stack(());

#[derive(Copy, Clone)]
pub struct LowWater([u8; 0]);

impl PartialEq for LowWater {
  #[inline(always)]
  fn eq(&self, other: &LowWater) -> bool {
    for i in 0 .. (size_of::<LowWater>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &LowWater) -> bool {
    for i in 0 .. (size_of::<LowWater>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Registers(());

#[derive(Copy, Clone)]
pub struct Regs(());

#[derive(Copy, Clone)]
pub struct RegsEnd([u8; 0]);

impl PartialEq for RegsEnd {
  #[inline(always)]
  fn eq(&self, other: &RegsEnd) -> bool {
    for i in 0 .. (size_of::<RegsEnd>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &RegsEnd) -> bool {
    for i in 0 .. (size_of::<RegsEnd>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct AppObject(());

#[derive(Copy, Clone)]
pub struct LLNode([u8; 24]);

impl PartialEq for LLNode {
  #[inline(always)]
  fn eq(&self, other: &LLNode) -> bool {
    for i in 0 .. (size_of::<LLNode>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &LLNode) -> bool {
    for i in 0 .. (size_of::<LLNode>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct QNode([u8; 16]);

impl PartialEq for QNode {
  #[inline(always)]
  fn eq(&self, other: &QNode) -> bool {
    for i in 0 .. (size_of::<QNode>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &QNode) -> bool {
    for i in 0 .. (size_of::<QNode>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Dequeue([u8; 16]);

impl PartialEq for Dequeue {
  #[inline(always)]
  fn eq(&self, other: &Dequeue) -> bool {
    for i in 0 .. (size_of::<Dequeue>()) { if self.0[i] != other.0[i] { return false; } }
    return true;
  }
  #[inline(always)]
  fn ne(&self, other: &Dequeue) -> bool {
    for i in 0 .. (size_of::<Dequeue>()) { if self.0[i] != other.0[i] { return true; } }
    return false;
  }
}

#[derive(Copy, Clone)]
pub struct Garbage(());

impl AppObject { }

pub const APPOBJECT_LOG_BYTES_ALIGN: usize = 0;

pub const APPOBJECT_BYTES_ALIGN: usize = 1 << APPOBJECT_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct AppObjectAddr(usize);

deriveAddr! { AppObjectAddr, APPOBJECT_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct AppObjectAddrEnd(usize);

deriveAddr! { AppObjectAddrEnd, 1 }

pub const __FLP_IDX_APPOBJECT: u8 = 30;

impl AppObjectAddr {
  pub const LLNODE_OFFSET_BYTES: usize = 0;
  pub fn lLNode(&self) -> LLNodeAddr {
    self.plus::<LLNodeAddr>(AppObjectAddr::LLNODE_OFFSET_BYTES)
  }
  pub fn from_lLNode(addr: LLNodeAddr) -> AppObjectAddr {
    addr.sub::<AppObjectAddr>(AppObjectAddr::LLNODE_OFFSET_BYTES)
  }
  pub const QNODE_OFFSET_BYTES: usize = 0;
  pub fn qNode(&self) -> QNodeAddr {
    self.plus::<QNodeAddr>(AppObjectAddr::QNODE_OFFSET_BYTES)
  }
  pub fn from_qNode(addr: QNodeAddr) -> AppObjectAddr {
    addr.sub::<AppObjectAddr>(AppObjectAddr::QNODE_OFFSET_BYTES)
  }
  pub const DEQUEUE_OFFSET_BYTES: usize = 0;
  pub fn dequeue(&self) -> DequeueAddr {
    self.plus::<DequeueAddr>(AppObjectAddr::DEQUEUE_OFFSET_BYTES)
  }
  pub fn from_dequeue(addr: DequeueAddr) -> AppObjectAddr {
    addr.sub::<AppObjectAddr>(AppObjectAddr::DEQUEUE_OFFSET_BYTES)
  }
  pub fn contains_lLNode(&self, addr: LLNodeAddr, end: AppObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_qNode(&self, addr: QNodeAddr, end: AppObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_dequeue(&self, addr: DequeueAddr, end: AppObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_lLNode(self) -> LLNodeAddr {
    LLNodeAddr::from_usize(self.as_usize())
  }
  pub fn get_first_qNode(self) -> QNodeAddr {
    QNodeAddr::from_usize(self.as_usize())
  }
  pub fn get_first_dequeue(self) -> DequeueAddr {
    DequeueAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn size_of(&self, end: AppObjectAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_APPOBJECT;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Block {
  pub fn memset_cells_until_remainder(val: u8, base: CellsAddr, mx: RemainderAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn bump_new_FreeCell(rhs: RemainderAddr, bytes: usize) -> (FreeCellAddr, RemainderAddr) {
    (rhs.plus(0), rhs.plus(bytes))
  }
  pub fn bump_new_Cell(rhs: RemainderAddr, bytes: usize) -> (CellAddr, RemainderAddr) {
    (rhs.plus(0), rhs.plus(bytes))
  }
  pub fn init_remainder_after_cells(p1: CellsAddr, bytes: usize) -> RemainderAddr {
    p1.plus(bytes)
  }
  pub fn cells_is_validly_before_remainder(p1: CellsAddr, p2: RemainderAddr) -> bool {
    p1.lte(p2)
  }
  pub fn memset_remainder_until_limit(val: u8, base: RemainderAddr, mx: LimitAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn init_limit_after_remainder(p1: RemainderAddr, bytes: usize) -> LimitAddr {
    p1.plus(bytes)
  }
  pub fn remainder_is_validly_before_limit(p1: RemainderAddr, p2: LimitAddr) -> bool {
    p1.lte(p2)
  }
  pub fn cast_line_to_cells(addr: LineAddr) -> CellsAddr {
    CellsAddr::from_usize(addr.as_usize())
  }
  pub fn cast_cells_to_line(addr: CellsAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_freeCell(addr: LineAddr) -> FreeCellAddr {
    FreeCellAddr::from_usize(addr.as_usize())
  }
  pub fn cast_freeCell_to_line(addr: FreeCellAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_cell(addr: LineAddr) -> CellAddr {
    CellAddr::from_usize(addr.as_usize())
  }
  pub fn cast_cell_to_line(addr: CellAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_cell_size(addr: LineAddr) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(addr.as_usize())
  }
  pub fn cast_cell_size_to_line(addr: Cell_sizeAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_object(addr: LineAddr) -> ObjectAddr {
    ObjectAddr::from_usize(addr.as_usize())
  }
  pub fn cast_object_to_line(addr: ObjectAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_ptr_0(addr: LineAddr) -> Ptr_0Addr {
    Ptr_0Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_0_to_line(addr: Ptr_0Addr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_ptr_1(addr: LineAddr) -> Ptr_1Addr {
    Ptr_1Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_1_to_line(addr: Ptr_1Addr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_ptr_2(addr: LineAddr) -> Ptr_2Addr {
    Ptr_2Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_2_to_line(addr: Ptr_2Addr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_ptr_3(addr: LineAddr) -> Ptr_3Addr {
    Ptr_3Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_3_to_line(addr: Ptr_3Addr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_payload(addr: LineAddr) -> PayloadAddr {
    PayloadAddr::from_usize(addr.as_usize())
  }
  pub fn cast_payload_to_line(addr: PayloadAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_remainder(addr: LineAddr) -> RemainderAddr {
    RemainderAddr::from_usize(addr.as_usize())
  }
  pub fn cast_remainder_to_line(addr: RemainderAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
  pub fn cast_line_to_limit(addr: LineAddr) -> LimitAddr {
    LimitAddr::from_usize(addr.as_usize())
  }
  pub fn cast_limit_to_line(addr: LimitAddr) -> LineAddr {
    LineAddr::from_usize(addr.as_usize())
  }
}

pub const BLOCK_LOG_BYTES_ALIGN: usize = 16;

pub const BLOCK_BYTES_ALIGN: usize = 1 << BLOCK_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct BlockAddr(usize);

deriveAddr! { BlockAddr, BLOCK_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct BlockAddrEnd(usize);

deriveAddr! { BlockAddrEnd, 1 }

pub const BYTES_IN_BLOCK: usize = 65536;

pub const LOG_BYTES_IN_BLOCK: usize = 16;

pub const __FLP_IDX_BLOCK: u8 = 4;

impl BlockAddr {
  pub const CELLS_OFFSET_BYTES: usize = 0;
  pub fn cells(&self) -> CellsAddr {
    self.plus::<CellsAddr>(BlockAddr::CELLS_OFFSET_BYTES)
  }
  pub fn from_cells(addr: CellsAddr) -> BlockAddr {
    addr.sub::<BlockAddr>(BlockAddr::CELLS_OFFSET_BYTES)
  }
  pub fn contains_cells(&self, addr: CellsAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_freeCell(&self, addr: FreeCellAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell(&self, addr: CellAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell_size(&self, addr: Cell_sizeAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_object(&self, addr: ObjectAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_0(&self, addr: Ptr_0Addr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_1(&self, addr: Ptr_1Addr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_2(&self, addr: Ptr_2Addr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_3(&self, addr: Ptr_3Addr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_payload(&self, addr: PayloadAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_remainder(&self, addr: RemainderAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_limit(&self, addr: LimitAddr, end: BlockAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_line(self) -> LineAddr {
    LineAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cells(self) -> CellsAddr {
    CellsAddr::from_usize(self.as_usize())
  }
  pub fn get_first_freeCell(self) -> FreeCellAddr {
    FreeCellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell(self) -> CellAddr {
    CellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell_size(self) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(self.as_usize())
  }
  pub fn jump_to_FreeBlock(&self, count: usize) -> FreeBlockAddr {
    self.plus(count << LOG_BYTES_IN_BLOCK)
  }
  pub fn jump_to_Block(&self, count: usize) -> BlockAddr {
    self.plus(count << LOG_BYTES_IN_BLOCK)
  }
  pub fn init_canonical_sequence(&self, a1: usize, a2: usize, a3: usize) -> (
    CellsAddr,
    RemainderAddr,
    LimitAddr,
    BlockAddrEnd,
  ) {
    let a0: usize = 0;
    let ret =
      (
        self.plus::<CellsAddr>(a0),
        self.plus::<CellsAddr>(a0).plus::<RemainderAddr>(a1),
        self.plus::<CellsAddr>(a0).plus::<RemainderAddr>(a1).plus::<LimitAddr>(a2),
        self
          .plus::<CellsAddr>(a0)
          .plus::<RemainderAddr>(a1)
          .plus::<LimitAddr>(a2)
          .plus::<BlockAddrEnd>(a3),
      );
    return ret;
  }
  pub fn size_of(&self, end: BlockAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_BLOCK;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Block(&self, val: Block) {
    self.store::<Block>(val);
  }
  pub fn load_Block(&self) -> Block {
    self.load::<Block>()
  }
}

impl Cell {
  pub fn memset_cell_size_until_object(val: u8, base: Cell_sizeAddr) {
    base.memset(val, BYTES_IN_CELL_SIZE)
  }
  pub fn init_object_after_cell_size(p1: Cell_sizeAddr) -> ObjectAddr {
    p1.plus(BYTES_IN_CELL_SIZE)
  }
  pub fn cell_size_is_validly_before_object(p1: Cell_sizeAddr, p2: ObjectAddr) -> bool {
    p1.lte(p2)
  }
  pub fn cast_word_to_cell_size(addr: WordAddr) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(addr.as_usize())
  }
  pub fn cast_cell_size_to_word(addr: Cell_sizeAddr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
  pub fn cast_word_to_object(addr: WordAddr) -> ObjectAddr {
    ObjectAddr::from_usize(addr.as_usize())
  }
  pub fn cast_object_to_word(addr: ObjectAddr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
  pub fn cast_word_to_ptr_0(addr: WordAddr) -> Ptr_0Addr {
    Ptr_0Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_0_to_word(addr: Ptr_0Addr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
  pub fn cast_word_to_ptr_1(addr: WordAddr) -> Ptr_1Addr {
    Ptr_1Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_1_to_word(addr: Ptr_1Addr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
  pub fn cast_word_to_ptr_2(addr: WordAddr) -> Ptr_2Addr {
    Ptr_2Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_2_to_word(addr: Ptr_2Addr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
  pub fn cast_word_to_ptr_3(addr: WordAddr) -> Ptr_3Addr {
    Ptr_3Addr::from_usize(addr.as_usize())
  }
  pub fn cast_ptr_3_to_word(addr: Ptr_3Addr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
  pub fn cast_word_to_payload(addr: WordAddr) -> PayloadAddr {
    PayloadAddr::from_usize(addr.as_usize())
  }
  pub fn cast_payload_to_word(addr: PayloadAddr) -> WordAddr {
    WordAddr::from_usize(addr.as_usize())
  }
}

pub const CELL_LOG_BYTES_ALIGN: usize = 3;

pub const CELL_BYTES_ALIGN: usize = 1 << CELL_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct CellAddr(usize);

deriveAddr! { CellAddr, CELL_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct CellAddrEnd(usize);

deriveAddr! { CellAddrEnd, 1 }

pub const __FLP_IDX_CELL: u8 = 7;

impl CellAddr {
  pub const CELL_SIZE_OFFSET_BYTES: usize = 0;
  pub fn cell_size(&self) -> Cell_sizeAddr {
    self.plus::<Cell_sizeAddr>(CellAddr::CELL_SIZE_OFFSET_BYTES)
  }
  pub fn from_cell_size(addr: Cell_sizeAddr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::CELL_SIZE_OFFSET_BYTES)
  }
  pub const OBJECT_OFFSET_BYTES: usize = 8;
  pub fn object(&self) -> ObjectAddr {
    self.plus::<ObjectAddr>(CellAddr::OBJECT_OFFSET_BYTES)
  }
  pub fn from_object(addr: ObjectAddr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::OBJECT_OFFSET_BYTES)
  }
  pub const PTR_0_OFFSET_BYTES: usize = 8;
  pub fn ptr_0(&self) -> Ptr_0Addr {
    self.plus::<Ptr_0Addr>(CellAddr::PTR_0_OFFSET_BYTES)
  }
  pub fn from_ptr_0(addr: Ptr_0Addr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::PTR_0_OFFSET_BYTES)
  }
  pub const PTR_1_OFFSET_BYTES: usize = 16;
  pub fn ptr_1(&self) -> Ptr_1Addr {
    self.plus::<Ptr_1Addr>(CellAddr::PTR_1_OFFSET_BYTES)
  }
  pub fn from_ptr_1(addr: Ptr_1Addr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::PTR_1_OFFSET_BYTES)
  }
  pub const PTR_2_OFFSET_BYTES: usize = 24;
  pub fn ptr_2(&self) -> Ptr_2Addr {
    self.plus::<Ptr_2Addr>(CellAddr::PTR_2_OFFSET_BYTES)
  }
  pub fn from_ptr_2(addr: Ptr_2Addr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::PTR_2_OFFSET_BYTES)
  }
  pub const PTR_3_OFFSET_BYTES: usize = 32;
  pub fn ptr_3(&self) -> Ptr_3Addr {
    self.plus::<Ptr_3Addr>(CellAddr::PTR_3_OFFSET_BYTES)
  }
  pub fn from_ptr_3(addr: Ptr_3Addr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::PTR_3_OFFSET_BYTES)
  }
  pub const PAYLOAD_OFFSET_BYTES: usize = 40;
  pub fn payload(&self) -> PayloadAddr {
    self.plus::<PayloadAddr>(CellAddr::PAYLOAD_OFFSET_BYTES)
  }
  pub fn from_payload(addr: PayloadAddr) -> CellAddr {
    addr.sub::<CellAddr>(CellAddr::PAYLOAD_OFFSET_BYTES)
  }
  pub fn contains_cell_size(&self, addr: Cell_sizeAddr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_object(&self, addr: ObjectAddr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_0(&self, addr: Ptr_0Addr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_1(&self, addr: Ptr_1Addr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_2(&self, addr: Ptr_2Addr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_3(&self, addr: Ptr_3Addr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_payload(&self, addr: PayloadAddr, end: CellAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell_size(self) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(self.as_usize())
  }
  pub fn skip_bytes_to_FreeCell(&self, bytes: usize) -> FreeCellAddr {
    self.plus(bytes)
  }
  pub fn skip_bytes_to_Cell(&self, bytes: usize) -> CellAddr {
    self.plus(bytes)
  }
  pub fn shadow_alloc_from_cells(&self, bytes: usize) { }
  pub fn size_of(&self, end: CellAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_CELL;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Cell_size { }

pub const CELL_SIZE_LOG_BYTES_ALIGN: usize = 0;

pub const CELL_SIZE_BYTES_ALIGN: usize = 1 << CELL_SIZE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Cell_sizeAddr(usize);

deriveAddr! { Cell_sizeAddr, CELL_SIZE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Cell_sizeAddrEnd(usize);

deriveAddr! { Cell_sizeAddrEnd, 1 }

pub const BYTES_IN_CELL_SIZE: usize = 8;

pub const LOG_BYTES_IN_CELL_SIZE: usize = 3;

pub const __FLP_IDX_CELL_SIZE: u8 = 8;

impl Cell_sizeAddr {
  pub fn size_of(&self, end: Cell_sizeAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_CELL_SIZE;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Cell_size(&self, val: Cell_size) {
    self.store::<Cell_size>(val);
  }
  pub fn load_Cell_size(&self) -> Cell_size {
    self.load::<Cell_size>()
  }
  pub fn store_word(&self, val: usize) {
    self.store::<usize>(val);
  }
}

impl Cell_sizeAddrEnd {
  pub fn to_ObjectAddr(&self) -> ObjectAddr {
    ObjectAddr::from_usize(self.as_usize())
  }
  pub fn from_ObjectAddr(ptr: ObjectAddr) -> Cell_sizeAddrEnd {
    Cell_sizeAddrEnd::from_usize(ptr.as_usize())
  }
}

impl Cells { }

pub const CELLS_LOG_BYTES_ALIGN: usize = 3;

pub const CELLS_BYTES_ALIGN: usize = 1 << CELLS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct CellsAddr(usize);

deriveAddr! { CellsAddr, CELLS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct CellsAddrEnd(usize);

deriveAddr! { CellsAddrEnd, 1 }

pub const __FLP_IDX_CELLS: u8 = 5;

impl CellsAddr {
  pub fn contains_freeCell(&self, addr: FreeCellAddr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell(&self, addr: CellAddr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell_size(&self, addr: Cell_sizeAddr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_object(&self, addr: ObjectAddr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_0(&self, addr: Ptr_0Addr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_1(&self, addr: Ptr_1Addr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_2(&self, addr: Ptr_2Addr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_3(&self, addr: Ptr_3Addr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_payload(&self, addr: PayloadAddr, end: CellsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_freeCell(self) -> FreeCellAddr {
    FreeCellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell(self) -> CellAddr {
    CellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell_size(self) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(self.as_usize())
  }
  pub fn shadow_alloc_from_block(&self, bytes: usize) { }
  pub fn size_of(&self, end: CellsAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_CELLS;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl CellsAddrEnd {
  pub fn to_RemainderAddr(&self) -> RemainderAddr {
    RemainderAddr::from_usize(self.as_usize())
  }
  pub fn from_RemainderAddr(ptr: RemainderAddr) -> CellsAddrEnd {
    CellsAddrEnd::from_usize(ptr.as_usize())
  }
}

impl Dequeue { }

pub const DEQUEUE_LOG_BYTES_ALIGN: usize = 0;

pub const DEQUEUE_BYTES_ALIGN: usize = 1 << DEQUEUE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct DequeueAddr(usize);

deriveAddr! { DequeueAddr, DEQUEUE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct DequeueAddrEnd(usize);

deriveAddr! { DequeueAddrEnd, 1 }

pub const BYTES_IN_DEQUEUE: usize = 16;

pub const LOG_BYTES_IN_DEQUEUE: usize = 4;

pub const __FLP_IDX_DEQUEUE: u8 = 33;

impl DequeueAddr {
  pub fn size_of(&self, end: DequeueAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_DEQUEUE;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_APPOBJECT, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Dequeue(&self, val: Dequeue) {
    self.store::<Dequeue>(val);
  }
  pub fn load_Dequeue(&self) -> Dequeue {
    self.load::<Dequeue>()
  }
}

impl FreeBlock { }

pub const FREEBLOCK_LOG_BYTES_ALIGN: usize = 16;

pub const FREEBLOCK_BYTES_ALIGN: usize = 1 << FREEBLOCK_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct FreeBlockAddr(usize);

deriveAddr! { FreeBlockAddr, FREEBLOCK_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct FreeBlockAddrEnd(usize);

deriveAddr! { FreeBlockAddrEnd, 1 }

pub const BYTES_IN_FREEBLOCK: usize = 65536;

pub const LOG_BYTES_IN_FREEBLOCK: usize = 16;

pub const __FLP_IDX_FREEBLOCK: u8 = 3;

impl FreeBlockAddr {
  pub fn jump_to_FreeBlock(&self, count: usize) -> FreeBlockAddr {
    self.plus(count << LOG_BYTES_IN_FREEBLOCK)
  }
  pub fn jump_to_Block(&self, count: usize) -> BlockAddr {
    self.plus(count << LOG_BYTES_IN_FREEBLOCK)
  }
  pub fn size_of(&self, end: FreeBlockAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_FREEBLOCK;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_FreeBlock(&self, val: FreeBlock) {
    self.store::<FreeBlock>(val);
  }
  pub fn load_FreeBlock(&self) -> FreeBlock {
    self.load::<FreeBlock>()
  }
}

impl FreeCell { }

pub const FREECELL_LOG_BYTES_ALIGN: usize = 3;

pub const FREECELL_BYTES_ALIGN: usize = 1 << FREECELL_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct FreeCellAddr(usize);

deriveAddr! { FreeCellAddr, FREECELL_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct FreeCellAddrEnd(usize);

deriveAddr! { FreeCellAddrEnd, 1 }

pub const __FLP_IDX_FREECELL: u8 = 6;

impl FreeCellAddr {
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn skip_bytes_to_FreeCell(&self, bytes: usize) -> FreeCellAddr {
    self.plus(bytes)
  }
  pub fn skip_bytes_to_Cell(&self, bytes: usize) -> CellAddr {
    self.plus(bytes)
  }
  pub fn shadow_alloc_from_cells(&self, bytes: usize) { }
  pub fn size_of(&self, end: FreeCellAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_FREECELL;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Garbage { }

pub const GARBAGE_LOG_BYTES_ALIGN: usize = 0;

pub const GARBAGE_BYTES_ALIGN: usize = 1 << GARBAGE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct GarbageAddr(usize);

deriveAddr! { GarbageAddr, GARBAGE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct GarbageAddrEnd(usize);

deriveAddr! { GarbageAddrEnd, 1 }

pub const __FLP_IDX_GARBAGE: u8 = 34;

impl GarbageAddr {
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn size_of(&self, end: GarbageAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_GARBAGE;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl LLNode { }

pub const LLNODE_LOG_BYTES_ALIGN: usize = 0;

pub const LLNODE_BYTES_ALIGN: usize = 1 << LLNODE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LLNodeAddr(usize);

deriveAddr! { LLNodeAddr, LLNODE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LLNodeAddrEnd(usize);

deriveAddr! { LLNodeAddrEnd, 1 }

pub const BYTES_IN_LLNODE: usize = 24;

pub const __FLP_IDX_LLNODE: u8 = 31;

impl LLNodeAddr {
  pub fn size_of(&self, end: LLNodeAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_LLNODE;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_APPOBJECT, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_LLNode(&self, val: LLNode) {
    self.store::<LLNode>(val);
  }
  pub fn load_LLNode(&self) -> LLNode {
    self.load::<LLNode>()
  }
}

impl Limit { }

pub const LIMIT_LOG_BYTES_ALIGN: usize = 0;

pub const LIMIT_BYTES_ALIGN: usize = 1 << LIMIT_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LimitAddr(usize);

deriveAddr! { LimitAddr, LIMIT_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LimitAddrEnd(usize);

deriveAddr! { LimitAddrEnd, 1 }

pub const BYTES_IN_LIMIT: usize = 0;

pub const __FLP_IDX_LIMIT: u8 = 16;

impl LimitAddr {
  pub fn to_RemainderAddrEnd(self) -> RemainderAddrEnd {
    RemainderAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_RemainderAddrEnd(ptr: RemainderAddrEnd) -> LimitAddr {
    LimitAddr::from_usize(ptr.as_usize())
  }
  pub fn size_of(&self, end: LimitAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_LIMIT;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn store_Limit(&self, val: Limit) {
    self.store::<Limit>(val);
  }
  pub fn load_Limit(&self) -> Limit {
    self.load::<Limit>()
  }
}

impl Line { }

pub struct Line2LineMark {
  pub fromStart: LineAddr,
  pub toStart: LineMarkAddr,
}

impl Line2LineMark {
  #[inline(always)]
  pub fn line_from_idx(base: LineAddr, idx: usize) -> LineAddr {
    let result = LineAddr::from_usize(base.as_usize() + (idx << 8));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: LineAddr, toStart: LineMarkAddr) -> Line2LineMark {
    Line2LineMark { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: LineAddr, value: LineMark) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: LineAddr) -> LineMarkAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: LineAddr) -> LineMark {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: LineAddr, elem: LineAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 8;
    result
  }
  #[inline(always)]
  pub fn map_set(fromStart: LineAddr, toStart: LineMarkAddr, fromAddr: LineAddr, value: LineMark) {
    let idxV = Line2LineMark::idx(fromStart, fromAddr);
    toStart.offset::<LineMark, LineMarkAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: LineMarkAddr, idxV: usize, value: LineMark) {
    toStart.offset::<LineMark, LineMarkAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: LineMarkAddr, idxV: usize) -> LineMark {
    toStart.offset::<LineMark, LineMarkAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(
    fromStart: LineAddr,
    toStart: LineMarkAddr,
    fromAddr: LineAddr,
  ) -> LineMarkAddr {
    let idxV = Line2LineMark::idx(fromStart, fromAddr);
    toStart.offset::<LineMark, LineMarkAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: LineAddr, toStart: LineMarkAddr, fromAddr: LineAddr) -> LineMark {
    let idxV = Line2LineMark::idx(fromStart, fromAddr);
    toStart.offset::<LineMark, LineMarkAddr>(idxV).load()
  }
}

pub const LINE_LOG_BYTES_ALIGN: usize = 8;

pub const LINE_BYTES_ALIGN: usize = 1 << LINE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LineAddr(usize);

deriveAddr! { LineAddr, LINE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LineAddrEnd(usize);

deriveAddr! { LineAddrEnd, 1 }

pub const BYTES_IN_LINE: usize = 256;

pub const LOG_BYTES_IN_LINE: usize = 8;

pub const __FLP_IDX_LINE: u8 = 17;

impl LineAddr {
  pub fn get_first_cell(self) -> CellAddr {
    CellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_byte(self) -> ByteAddr {
    ByteAddr::from_usize(self.as_usize())
  }
  pub fn jump_to_Line(&self, count: usize) -> LineAddr {
    self.plus(count << LOG_BYTES_IN_LINE)
  }
  pub fn size_of(&self, end: LineAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_LINE;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Line(&self, val: Line) {
    self.store::<Line>(val);
  }
  pub fn load_Line(&self) -> Line {
    self.load::<Line>()
  }
}

impl LineMark {
  #[inline(always)]
  pub fn from_enum(val: u8) -> Self {
    Self([ val ])
  }
  #[inline(always)]
  pub fn to_enum(&self) -> u8 {
    self.0[0]
  }
}

pub struct LineMark2Line {
  pub fromStart: LineMarkAddr,
  pub toStart: LineAddr,
}

impl LineMark2Line {
  #[inline(always)]
  pub fn lineMark_from_idx(base: LineMarkAddr, idx: usize) -> LineMarkAddr {
    let result = LineMarkAddr::from_usize(base.as_usize() + (idx << 0));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: LineMarkAddr, toStart: LineAddr) -> LineMark2Line {
    LineMark2Line { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: LineMarkAddr, value: Line) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: LineMarkAddr) -> LineAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: LineMarkAddr) -> Line {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: LineMarkAddr, elem: LineMarkAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 0;
    result
  }
  #[inline(always)]
  pub fn map_set(fromStart: LineMarkAddr, toStart: LineAddr, fromAddr: LineMarkAddr, value: Line) {
    let idxV = LineMark2Line::idx(fromStart, fromAddr);
    toStart.offset::<Line, LineAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: LineAddr, idxV: usize, value: Line) {
    toStart.offset::<Line, LineAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: LineAddr, idxV: usize) -> Line {
    toStart.offset::<Line, LineAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(
    fromStart: LineMarkAddr,
    toStart: LineAddr,
    fromAddr: LineMarkAddr,
  ) -> LineAddr {
    let idxV = LineMark2Line::idx(fromStart, fromAddr);
    toStart.offset::<Line, LineAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: LineMarkAddr, toStart: LineAddr, fromAddr: LineMarkAddr) -> Line {
    let idxV = LineMark2Line::idx(fromStart, fromAddr);
    toStart.offset::<Line, LineAddr>(idxV).load()
  }
}

pub const LINEMARK_LOG_BYTES_ALIGN: usize = 0;

pub const LINEMARK_BYTES_ALIGN: usize = 1 << LINEMARK_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LineMarkAddr(usize);

deriveAddr! { LineMarkAddr, LINEMARK_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LineMarkAddrEnd(usize);

deriveAddr! { LineMarkAddrEnd, 1 }

pub const BYTES_IN_LINEMARK: usize = 1;

pub const LOG_BYTES_IN_LINEMARK: usize = 0;

pub const __FLP_IDX_LINEMARK: u8 = 19;

impl LineMarkAddr {
  pub fn jump_to_LineMark(&self, count: usize) -> LineMarkAddr {
    self.plus(count << LOG_BYTES_IN_LINEMARK)
  }
  pub fn shadow_alloc_from_lms(&self) { }
  pub fn size_of(&self, end: LineMarkAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_LINEMARK;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_LMS, __FLP_IDX_REGION, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_LineMark(&self, val: LineMark) {
    self.store::<LineMark>(val);
  }
  pub fn load_LineMark(&self) -> LineMark {
    self.load::<LineMark>()
  }
}

impl Lms { }

pub const LMS_LOG_BYTES_ALIGN: usize = 0;

pub const LMS_BYTES_ALIGN: usize = 1 << LMS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LmsAddr(usize);

deriveAddr! { LmsAddr, LMS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LmsAddrEnd(usize);

deriveAddr! { LmsAddrEnd, 1 }

pub const __FLP_IDX_LMS: u8 = 18;

impl LmsAddr {
  pub fn contains_lineMark(&self, addr: LineMarkAddr, end: LmsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_lineMark(self) -> LineMarkAddr {
    LineMarkAddr::from_usize(self.as_usize())
  }
  pub fn to_SpaceAddrEnd(self) -> SpaceAddrEnd {
    SpaceAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_SpaceAddrEnd(ptr: SpaceAddrEnd) -> LmsAddr {
    LmsAddr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_region(&self, bytes: usize) { }
  pub fn size_of(&self, end: LmsAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_LMS;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_REGION, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl LmsAddrEnd {
  pub fn to_RefsAddr(&self) -> RefsAddr {
    RefsAddr::from_usize(self.as_usize())
  }
  pub fn from_RefsAddr(ptr: RefsAddr) -> LmsAddrEnd {
    LmsAddrEnd::from_usize(ptr.as_usize())
  }
}

impl LowWater { }

pub const LOWWATER_LOG_BYTES_ALIGN: usize = 0;

pub const LOWWATER_BYTES_ALIGN: usize = 1 << LOWWATER_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LowWaterAddr(usize);

deriveAddr! { LowWaterAddr, LOWWATER_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct LowWaterAddrEnd(usize);

deriveAddr! { LowWaterAddrEnd, 1 }

pub const BYTES_IN_LOWWATER: usize = 0;

pub const __FLP_IDX_LOWWATER: u8 = 26;

impl LowWaterAddr {
  pub fn to_StackAddrEnd(self) -> StackAddrEnd {
    StackAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_StackAddrEnd(ptr: StackAddrEnd) -> LowWaterAddr {
    LowWaterAddr::from_usize(ptr.as_usize())
  }
  pub fn size_of(&self, end: LowWaterAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_LOWWATER;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_STK, __FLP_IDX_UNMAPPED ];
  pub fn store_LowWater(&self, val: LowWater) {
    self.store::<LowWater>(val);
  }
  pub fn load_LowWater(&self) -> LowWater {
    self.load::<LowWater>()
  }
}

impl MarkBits {
  pub fn get_MARK_bits(&self) -> u8 {
    self.0[0]
  }
  pub fn set_MARK_from_u8(v: u8) -> Self {
    Self([ v ])
  }
  pub const MARK_NUM_BITS: usize = 8;
}

pub struct MarkBits2RefBits {
  pub fromStart: MarkBitsAddr,
  pub toStart: RefBitsAddr,
}

impl MarkBits2RefBits {
  #[inline(always)]
  pub fn markBits_from_idx(base: MarkBitsAddr, idx: usize) -> MarkBitsAddr {
    let result = MarkBitsAddr::from_usize(base.as_usize() + (idx << 0));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: MarkBitsAddr, toStart: RefBitsAddr) -> MarkBits2RefBits {
    MarkBits2RefBits { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: MarkBitsAddr, value: RefBits) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: MarkBitsAddr) -> RefBitsAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: MarkBitsAddr) -> RefBits {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: MarkBitsAddr, elem: MarkBitsAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 0;
    result
  }
  #[inline(always)]
  pub fn map_set(
    fromStart: MarkBitsAddr,
    toStart: RefBitsAddr,
    fromAddr: MarkBitsAddr,
    value: RefBits,
  ) {
    let idxV = MarkBits2RefBits::idx(fromStart, fromAddr);
    toStart.offset::<RefBits, RefBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: RefBitsAddr, idxV: usize, value: RefBits) {
    toStart.offset::<RefBits, RefBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: RefBitsAddr, idxV: usize) -> RefBits {
    toStart.offset::<RefBits, RefBitsAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(
    fromStart: MarkBitsAddr,
    toStart: RefBitsAddr,
    fromAddr: MarkBitsAddr,
  ) -> RefBitsAddr {
    let idxV = MarkBits2RefBits::idx(fromStart, fromAddr);
    toStart.offset::<RefBits, RefBitsAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: MarkBitsAddr, toStart: RefBitsAddr, fromAddr: MarkBitsAddr) -> RefBits {
    let idxV = MarkBits2RefBits::idx(fromStart, fromAddr);
    toStart.offset::<RefBits, RefBitsAddr>(idxV).load()
  }
}

pub struct MarkBits2Word {
  pub fromStart: MarkBitsAddr,
  pub toStart: WordAddr,
}

impl MarkBits2Word {
  #[inline(always)]
  pub fn markBits_from_idx(base: MarkBitsAddr, idx: usize) -> MarkBitsAddr {
    let result = MarkBitsAddr::from_usize(base.as_usize() + (idx << 0));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: MarkBitsAddr, toStart: WordAddr) -> MarkBits2Word {
    MarkBits2Word { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: MarkBitsAddr, value: Word) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: MarkBitsAddr) -> WordAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: MarkBitsAddr) -> Word {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: MarkBitsAddr, elem: MarkBitsAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 0;
    result
  }
  #[inline(always)]
  pub fn map_set(fromStart: MarkBitsAddr, toStart: WordAddr, fromAddr: MarkBitsAddr, value: Word) {
    let idxV = MarkBits2Word::idx(fromStart, fromAddr);
    toStart.offset::<Word, WordAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: WordAddr, idxV: usize, value: Word) {
    toStart.offset::<Word, WordAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: WordAddr, idxV: usize) -> Word {
    toStart.offset::<Word, WordAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(
    fromStart: MarkBitsAddr,
    toStart: WordAddr,
    fromAddr: MarkBitsAddr,
  ) -> WordAddr {
    let idxV = MarkBits2Word::idx(fromStart, fromAddr);
    toStart.offset::<Word, WordAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: MarkBitsAddr, toStart: WordAddr, fromAddr: MarkBitsAddr) -> Word {
    let idxV = MarkBits2Word::idx(fromStart, fromAddr);
    toStart.offset::<Word, WordAddr>(idxV).load()
  }
}

pub const MARKBITS_LOG_BYTES_ALIGN: usize = 0;

pub const MARKBITS_BYTES_ALIGN: usize = 1 << MARKBITS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct MarkBitsAddr(usize);

deriveAddr! { MarkBitsAddr, MARKBITS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct MarkBitsAddrEnd(usize);

deriveAddr! { MarkBitsAddrEnd, 1 }

pub const BYTES_IN_MARKBITS: usize = 1;

pub const LOG_BYTES_IN_MARKBITS: usize = 0;

pub const __FLP_IDX_MARKBITS: u8 = 23;

impl MarkBitsAddr {
  pub fn jump_to_MarkBits(&self, count: usize) -> MarkBitsAddr {
    self.plus(count << LOG_BYTES_IN_MARKBITS)
  }
  pub fn shadow_alloc_from_mks(&self) { }
  pub fn size_of(&self, end: MarkBitsAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_MARKBITS;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_MKS, __FLP_IDX_REGION, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_MarkBits(&self, val: MarkBits) {
    self.store::<MarkBits>(val);
  }
  pub fn load_MarkBits(&self) -> MarkBits {
    self.load::<MarkBits>()
  }
}

impl Mks { }

pub const MKS_LOG_BYTES_ALIGN: usize = 0;

pub const MKS_BYTES_ALIGN: usize = 1 << MKS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct MksAddr(usize);

deriveAddr! { MksAddr, MKS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct MksAddrEnd(usize);

deriveAddr! { MksAddrEnd, 1 }

pub const __FLP_IDX_MKS: u8 = 22;

impl MksAddr {
  pub fn contains_markBits(&self, addr: MarkBitsAddr, end: MksAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_markBits(self) -> MarkBitsAddr {
    MarkBitsAddr::from_usize(self.as_usize())
  }
  pub fn to_RefsAddrEnd(self) -> RefsAddrEnd {
    RefsAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_RefsAddrEnd(ptr: RefsAddrEnd) -> MksAddr {
    MksAddr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_region(&self, bytes: usize) { }
  pub fn size_of(&self, end: MksAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_MKS;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_REGION, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Object {
  pub fn memset_ptr_0_until_ptr_1(val: u8, base: Ptr_0Addr) {
    base.memset(val, BYTES_IN_PTR_0)
  }
  pub fn init_ptr_1_after_ptr_0(p1: Ptr_0Addr) -> Ptr_1Addr {
    p1.plus(BYTES_IN_PTR_0)
  }
  pub fn ptr_0_is_validly_before_ptr_1(p1: Ptr_0Addr, p2: Ptr_1Addr) -> bool {
    p1.lte(p2)
  }
  pub fn memset_ptr_1_until_ptr_2(val: u8, base: Ptr_1Addr) {
    base.memset(val, BYTES_IN_PTR_1)
  }
  pub fn init_ptr_2_after_ptr_1(p1: Ptr_1Addr) -> Ptr_2Addr {
    p1.plus(BYTES_IN_PTR_1)
  }
  pub fn ptr_1_is_validly_before_ptr_2(p1: Ptr_1Addr, p2: Ptr_2Addr) -> bool {
    p1.lte(p2)
  }
  pub fn memset_ptr_2_until_ptr_3(val: u8, base: Ptr_2Addr) {
    base.memset(val, BYTES_IN_PTR_2)
  }
  pub fn init_ptr_3_after_ptr_2(p1: Ptr_2Addr) -> Ptr_3Addr {
    p1.plus(BYTES_IN_PTR_2)
  }
  pub fn ptr_2_is_validly_before_ptr_3(p1: Ptr_2Addr, p2: Ptr_3Addr) -> bool {
    p1.lte(p2)
  }
  pub fn memset_ptr_3_until_payload(val: u8, base: Ptr_3Addr) {
    base.memset(val, BYTES_IN_PTR_3)
  }
  pub fn init_payload_after_ptr_3(p1: Ptr_3Addr) -> PayloadAddr {
    p1.plus(BYTES_IN_PTR_3)
  }
  pub fn ptr_3_is_validly_before_payload(p1: Ptr_3Addr, p2: PayloadAddr) -> bool {
    p1.lte(p2)
  }
}

pub const OBJECT_LOG_BYTES_ALIGN: usize = 0;

pub const OBJECT_BYTES_ALIGN: usize = 1 << OBJECT_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct ObjectAddr(usize);

deriveAddr! { ObjectAddr, OBJECT_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct ObjectAddrEnd(usize);

deriveAddr! { ObjectAddrEnd, 1 }

pub const __FLP_IDX_OBJECT: u8 = 9;

impl ObjectAddr {
  pub const PTR_0_OFFSET_BYTES: usize = 0;
  pub fn ptr_0(&self) -> Ptr_0Addr {
    self.plus::<Ptr_0Addr>(ObjectAddr::PTR_0_OFFSET_BYTES)
  }
  pub fn from_ptr_0(addr: Ptr_0Addr) -> ObjectAddr {
    addr.sub::<ObjectAddr>(ObjectAddr::PTR_0_OFFSET_BYTES)
  }
  pub const PTR_1_OFFSET_BYTES: usize = 8;
  pub fn ptr_1(&self) -> Ptr_1Addr {
    self.plus::<Ptr_1Addr>(ObjectAddr::PTR_1_OFFSET_BYTES)
  }
  pub fn from_ptr_1(addr: Ptr_1Addr) -> ObjectAddr {
    addr.sub::<ObjectAddr>(ObjectAddr::PTR_1_OFFSET_BYTES)
  }
  pub const PTR_2_OFFSET_BYTES: usize = 16;
  pub fn ptr_2(&self) -> Ptr_2Addr {
    self.plus::<Ptr_2Addr>(ObjectAddr::PTR_2_OFFSET_BYTES)
  }
  pub fn from_ptr_2(addr: Ptr_2Addr) -> ObjectAddr {
    addr.sub::<ObjectAddr>(ObjectAddr::PTR_2_OFFSET_BYTES)
  }
  pub const PTR_3_OFFSET_BYTES: usize = 24;
  pub fn ptr_3(&self) -> Ptr_3Addr {
    self.plus::<Ptr_3Addr>(ObjectAddr::PTR_3_OFFSET_BYTES)
  }
  pub fn from_ptr_3(addr: Ptr_3Addr) -> ObjectAddr {
    addr.sub::<ObjectAddr>(ObjectAddr::PTR_3_OFFSET_BYTES)
  }
  pub const PAYLOAD_OFFSET_BYTES: usize = 32;
  pub fn payload(&self) -> PayloadAddr {
    self.plus::<PayloadAddr>(ObjectAddr::PAYLOAD_OFFSET_BYTES)
  }
  pub fn from_payload(addr: PayloadAddr) -> ObjectAddr {
    addr.sub::<ObjectAddr>(ObjectAddr::PAYLOAD_OFFSET_BYTES)
  }
  pub fn contains_ptr_0(&self, addr: Ptr_0Addr, end: ObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_1(&self, addr: Ptr_1Addr, end: ObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_2(&self, addr: Ptr_2Addr, end: ObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_3(&self, addr: Ptr_3Addr, end: ObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_payload(&self, addr: PayloadAddr, end: ObjectAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_ptr_0(self) -> Ptr_0Addr {
    Ptr_0Addr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn to_Cell_sizeAddrEnd(self) -> Cell_sizeAddrEnd {
    Cell_sizeAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_Cell_sizeAddrEnd(ptr: Cell_sizeAddrEnd) -> ObjectAddr {
    ObjectAddr::from_usize(ptr.as_usize())
  }
  pub fn init_canonical_sequence(&self, a1: usize, a2: usize, a3: usize, a4: usize, a5: usize) -> (
    Ptr_0Addr,
    Ptr_1Addr,
    Ptr_2Addr,
    Ptr_3Addr,
    PayloadAddr,
    ObjectAddrEnd,
  ) {
    let a0: usize = 0;
    let ret =
      (
        self.plus::<Ptr_0Addr>(a0),
        self.plus::<Ptr_0Addr>(a0).plus::<Ptr_1Addr>(a1),
        self.plus::<Ptr_0Addr>(a0).plus::<Ptr_1Addr>(a1).plus::<Ptr_2Addr>(a2),
        self
          .plus::<Ptr_0Addr>(a0)
          .plus::<Ptr_1Addr>(a1)
          .plus::<Ptr_2Addr>(a2)
          .plus::<Ptr_3Addr>(a3),
        self
          .plus::<Ptr_0Addr>(a0)
          .plus::<Ptr_1Addr>(a1)
          .plus::<Ptr_2Addr>(a2)
          .plus::<Ptr_3Addr>(a3)
          .plus::<PayloadAddr>(a4),
        self
          .plus::<Ptr_0Addr>(a0)
          .plus::<Ptr_1Addr>(a1)
          .plus::<Ptr_2Addr>(a2)
          .plus::<Ptr_3Addr>(a3)
          .plus::<PayloadAddr>(a4)
          .plus::<ObjectAddrEnd>(a5),
      );
    return ret;
  }
  pub fn size_of(&self, end: ObjectAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_OBJECT;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct ObjectAddrAddr(usize);

pub const OBJECTADDR_BYTES_ALIGN: usize = 1;

deriveAddr! { ObjectAddrAddr, OBJECTADDR_BYTES_ALIGN }

impl ObjectAddrAddr {
  pub fn objectAddr(&self) -> ObjectAddr {
    self.load()
  }
}

impl Payload { }

pub const PAYLOAD_LOG_BYTES_ALIGN: usize = 0;

pub const PAYLOAD_BYTES_ALIGN: usize = 1 << PAYLOAD_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct PayloadAddr(usize);

deriveAddr! { PayloadAddr, PAYLOAD_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct PayloadAddrEnd(usize);

deriveAddr! { PayloadAddrEnd, 1 }

pub const __FLP_IDX_PAYLOAD: u8 = 14;

impl PayloadAddr {
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn to_Ptr_3AddrEnd(self) -> Ptr_3AddrEnd {
    Ptr_3AddrEnd::from_usize(self.as_usize())
  }
  pub fn from_Ptr_3AddrEnd(ptr: Ptr_3AddrEnd) -> PayloadAddr {
    PayloadAddr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_object(&self, bytes: usize) { }
  pub fn size_of(&self, end: PayloadAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_PAYLOAD;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_OBJECT,
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Ptr_0 { }

pub const PTR_0_LOG_BYTES_ALIGN: usize = 0;

pub const PTR_0_BYTES_ALIGN: usize = 1 << PTR_0_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_0Addr(usize);

deriveAddr! { Ptr_0Addr, PTR_0_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_0AddrEnd(usize);

deriveAddr! { Ptr_0AddrEnd, 1 }

pub const BYTES_IN_PTR_0: usize = 8;

pub const LOG_BYTES_IN_PTR_0: usize = 3;

pub const __FLP_IDX_PTR_0: u8 = 10;

impl Ptr_0Addr {
  pub fn load_ObjectAddr(self) -> ObjectAddr {
    let ret = self.load::<ObjectAddr>();
    ret
  }
  pub fn store_ObjectAddr(self, val: ObjectAddr) {
    self.store::<ObjectAddr>(val)
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn shadow_alloc_from_object(&self) { }
  pub fn size_of(&self, end: Ptr_0AddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_PTR_0;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_OBJECT,
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Ptr_0(&self, val: Ptr_0) {
    self.store::<Ptr_0>(val);
  }
  pub fn load_Ptr_0(&self) -> Ptr_0 {
    self.load::<Ptr_0>()
  }
  pub fn store_word(&self, val: usize) {
    self.store::<usize>(val);
  }
}

impl Ptr_0AddrEnd {
  pub fn to_Ptr_1Addr(&self) -> Ptr_1Addr {
    Ptr_1Addr::from_usize(self.as_usize())
  }
  pub fn from_Ptr_1Addr(ptr: Ptr_1Addr) -> Ptr_0AddrEnd {
    Ptr_0AddrEnd::from_usize(ptr.as_usize())
  }
}

impl Ptr_1 { }

pub const PTR_1_LOG_BYTES_ALIGN: usize = 0;

pub const PTR_1_BYTES_ALIGN: usize = 1 << PTR_1_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_1Addr(usize);

deriveAddr! { Ptr_1Addr, PTR_1_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_1AddrEnd(usize);

deriveAddr! { Ptr_1AddrEnd, 1 }

pub const BYTES_IN_PTR_1: usize = 8;

pub const LOG_BYTES_IN_PTR_1: usize = 3;

pub const __FLP_IDX_PTR_1: u8 = 11;

impl Ptr_1Addr {
  pub fn load_ObjectAddr(self) -> ObjectAddr {
    let ret = self.load::<ObjectAddr>();
    ret
  }
  pub fn store_ObjectAddr(self, val: ObjectAddr) {
    self.store::<ObjectAddr>(val)
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn to_Ptr_0AddrEnd(self) -> Ptr_0AddrEnd {
    Ptr_0AddrEnd::from_usize(self.as_usize())
  }
  pub fn from_Ptr_0AddrEnd(ptr: Ptr_0AddrEnd) -> Ptr_1Addr {
    Ptr_1Addr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_object(&self) { }
  pub fn size_of(&self, end: Ptr_1AddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_PTR_1;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_OBJECT,
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Ptr_1(&self, val: Ptr_1) {
    self.store::<Ptr_1>(val);
  }
  pub fn load_Ptr_1(&self) -> Ptr_1 {
    self.load::<Ptr_1>()
  }
  pub fn store_word(&self, val: usize) {
    self.store::<usize>(val);
  }
}

impl Ptr_1AddrEnd {
  pub fn to_Ptr_2Addr(&self) -> Ptr_2Addr {
    Ptr_2Addr::from_usize(self.as_usize())
  }
  pub fn from_Ptr_2Addr(ptr: Ptr_2Addr) -> Ptr_1AddrEnd {
    Ptr_1AddrEnd::from_usize(ptr.as_usize())
  }
}

impl Ptr_2 { }

pub const PTR_2_LOG_BYTES_ALIGN: usize = 0;

pub const PTR_2_BYTES_ALIGN: usize = 1 << PTR_2_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_2Addr(usize);

deriveAddr! { Ptr_2Addr, PTR_2_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_2AddrEnd(usize);

deriveAddr! { Ptr_2AddrEnd, 1 }

pub const BYTES_IN_PTR_2: usize = 8;

pub const LOG_BYTES_IN_PTR_2: usize = 3;

pub const __FLP_IDX_PTR_2: u8 = 12;

impl Ptr_2Addr {
  pub fn load_ObjectAddr(self) -> ObjectAddr {
    let ret = self.load::<ObjectAddr>();
    ret
  }
  pub fn store_ObjectAddr(self, val: ObjectAddr) {
    self.store::<ObjectAddr>(val)
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn to_Ptr_1AddrEnd(self) -> Ptr_1AddrEnd {
    Ptr_1AddrEnd::from_usize(self.as_usize())
  }
  pub fn from_Ptr_1AddrEnd(ptr: Ptr_1AddrEnd) -> Ptr_2Addr {
    Ptr_2Addr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_object(&self) { }
  pub fn size_of(&self, end: Ptr_2AddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_PTR_2;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_OBJECT,
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Ptr_2(&self, val: Ptr_2) {
    self.store::<Ptr_2>(val);
  }
  pub fn load_Ptr_2(&self) -> Ptr_2 {
    self.load::<Ptr_2>()
  }
  pub fn store_word(&self, val: usize) {
    self.store::<usize>(val);
  }
}

impl Ptr_2AddrEnd {
  pub fn to_Ptr_3Addr(&self) -> Ptr_3Addr {
    Ptr_3Addr::from_usize(self.as_usize())
  }
  pub fn from_Ptr_3Addr(ptr: Ptr_3Addr) -> Ptr_2AddrEnd {
    Ptr_2AddrEnd::from_usize(ptr.as_usize())
  }
}

impl Ptr_3 { }

pub const PTR_3_LOG_BYTES_ALIGN: usize = 0;

pub const PTR_3_BYTES_ALIGN: usize = 1 << PTR_3_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_3Addr(usize);

deriveAddr! { Ptr_3Addr, PTR_3_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Ptr_3AddrEnd(usize);

deriveAddr! { Ptr_3AddrEnd, 1 }

pub const BYTES_IN_PTR_3: usize = 8;

pub const LOG_BYTES_IN_PTR_3: usize = 3;

pub const __FLP_IDX_PTR_3: u8 = 13;

impl Ptr_3Addr {
  pub fn load_ObjectAddr(self) -> ObjectAddr {
    let ret = self.load::<ObjectAddr>();
    ret
  }
  pub fn store_ObjectAddr(self, val: ObjectAddr) {
    self.store::<ObjectAddr>(val)
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn to_Ptr_2AddrEnd(self) -> Ptr_2AddrEnd {
    Ptr_2AddrEnd::from_usize(self.as_usize())
  }
  pub fn from_Ptr_2AddrEnd(ptr: Ptr_2AddrEnd) -> Ptr_3Addr {
    Ptr_3Addr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_object(&self) { }
  pub fn size_of(&self, end: Ptr_3AddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_PTR_3;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_OBJECT,
    __FLP_IDX_CELL,
    __FLP_IDX_CELLS,
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_Ptr_3(&self, val: Ptr_3) {
    self.store::<Ptr_3>(val);
  }
  pub fn load_Ptr_3(&self) -> Ptr_3 {
    self.load::<Ptr_3>()
  }
  pub fn store_word(&self, val: usize) {
    self.store::<usize>(val);
  }
}

impl Ptr_3AddrEnd {
  pub fn to_PayloadAddr(&self) -> PayloadAddr {
    PayloadAddr::from_usize(self.as_usize())
  }
  pub fn from_PayloadAddr(ptr: PayloadAddr) -> Ptr_3AddrEnd {
    Ptr_3AddrEnd::from_usize(ptr.as_usize())
  }
}

impl QNode { }

pub const QNODE_LOG_BYTES_ALIGN: usize = 0;

pub const QNODE_BYTES_ALIGN: usize = 1 << QNODE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct QNodeAddr(usize);

deriveAddr! { QNodeAddr, QNODE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct QNodeAddrEnd(usize);

deriveAddr! { QNodeAddrEnd, 1 }

pub const BYTES_IN_QNODE: usize = 16;

pub const LOG_BYTES_IN_QNODE: usize = 4;

pub const __FLP_IDX_QNODE: u8 = 32;

impl QNodeAddr {
  pub fn size_of(&self, end: QNodeAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_QNODE;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_APPOBJECT, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_QNode(&self, val: QNode) {
    self.store::<QNode>(val);
  }
  pub fn load_QNode(&self) -> QNode {
    self.load::<QNode>()
  }
}

impl RefBits {
  pub const REF_LOW_BIT: usize = 2;
  pub const REF_NUM_BITS: usize = 6;
  pub const REF_MASK: u8 = 0b11111100;
  pub fn get_REF_bits(&self) -> u8 {
    (self.0[0]) & Self::REF_MASK
  }
  pub fn set_REF_from_u8(v: u8) -> Self {
    Self([ v ])
  }
  pub const OBJ_START_LOW_BIT: usize = 1;
  pub const OBJ_START_NUM_BITS: usize = 1;
  pub const OBJ_START_MASK: u8 = 0b00000010;
  pub fn get_OBJ_START_bit(&self) -> bool {
    ((self.0[0]) & Self::OBJ_START_MASK) > 0
  }
  pub fn set_OBJ_START_from_bool(b: bool) -> Self {
    Self([ (b as u8) << Self::OBJ_START_LOW_BIT ])
  }
  pub const SHORT_ENCODE_LOW_BIT: usize = 0;
  pub const SHORT_ENCODE_NUM_BITS: usize = 1;
  pub const SHORT_ENCODE_MASK: u8 = 0b00000001;
  pub fn get_SHORT_ENCODE_bit(&self) -> bool {
    ((self.0[0]) & Self::SHORT_ENCODE_MASK) > 0
  }
  pub fn set_SHORT_ENCODE_from_bool(b: bool) -> Self {
    Self([ (b as u8) << Self::SHORT_ENCODE_LOW_BIT ])
  }
}

pub struct RefBits2MarkBits {
  pub fromStart: RefBitsAddr,
  pub toStart: MarkBitsAddr,
}

impl RefBits2MarkBits {
  #[inline(always)]
  pub fn refBits_from_idx(base: RefBitsAddr, idx: usize) -> RefBitsAddr {
    let result = RefBitsAddr::from_usize(base.as_usize() + (idx << 0));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: RefBitsAddr, toStart: MarkBitsAddr) -> RefBits2MarkBits {
    RefBits2MarkBits { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: RefBitsAddr, value: MarkBits) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: RefBitsAddr) -> MarkBitsAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: RefBitsAddr) -> MarkBits {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: RefBitsAddr, elem: RefBitsAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 0;
    result
  }
  #[inline(always)]
  pub fn map_set(
    fromStart: RefBitsAddr,
    toStart: MarkBitsAddr,
    fromAddr: RefBitsAddr,
    value: MarkBits,
  ) {
    let idxV = RefBits2MarkBits::idx(fromStart, fromAddr);
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: MarkBitsAddr, idxV: usize, value: MarkBits) {
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: MarkBitsAddr, idxV: usize) -> MarkBits {
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(
    fromStart: RefBitsAddr,
    toStart: MarkBitsAddr,
    fromAddr: RefBitsAddr,
  ) -> MarkBitsAddr {
    let idxV = RefBits2MarkBits::idx(fromStart, fromAddr);
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: RefBitsAddr, toStart: MarkBitsAddr, fromAddr: RefBitsAddr) -> MarkBits {
    let idxV = RefBits2MarkBits::idx(fromStart, fromAddr);
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).load()
  }
}

pub struct RefBits2Word {
  pub fromStart: RefBitsAddr,
  pub toStart: WordAddr,
}

impl RefBits2Word {
  #[inline(always)]
  pub fn refBits_from_idx(base: RefBitsAddr, idx: usize) -> RefBitsAddr {
    let result = RefBitsAddr::from_usize(base.as_usize() + (idx << 0));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: RefBitsAddr, toStart: WordAddr) -> RefBits2Word {
    RefBits2Word { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: RefBitsAddr, value: Word) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: RefBitsAddr) -> WordAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: RefBitsAddr) -> Word {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: RefBitsAddr, elem: RefBitsAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 0;
    result
  }
  #[inline(always)]
  pub fn map_set(fromStart: RefBitsAddr, toStart: WordAddr, fromAddr: RefBitsAddr, value: Word) {
    let idxV = RefBits2Word::idx(fromStart, fromAddr);
    toStart.offset::<Word, WordAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: WordAddr, idxV: usize, value: Word) {
    toStart.offset::<Word, WordAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: WordAddr, idxV: usize) -> Word {
    toStart.offset::<Word, WordAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(fromStart: RefBitsAddr, toStart: WordAddr, fromAddr: RefBitsAddr) -> WordAddr {
    let idxV = RefBits2Word::idx(fromStart, fromAddr);
    toStart.offset::<Word, WordAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: RefBitsAddr, toStart: WordAddr, fromAddr: RefBitsAddr) -> Word {
    let idxV = RefBits2Word::idx(fromStart, fromAddr);
    toStart.offset::<Word, WordAddr>(idxV).load()
  }
}

pub const REFBITS_LOG_BYTES_ALIGN: usize = 0;

pub const REFBITS_BYTES_ALIGN: usize = 1 << REFBITS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RefBitsAddr(usize);

deriveAddr! { RefBitsAddr, REFBITS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RefBitsAddrEnd(usize);

deriveAddr! { RefBitsAddrEnd, 1 }

pub const BYTES_IN_REFBITS: usize = 1;

pub const LOG_BYTES_IN_REFBITS: usize = 0;

pub const __FLP_IDX_REFBITS: u8 = 21;

impl RefBitsAddr {
  pub fn jump_to_RefBits(&self, count: usize) -> RefBitsAddr {
    self.plus(count << LOG_BYTES_IN_REFBITS)
  }
  pub fn shadow_alloc_from_refs(&self) { }
  pub fn size_of(&self, end: RefBitsAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REFBITS;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_REFS,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, vec_IDX: Vec<u8>) { }
  pub fn store_RefBits(&self, val: RefBits) {
    self.store::<RefBits>(val);
  }
  pub fn load_RefBits(&self) -> RefBits {
    self.load::<RefBits>()
  }
}

impl Refs { }

pub const REFS_LOG_BYTES_ALIGN: usize = 0;

pub const REFS_BYTES_ALIGN: usize = 1 << REFS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RefsAddr(usize);

deriveAddr! { RefsAddr, REFS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RefsAddrEnd(usize);

deriveAddr! { RefsAddrEnd, 1 }

pub const __FLP_IDX_REFS: u8 = 20;

impl RefsAddr {
  pub fn contains_refBits(&self, addr: RefBitsAddr, end: RefsAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_refBits(self) -> RefBitsAddr {
    RefBitsAddr::from_usize(self.as_usize())
  }
  pub fn to_LmsAddrEnd(self) -> LmsAddrEnd {
    LmsAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_LmsAddrEnd(ptr: LmsAddrEnd) -> RefsAddr {
    RefsAddr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_region(&self, bytes: usize) { }
  pub fn size_of(&self, end: RefsAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REFS;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_REGION, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl RefsAddrEnd {
  pub fn to_MksAddr(&self) -> MksAddr {
    MksAddr::from_usize(self.as_usize())
  }
  pub fn from_MksAddr(ptr: MksAddr) -> RefsAddrEnd {
    RefsAddrEnd::from_usize(ptr.as_usize())
  }
}

impl Region {
  pub fn memset_space_until_lms(val: u8, base: SpaceAddr, mx: LmsAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn init_lms_after_space(p1: SpaceAddr, bytes: usize) -> LmsAddr {
    p1.plus(bytes)
  }
  pub fn space_is_validly_before_lms(p1: SpaceAddr, p2: LmsAddr) -> bool {
    p1.lte(p2)
  }
  pub fn memset_lms_until_refs(val: u8, base: LmsAddr, mx: RefsAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn init_refs_after_lms(p1: LmsAddr, bytes: usize) -> RefsAddr {
    p1.plus(bytes)
  }
  pub fn lms_is_validly_before_refs(p1: LmsAddr, p2: RefsAddr) -> bool {
    p1.lte(p2)
  }
  pub fn memset_refs_until_mks(val: u8, base: RefsAddr, mx: MksAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn init_mks_after_refs(p1: RefsAddr, bytes: usize) -> MksAddr {
    p1.plus(bytes)
  }
  pub fn refs_is_validly_before_mks(p1: RefsAddr, p2: MksAddr) -> bool {
    p1.lte(p2)
  }
}

pub const REGION_LOG_BYTES_ALIGN: usize = 19;

pub const REGION_BYTES_ALIGN: usize = 1 << REGION_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegionAddr(usize);

deriveAddr! { RegionAddr, REGION_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegionAddrEnd(usize);

deriveAddr! { RegionAddrEnd, 1 }

pub const __FLP_IDX_REGION: u8 = 1;

impl RegionAddr {
  pub const SPACE_OFFSET_BYTES: usize = 0;
  pub fn space(&self) -> SpaceAddr {
    self.plus::<SpaceAddr>(RegionAddr::SPACE_OFFSET_BYTES)
  }
  pub fn from_space(addr: SpaceAddr) -> RegionAddr {
    addr.sub::<RegionAddr>(RegionAddr::SPACE_OFFSET_BYTES)
  }
  pub fn contains_space(&self, addr: SpaceAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_freeBlock(&self, addr: FreeBlockAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_block(&self, addr: BlockAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cells(&self, addr: CellsAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_freeCell(&self, addr: FreeCellAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell(&self, addr: CellAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell_size(&self, addr: Cell_sizeAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_object(&self, addr: ObjectAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_0(&self, addr: Ptr_0Addr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_1(&self, addr: Ptr_1Addr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_2(&self, addr: Ptr_2Addr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_3(&self, addr: Ptr_3Addr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_payload(&self, addr: PayloadAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_remainder(&self, addr: RemainderAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_limit(&self, addr: LimitAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_line(&self, addr: LineAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_lms(&self, addr: LmsAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_lineMark(&self, addr: LineMarkAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_refs(&self, addr: RefsAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_refBits(&self, addr: RefBitsAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_mks(&self, addr: MksAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_markBits(&self, addr: MarkBitsAddr, end: RegionAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_space(self) -> SpaceAddr {
    SpaceAddr::from_usize(self.as_usize())
  }
  pub fn get_first_freeBlock(self) -> FreeBlockAddr {
    FreeBlockAddr::from_usize(self.as_usize())
  }
  pub fn get_first_block(self) -> BlockAddr {
    BlockAddr::from_usize(self.as_usize())
  }
  pub fn get_first_line(self) -> LineAddr {
    LineAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cells(self) -> CellsAddr {
    CellsAddr::from_usize(self.as_usize())
  }
  pub fn get_first_freeCell(self) -> FreeCellAddr {
    FreeCellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell(self) -> CellAddr {
    CellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell_size(self) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(self.as_usize())
  }
  pub fn get_first_byte(self) -> ByteAddr {
    ByteAddr::from_usize(self.as_usize())
  }
  pub fn init_canonical_sequence(&self, a1: usize, a2: usize, a3: usize, a4: usize) -> (
    SpaceAddr,
    LmsAddr,
    RefsAddr,
    MksAddr,
    RegionAddrEnd,
  ) {
    let a0: usize = 0;
    let ret =
      (
        self.plus::<SpaceAddr>(a0),
        self.plus::<SpaceAddr>(a0).plus::<LmsAddr>(a1),
        self.plus::<SpaceAddr>(a0).plus::<LmsAddr>(a1).plus::<RefsAddr>(a2),
        self.plus::<SpaceAddr>(a0).plus::<LmsAddr>(a1).plus::<RefsAddr>(a2).plus::<MksAddr>(a3),
        self
          .plus::<SpaceAddr>(a0)
          .plus::<LmsAddr>(a1)
          .plus::<RefsAddr>(a2)
          .plus::<MksAddr>(a3)
          .plus::<RegionAddrEnd>(a4),
      );
    return ret;
  }
  pub fn size_of(&self, end: RegionAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REGION;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Registers {
  pub fn memset_regs_until_regsEnd(val: u8, base: RegsAddr, mx: RegsEndAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn init_regsEnd_after_regs(p1: RegsAddr, bytes: usize) -> RegsEndAddr {
    p1.plus(bytes)
  }
  pub fn regs_is_validly_before_regsEnd(p1: RegsAddr, p2: RegsEndAddr) -> bool {
    p1.lte(p2)
  }
}

pub const REGISTERS_LOG_BYTES_ALIGN: usize = 0;

pub const REGISTERS_BYTES_ALIGN: usize = 1 << REGISTERS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegistersAddr(usize);

deriveAddr! { RegistersAddr, REGISTERS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegistersAddrEnd(usize);

deriveAddr! { RegistersAddrEnd, 1 }

pub const __FLP_IDX_REGISTERS: u8 = 27;

impl RegistersAddr {
  pub const REGS_OFFSET_BYTES: usize = 0;
  pub fn regs(&self) -> RegsAddr {
    self.plus::<RegsAddr>(RegistersAddr::REGS_OFFSET_BYTES)
  }
  pub fn from_regs(addr: RegsAddr) -> RegistersAddr {
    addr.sub::<RegistersAddr>(RegistersAddr::REGS_OFFSET_BYTES)
  }
  pub fn contains_regs(&self, addr: RegsAddr, end: RegistersAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_regsEnd(&self, addr: RegsEndAddr, end: RegistersAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_regs(self) -> RegsAddr {
    RegsAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn init_canonical_sequence(&self, a1: usize, a2: usize) -> (
    RegsAddr,
    RegsEndAddr,
    RegistersAddrEnd,
  ) {
    let a0: usize = 0;
    let ret =
      (
        self.plus::<RegsAddr>(a0),
        self.plus::<RegsAddr>(a0).plus::<RegsEndAddr>(a1),
        self.plus::<RegsAddr>(a0).plus::<RegsEndAddr>(a1).plus::<RegistersAddrEnd>(a2),
      );
    return ret;
  }
  pub fn size_of(&self, end: RegistersAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REGISTERS;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl Regs { }

pub const REGS_LOG_BYTES_ALIGN: usize = 0;

pub const REGS_BYTES_ALIGN: usize = 1 << REGS_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegsAddr(usize);

deriveAddr! { RegsAddr, REGS_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegsAddrEnd(usize);

deriveAddr! { RegsAddrEnd, 1 }

pub const __FLP_IDX_REGS: u8 = 28;

impl RegsAddr {
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn next_Object(ptr: ObjectAddrAddr) -> ObjectAddrAddr {
    ptr.plus(BYTES_IN_WORD)
  }
  pub fn get_first_objectAddrAddr(&self) -> ObjectAddrAddr {
    ObjectAddrAddr(self.as_usize())
  }
  pub fn shadow_alloc_from_registers(&self, bytes: usize) { }
  pub fn size_of(&self, end: RegsAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REGS;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_REGISTERS, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl RegsAddrEnd {
  pub fn to_RegsEndAddr(&self) -> RegsEndAddr {
    RegsEndAddr::from_usize(self.as_usize())
  }
  pub fn from_RegsEndAddr(ptr: RegsEndAddr) -> RegsAddrEnd {
    RegsAddrEnd::from_usize(ptr.as_usize())
  }
}

impl RegsEnd { }

pub const REGSEND_LOG_BYTES_ALIGN: usize = 0;

pub const REGSEND_BYTES_ALIGN: usize = 1 << REGSEND_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegsEndAddr(usize);

deriveAddr! { RegsEndAddr, REGSEND_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RegsEndAddrEnd(usize);

deriveAddr! { RegsEndAddrEnd, 1 }

pub const BYTES_IN_REGSEND: usize = 0;

pub const __FLP_IDX_REGSEND: u8 = 29;

impl RegsEndAddr {
  pub fn to_RegsAddrEnd(self) -> RegsAddrEnd {
    RegsAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_RegsAddrEnd(ptr: RegsAddrEnd) -> RegsEndAddr {
    RegsEndAddr::from_usize(ptr.as_usize())
  }
  pub fn size_of(&self, end: RegsEndAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REGSEND;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_REGISTERS, __FLP_IDX_UNMAPPED ];
  pub fn store_RegsEnd(&self, val: RegsEnd) {
    self.store::<RegsEnd>(val);
  }
  pub fn load_RegsEnd(&self) -> RegsEnd {
    self.load::<RegsEnd>()
  }
}

impl Remainder { }

pub const REMAINDER_LOG_BYTES_ALIGN: usize = 0;

pub const REMAINDER_BYTES_ALIGN: usize = 1 << REMAINDER_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RemainderAddr(usize);

deriveAddr! { RemainderAddr, REMAINDER_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct RemainderAddrEnd(usize);

deriveAddr! { RemainderAddrEnd, 1 }

pub const __FLP_IDX_REMAINDER: u8 = 15;

impl RemainderAddr {
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn to_CellsAddrEnd(self) -> CellsAddrEnd {
    CellsAddrEnd::from_usize(self.as_usize())
  }
  pub fn from_CellsAddrEnd(ptr: CellsAddrEnd) -> RemainderAddr {
    RemainderAddr::from_usize(ptr.as_usize())
  }
  pub fn shadow_alloc_from_block(&self, bytes: usize) { }
  pub fn size_of(&self, end: RemainderAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_REMAINDER;
  pub const __FLP_PARENTS: &'static [u8] = &[
    __FLP_IDX_BLOCK,
    __FLP_IDX_SPACE,
    __FLP_IDX_REGION,
    __FLP_IDX_UNMAPPED,
  ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl RemainderAddrEnd {
  pub fn to_LimitAddr(&self) -> LimitAddr {
    LimitAddr::from_usize(self.as_usize())
  }
  pub fn from_LimitAddr(ptr: LimitAddr) -> RemainderAddrEnd {
    RemainderAddrEnd::from_usize(ptr.as_usize())
  }
}

impl Space { }

pub const SPACE_LOG_BYTES_ALIGN: usize = 19;

pub const SPACE_BYTES_ALIGN: usize = 1 << SPACE_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct SpaceAddr(usize);

deriveAddr! { SpaceAddr, SPACE_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct SpaceAddrEnd(usize);

deriveAddr! { SpaceAddrEnd, 1 }

pub const __FLP_IDX_SPACE: u8 = 2;

impl SpaceAddr {
  pub fn contains_freeBlock(&self, addr: FreeBlockAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_block(&self, addr: BlockAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cells(&self, addr: CellsAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_freeCell(&self, addr: FreeCellAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell(&self, addr: CellAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_cell_size(&self, addr: Cell_sizeAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_object(&self, addr: ObjectAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_0(&self, addr: Ptr_0Addr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_1(&self, addr: Ptr_1Addr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_2(&self, addr: Ptr_2Addr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_ptr_3(&self, addr: Ptr_3Addr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_payload(&self, addr: PayloadAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_remainder(&self, addr: RemainderAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_limit(&self, addr: LimitAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_line(&self, addr: LineAddr, end: SpaceAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_freeBlock(self) -> FreeBlockAddr {
    FreeBlockAddr::from_usize(self.as_usize())
  }
  pub fn get_first_block(self) -> BlockAddr {
    BlockAddr::from_usize(self.as_usize())
  }
  pub fn get_first_line(self) -> LineAddr {
    LineAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cells(self) -> CellsAddr {
    CellsAddr::from_usize(self.as_usize())
  }
  pub fn get_first_freeCell(self) -> FreeCellAddr {
    FreeCellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell(self) -> CellAddr {
    CellAddr::from_usize(self.as_usize())
  }
  pub fn get_first_cell_size(self) -> Cell_sizeAddr {
    Cell_sizeAddr::from_usize(self.as_usize())
  }
  pub fn get_first_byte(self) -> ByteAddr {
    ByteAddr::from_usize(self.as_usize())
  }
  pub fn shadow_alloc_from_region(&self, bytes: usize) { }
  pub fn size_of(&self, end: SpaceAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_SPACE;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_REGION, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl SpaceAddrEnd {
  pub fn to_LmsAddr(&self) -> LmsAddr {
    LmsAddr::from_usize(self.as_usize())
  }
  pub fn from_LmsAddr(ptr: LmsAddr) -> SpaceAddrEnd {
    SpaceAddrEnd::from_usize(ptr.as_usize())
  }
}

impl Stack { }

pub const STACK_LOG_BYTES_ALIGN: usize = 0;

pub const STACK_BYTES_ALIGN: usize = 1 << STACK_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct StackAddr(usize);

deriveAddr! { StackAddr, STACK_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct StackAddrEnd(usize);

deriveAddr! { StackAddrEnd, 1 }

pub const __FLP_IDX_STACK: u8 = 25;

impl StackAddr {
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn next_Object(ptr: ObjectAddrAddr) -> ObjectAddrAddr {
    ptr.plus(BYTES_IN_WORD)
  }
  pub fn get_first_objectAddrAddr(&self) -> ObjectAddrAddr {
    ObjectAddrAddr(self.as_usize())
  }
  pub fn shadow_alloc_from_stk(&self, bytes: usize) { }
  pub fn size_of(&self, end: StackAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_STACK;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_STK, __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

impl StackAddrEnd {
  pub fn to_LowWaterAddr(&self) -> LowWaterAddr {
    LowWaterAddr::from_usize(self.as_usize())
  }
  pub fn from_LowWaterAddr(ptr: LowWaterAddr) -> StackAddrEnd {
    StackAddrEnd::from_usize(ptr.as_usize())
  }
}

impl Stk {
  pub fn memset_stack_until_lowWater(val: u8, base: StackAddr, mx: LowWaterAddr) {
    debug_assert!(mx.greater(base));
    base.memset(val, mx.diff(base))
  }
  pub fn init_lowWater_after_stack(p1: StackAddr, bytes: usize) -> LowWaterAddr {
    p1.plus(bytes)
  }
  pub fn stack_is_validly_before_lowWater(p1: StackAddr, p2: LowWaterAddr) -> bool {
    p1.lte(p2)
  }
}

pub const STK_LOG_BYTES_ALIGN: usize = 0;

pub const STK_BYTES_ALIGN: usize = 1 << STK_LOG_BYTES_ALIGN;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct StkAddr(usize);

deriveAddr! { StkAddr, STK_BYTES_ALIGN }

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct StkAddrEnd(usize);

deriveAddr! { StkAddrEnd, 1 }

pub const __FLP_IDX_STK: u8 = 24;

impl StkAddr {
  pub const STACK_OFFSET_BYTES: usize = 0;
  pub fn stack(&self) -> StackAddr {
    self.plus::<StackAddr>(StkAddr::STACK_OFFSET_BYTES)
  }
  pub fn from_stack(addr: StackAddr) -> StkAddr {
    addr.sub::<StkAddr>(StkAddr::STACK_OFFSET_BYTES)
  }
  pub fn contains_stack(&self, addr: StackAddr, end: StkAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn contains_lowWater(&self, addr: LowWaterAddr, end: StkAddrEnd) -> bool {
    addr.as_usize() >= self.as_usize() && addr.as_usize() < end.as_usize()
  }
  pub fn get_first_stack(self) -> StackAddr {
    StackAddr::from_usize(self.as_usize())
  }
  pub fn get_first_word(self) -> WordAddr {
    WordAddr::from_usize(self.as_usize())
  }
  pub fn init_canonical_sequence(&self, a1: usize, a2: usize) -> (
    StackAddr,
    LowWaterAddr,
    StkAddrEnd,
  ) {
    let a0: usize = 0;
    let ret =
      (
        self.plus::<StackAddr>(a0),
        self.plus::<StackAddr>(a0).plus::<LowWaterAddr>(a1),
        self.plus::<StackAddr>(a0).plus::<LowWaterAddr>(a1).plus::<StkAddrEnd>(a2),
      );
    return ret;
  }
  pub fn size_of(&self, end: StkAddrEnd) -> usize {
    debug_assert!(end.as_usize() >= self.as_usize());
    end.diff(*self)
  }
  pub const __FLP_IDX: u8 = __FLP_IDX_STK;
  pub const __FLP_PARENTS: &'static [u8] = &[ __FLP_IDX_UNMAPPED ];
  pub fn shadow_alloc_from(&self, bytes: usize, vec_IDX: Vec<u8>) { }
}

pub struct Word2MarkBits {
  pub fromStart: WordAddr,
  pub toStart: MarkBitsAddr,
}

impl Word2MarkBits {
  #[inline(always)]
  pub fn word_from_idx(base: WordAddr, idx: usize) -> WordAddr {
    let result = WordAddr::from_usize(base.as_usize() + (idx << 3));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: WordAddr, toStart: MarkBitsAddr) -> Word2MarkBits {
    Word2MarkBits { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: WordAddr, value: MarkBits) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: WordAddr) -> MarkBitsAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: WordAddr) -> MarkBits {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: WordAddr, elem: WordAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 3;
    result
  }
  #[inline(always)]
  pub fn map_set(fromStart: WordAddr, toStart: MarkBitsAddr, fromAddr: WordAddr, value: MarkBits) {
    let idxV = Word2MarkBits::idx(fromStart, fromAddr);
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: MarkBitsAddr, idxV: usize, value: MarkBits) {
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: MarkBitsAddr, idxV: usize) -> MarkBits {
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(
    fromStart: WordAddr,
    toStart: MarkBitsAddr,
    fromAddr: WordAddr,
  ) -> MarkBitsAddr {
    let idxV = Word2MarkBits::idx(fromStart, fromAddr);
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: WordAddr, toStart: MarkBitsAddr, fromAddr: WordAddr) -> MarkBits {
    let idxV = Word2MarkBits::idx(fromStart, fromAddr);
    toStart.offset::<MarkBits, MarkBitsAddr>(idxV).load()
  }
}

pub struct Word2RefBits {
  pub fromStart: WordAddr,
  pub toStart: RefBitsAddr,
}

impl Word2RefBits {
  #[inline(always)]
  pub fn word_from_idx(base: WordAddr, idx: usize) -> WordAddr {
    let result = WordAddr::from_usize(base.as_usize() + (idx << 3));
    result
  }
  #[inline(always)]
  pub fn new(fromStart: WordAddr, toStart: RefBitsAddr) -> Word2RefBits {
    Word2RefBits { fromStart, toStart }
  }
  #[inline(always)]
  pub fn set(&self, fromAddr: WordAddr, value: RefBits) {
    Self::map_set(self.fromStart, self.toStart, fromAddr, value)
  }
  #[inline(always)]
  pub fn getAddr(&self, fromAddr: WordAddr) -> RefBitsAddr {
    Self::map_getAddr(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn get(&self, fromAddr: WordAddr) -> RefBits {
    Self::map_get(self.fromStart, self.toStart, fromAddr)
  }
  #[inline(always)]
  pub fn idx(base: WordAddr, elem: WordAddr) -> usize {
    debug_assert!(elem >= base);
    let result = (elem.as_usize() - base.as_usize()) >> 3;
    result
  }
  #[inline(always)]
  pub fn map_set(fromStart: WordAddr, toStart: RefBitsAddr, fromAddr: WordAddr, value: RefBits) {
    let idxV = Word2RefBits::idx(fromStart, fromAddr);
    toStart.offset::<RefBits, RefBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_set_idx(toStart: RefBitsAddr, idxV: usize, value: RefBits) {
    toStart.offset::<RefBits, RefBitsAddr>(idxV).store(value)
  }
  #[inline(always)]
  pub fn map_get_idx(toStart: RefBitsAddr, idxV: usize) -> RefBits {
    toStart.offset::<RefBits, RefBitsAddr>(idxV).load()
  }
  #[inline(always)]
  pub fn map_getAddr(fromStart: WordAddr, toStart: RefBitsAddr, fromAddr: WordAddr) -> RefBitsAddr {
    let idxV = Word2RefBits::idx(fromStart, fromAddr);
    toStart.offset::<RefBits, RefBitsAddr>(idxV)
  }
  #[inline(always)]
  pub fn map_get(fromStart: WordAddr, toStart: RefBitsAddr, fromAddr: WordAddr) -> RefBits {
    let idxV = Word2RefBits::idx(fromStart, fromAddr);
    toStart.offset::<RefBits, RefBitsAddr>(idxV).load()
  }
}

pub const __FLP_IDX_UNMAPPED: u8 = 0;

pub const __FLP_TYPES: &'static [&'static str] = &[
  "UNMAPPED",
  "REGION",
  "SPACE",
  "FREEBLOCK",
  "BLOCK",
  "CELLS",
  "FREECELL",
  "CELL",
  "CELL_SIZE",
  "OBJECT",
  "PTR_0",
  "PTR_1",
  "PTR_2",
  "PTR_3",
  "PAYLOAD",
  "REMAINDER",
  "LIMIT",
  "LINE",
  "LMS",
  "LINEMARK",
  "REFS",
  "REFBITS",
  "MKS",
  "MARKBITS",
  "STK",
  "STACK",
  "LOWWATER",
  "REGISTERS",
  "REGS",
  "REGSEND",
  "APPOBJECT",
  "LLNODE",
  "QNODE",
  "DEQUEUE",
  "GARBAGE",
];