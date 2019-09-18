#![allow(non_snake_case)]
#![allow(dead_code)]
use std::cmp;
use std::fmt;
use std::mem::size_of as size_of;

use super::*;

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct VoidAddr(usize);

deriveAddrReqs!(VoidAddr);
impl Address for VoidAddr {

    fn from_usize(val : usize) -> VoidAddr {
        VoidAddr(val)
    }

    fn as_usize(&self) -> usize {
        self.0
    }

    fn verify(self) -> bool {
        // No verification of void addresses
        true
    }

    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a VoidAddr!")
    }
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a VoidAddr!")
    }

}

impl VoidAddr {

    pub fn from_addr<A: Address>(ptr : A) -> Self {
        VoidAddr(ptr.as_usize())
    }

}

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct AddrAddr(usize);
deriveAddr!(AddrAddr, 1 << size_of::<usize>());

#[inline(always)]
pub fn test_nth_bit(value: u8, index: usize) -> bool {
    value & (1 << index) != 0
}

#[inline(always)]
pub fn lower_bits(value: u8, len: usize) -> u8 {
    value & ((1 << len) - 1)
}

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Word(usize);
deriveAddrReqs!(Word);

fn trailing_zeros(x : usize) -> usize {
    if x % 2 != 0 { 0 } else { trailing_zeros(x / 2) }
}

pub const BYTES_IN_WORD : usize = size_of::<usize>();

#[cfg(target_pointer_width = "32")]
pub const LOG_BYTES_IN_WORD : usize = 2;

#[cfg(target_pointer_width = "64")]
pub const LOG_BYTES_IN_WORD : usize = 3;

#[cfg(all(not(target_pointer_width = "64"), not(target_pointer_width = "32")))]
pub const LOG_BYTES_IN_WORD : usize =
    panic!("Unsupported word size.");

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct WordAddr(usize);
impl Address for Word {

    fn from_usize(val : usize) -> Word {
        Word(val)
    }

    fn as_usize(&self) -> usize {
        self.0
    }

    fn verify(self) -> bool {
        // No verification of words
        true
    }

    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a Word!")
    }
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a Word!")
    }
    fn memset(&self, _char: u8, _length: usize) {
        panic!("Can't memset() a Word!")
    }
}

deriveAddrReqs!(WordAddr);
impl Address for WordAddr {

    fn from_usize(val : usize) -> WordAddr {
        WordAddr(val)
    }

    fn as_usize(&self) -> usize {
        self.0
    }

    fn verify(self) -> bool {
        // No verification of word addresses
        true
    }

    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a WordAddr!")
    }
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a WordAddr!")
    }
    fn memset(&self, _char: u8, _length: usize) {
        panic!("Can't memset() a WordAddr!")
    }
}

impl WordAddr {
    // TODO: "size" types for e.g. sizes of objects:
    #[inline(always)] fn load_raw_usize(&self) -> usize { unsafe {
        *(self.as_usize() as *mut usize)
    }}
    #[inline(always)] fn store_raw_usize(&self, value: usize) { unsafe {
        *(self.as_usize() as *mut usize) = value;
    }}
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Byte(u8);

#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct ByteAddr(usize);

deriveAddrReqs!(ByteAddr);
impl Address for ByteAddr {

    fn from_usize(val : usize) -> ByteAddr {
        ByteAddr(val)
    }

    fn as_usize(&self) -> usize {
        self.0
    }

    fn verify(self) -> bool {
        // No verification of word addresses
        true
    }

    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a ByteAddr!")
    }
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a ByteAddr!")
    }
    fn memset(&self, _char: u8, _length: usize) {
        panic!("Can't memset() a ByteAddr!")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate libc;

    #[test]
    pub fn test_u8_bits() {
        let value : u8 = 0b000011_11;

        assert_eq!(test_nth_bit(value, 6), true);

        assert_eq!(lower_bits(value, 6), 0b00_0011);
    }
}

