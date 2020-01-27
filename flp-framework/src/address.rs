//! # The `flp_framework::address` Module
//!
//! This module contains some of the basic primitive address
//! types supported by Floorplan including void addresses,
//! words, bytes, and so on. The implementations in this file
//! look much like what gets generated for Floorplan-defined
//! types, but are manually coded here to provide stronger
//! control over how they get used.
//!
//! For information on acquiring the Floorplan compiler itself,
//! go see the [GitHub project here][github-project].
//!
//! [github-project]: https://github.com/RedlineResearch/floorplan

#![allow(non_snake_case)]
#![allow(dead_code)]
use std::cmp;
use std::fmt;
use std::mem::size_of as size_of;

use super::*;

/// A `VoidAddr`, much like a `void*` in C, is available for those
/// in need of a quick-and-dirty way of representing the value of a
/// pointer without actually being able to read from or write to that
/// pointer.
#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct VoidAddr(usize);

deriveAddrReqs!(VoidAddr);
impl Address for VoidAddr {

    /// Construct a void address:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let va = VoidAddr::from_usize(0xdeadbeef);
    /// ```
    fn from_usize(val : usize) -> VoidAddr { VoidAddr(val) }

    /// Deconstruct a void address:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let va = VoidAddr::from_usize(0xdeadbeef);
    /// assert!(va.as_usize() == 3735928559);
    /// ```
    fn as_usize(&self) -> usize { self.0 }

    /// Trivially verify that this void address looks valid.
    /// All values are valid:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let va = VoidAddr::from_usize(0xdeadbeef);
    /// assert!(va.verify());
    /// ```
    fn verify(self) -> bool { true }

    /// Void addresses cannot be accessed.
    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a VoidAddr!")
    }
    
    /// Void addresses cannot be accessed.
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a VoidAddr!")
    }
    
    /// Void addresses cannot be accessed.
    #[inline(always)] fn memset(&self, _char: u8, _length: usize) {
        panic!("Can't memset() a VoidAddr!")
    }
}

impl VoidAddr {

    /// A void address can be constructed from any other address type.
    pub fn from_addr<A: Address>(ptr : A) -> Self {
        VoidAddr(ptr.as_usize())
    }

}

/// A generic address that points to a memory location containing an address.
#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct AddrAddr(usize);
deriveAddr!(AddrAddr, 1 << size_of::<usize>());

/// Determine whether or not the nth-lowest bit of a byte is set.
#[inline(always)]
pub fn test_nth_bit(value: u8, index: usize) -> bool {
    value & (1 << index) != 0
}

/// Mask for just the nth-lowest bits of a byte.
#[inline(always)]
pub fn lower_bits(value: u8, len: usize) -> u8 {
    value & ((1 << len) - 1)
}

/// A `Word` is a value representing data, useful for its ability to
/// be addressed and subsequently accessed by a `WordAddr`. Suppose
/// you have some word address in `wa` and you want to ensure that the lowest
/// bit of that word is set:
///
/// ```rust
/// # #![feature(rustc_private)]
/// # use std::alloc::{alloc, Layout};
/// # use flp_framework::*;
/// # let wa_alloc = unsafe { alloc(Layout::new::<usize>()) };
/// # let wa = WordAddr::from_ptr(wa_alloc);
/// # wa.store(WordAddr::from_usize(0b1));
/// let w : Word = wa.load();
/// assert!(w.is_aligned_to(2) == false);
/// ```
#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Word(usize);
deriveAddrReqs!(Word);

/// Computes the number of consecutive lower-order bits that are
/// set to zero in a `usize` value.
fn trailing_zeros(x : usize) -> usize {
    if x % 2 != 0 { 0 } else { trailing_zeros(x / 2) }
}

/// The number of bytes in a word of memory.
pub const BYTES_IN_WORD : usize = size_of::<usize>();

/// Log base 2 of the number of bytes in a word of memory.
#[cfg(target_pointer_width = "32")]
pub const LOG_BYTES_IN_WORD : usize = 2;

/// Log base 2 of the number of bytes in a word of memory.
#[cfg(target_pointer_width = "64")]
pub const LOG_BYTES_IN_WORD : usize = 3;

/// Only 32-bit and 64-bit architectures are currently supported by Floorplan.
#[cfg(all(not(target_pointer_width = "64"), not(target_pointer_width = "32")))]
pub const LOG_BYTES_IN_WORD : usize =
    panic!("Unsupported word size.");

/// A `WordAddr` is a value representing the address of a word of memory.
#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct WordAddr(usize);
impl Address for Word {

    /// Constructor for a word value from a raw `usize`.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let w = Word::from_usize(0xff);
    /// ```
    fn from_usize(val : usize) -> Word { Word(val) }

    /// Deconstructor for a word value into a raw `usize` value.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let w = Word::from_usize(0xff);
    /// assert!(w.as_usize() == 255);
    /// ```
    fn as_usize(&self) -> usize { self.0 }

    /// All possible word values are valid:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let w = Word::from_usize(0xff);
    /// assert!(w.verify());
    /// ```
    fn verify(self) -> bool { true }

    /// A word cannot be accessed.
    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a Word!")
    }
    
    /// A word cannot be accessed.
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a Word!")
    }

    /// Raw word values are not writeable.
    fn memset(&self, _char: u8, _length: usize) {
        panic!("Can't memset() a Word!")
    }
}

deriveAddrReqs!(WordAddr);
impl Address for WordAddr {

    /// Constructor for a word address from a raw `usize`.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let wa = WordAddr::from_usize(0xdeadbeef);
    /// ```
    fn from_usize(val : usize) -> WordAddr { WordAddr(val) }

    /// Deconstructor for a word address into a raw `usize` value.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let wa = WordAddr::from_usize(0xdeadbeef);
    /// assert!(wa.as_usize() == 3735928559);
    /// ```
    fn as_usize(&self) -> usize { self.0 }

    /// All possible word values are valid:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let wa = WordAddr::from_usize(0xdeadbeef);
    /// assert!(wa.verify());
    /// ```
    fn verify(self) -> bool { true }

    /// A generic address to a raw word cannot be accessed.
    #[inline(always)] fn load<T: Copy> (&self) -> T { unsafe {
        *(self.as_usize() as *mut T)
    }}
    
    /// A generic address to a raw word cannot be accessed.
    #[inline(always)] fn store<T> (&self, value: T) { unsafe {
        *(self.as_usize() as *mut T) = value;
    }}
    
}

/// A `Byte` is a value representing data, useful for its ability to
/// be addressed and subsequently accessed by a `ByteAddr`. Suppose
/// you have some byte address in `ba` and you want to ensure that the lowest
/// bit of that word is not set:
///
/// ```rust
/// # #![feature(rustc_private)]
/// # use std::alloc::{alloc, Layout};
/// # use flp_framework::*;
/// # let ba_alloc = unsafe { alloc(Layout::new::<usize>()) };
/// # let ba = ByteAddr::from_ptr(ba_alloc);
/// # ba.store(ByteAddr::from_usize(0b0));
/// let b : Byte = ba.load();
/// assert!(b.is_aligned_to(2) == true);
/// ```
#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct Byte(u8);
deriveAddrReqs!(Byte);

// TODO: both this impl and the one for `Word` need to be
// changed to represent a `Value` or `Prim` trait to show
// that the associated methods represent operations over primitive
// values and not operations over an address type.
impl Address for Byte {

    /// Constructor for a byte value from a raw `usize`.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let w = Byte::from_usize(0xff);
    /// ```
    fn from_usize(val : usize) -> Byte { Byte(val as u8) }

    /// Deconstructor for a byte value into a raw `usize` value.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let w = Byte::from_usize(0xff);
    /// assert!(w.as_usize() == 255);
    /// ```
    fn as_usize(&self) -> usize { self.0 as usize }

    /// All possible byte values are valid:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let w = Byte::from_usize(0xff);
    /// assert!(w.verify());
    /// ```
    fn verify(self) -> bool { true }

    /// A byte cannot be accessed.
    #[inline(always)] fn load<T: Copy> (&self) -> T {
        panic!("Can't load() a Byte!")
    }
    
    /// A byte cannot be accessed.
    #[inline(always)] fn store<T> (&self, _value: T) {
        panic!("Can't store() a Byte!")
    }

    /// Raw byte values are not writeable.
    fn memset(&self, _char: u8, _length: usize) {
        panic!("Can't memset() a Byte!")
    }
}

/// A `ByteAddr` is a value representing the address of zero or more bytes of memory.
#[repr(C)]
#[derive(Copy, Clone, Eq, Hash)]
pub struct ByteAddr(usize);

deriveAddrReqs!(ByteAddr);
impl Address for ByteAddr {

    /// Constructor for a byte address from a raw `usize`.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let ba = ByteAddr::from_usize(0xdeadbeef);
    /// ```
    fn from_usize(val : usize) -> ByteAddr { ByteAddr(val) }

    /// Deconstructor for a byte address into a raw `usize` value.
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let ba = ByteAddr::from_usize(0xdeadbeef);
    /// assert!(ba.as_usize() == 3735928559);
    /// ```
    fn as_usize(&self) -> usize { self.0 }

    /// All byte addresses are valid:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let ba = ByteAddr::from_usize(0xdeadbeef);
    /// assert!(ba.verify());
    /// ```
    fn verify(self) -> bool { true }

    /// A generic address to a raw byte cannot be accessed.
    #[inline(always)] fn load<T: Copy> (&self) -> T { unsafe {
        *(self.as_usize() as *mut T)
    }}
    
    /// A generic address to a raw byte cannot be accessed.
    #[inline(always)] fn store<T> (&self, value: T) { unsafe {
        *(self.as_usize() as *mut T) = value;
    }}
}

#[cfg(test)]
mod tests {
    use super::*;

    pub const VAL : u8 = 0b000011_11;
    #[test] pub fn v0() { assert_eq!(test_nth_bit(VAL, 0), true); }
    #[test] pub fn v1() { assert_eq!(test_nth_bit(VAL, 1), true); }
    #[test] pub fn v2() { assert_eq!(test_nth_bit(VAL, 2), true); }
    #[test] pub fn v3() { assert_eq!(test_nth_bit(VAL, 3), true); }
    #[test] pub fn v4() { assert_eq!(test_nth_bit(VAL, 4), false); }
    #[test] pub fn v5() { assert_eq!(test_nth_bit(VAL, 5), false); }
    #[test] pub fn v6() { assert_eq!(test_nth_bit(VAL, 6), false); }
    #[test] pub fn v7() { assert_eq!(test_nth_bit(VAL, 7), false); }
}

