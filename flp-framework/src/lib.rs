//! # The `flp_framework` Crate
//!
//! This module contains the Rust binding for the Floorplan compiler
//! to generate address types in terms of. These interfaces support
//! basic arithmetic operations, address comparisons, pretty printing,
//! and direct memory access.
//!
//! For information on acquiring the Floorplan compiler itself,
//! go see the [GitHub project here][github-project].
//!
//! [github-project]: https://github.com/RedlineResearch/floorplan

#![allow(dead_code)]
use std::mem;
use std::marker::Sized;

/// The `Address` trait exposes all the basic operations (arithmetic, etc.) that
/// can be performed on an address of a Floorplan type. These are all intrinsically
/// unsafe operations, and intended for use by developers of the Floorplan compiler, or other developers extending the capability of Floorplan. Directly accessing these
/// trait functions from memory management code is ill-advised, and unsupported.
pub trait Address: PartialOrd + Copy + Sized {
    
    /// An address can be constructed from a `usize` value.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn as_usize(&self) -> usize;
    
    /// An address can be decronstructed into a raw `usize` value.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn from_usize(usize) -> Self;
    
    /// Does this address appear valid accoring to the layout / address type?
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    fn verify(self) -> bool;
    
    /// Add some number of `bytes` to this address, producing an address of type `A`.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn plus<A : Address>(&self, bytes: usize) -> A { Address::from_usize(self.as_usize() + bytes) }
    
    /// Subtract some number of `bytes` from this address, producing an address of type `A`.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[allow(dead_code)]
    #[inline(always)] fn sub<A : Address>(&self, bytes: usize) -> A { Address::from_usize(self.as_usize() - bytes) }
    
    /// Offset this `&self` address some number of instances of type `T`, producing an address of
    /// type `A`.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn offset<T, A : Address>(&self, offset: usize) -> A {
        Address::from_usize(self.as_usize() + (mem::size_of::<T>() as usize) * offset)
    }
    
    /// Read a single instance of a value of type `T` from this address.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn load<T: Copy> (&self) -> T { unsafe { *(self.as_usize() as *mut T) } }
    
    /// Store a single instance `value` into the memory at this address.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn store<T> (&self, value: T) { unsafe { *(self.as_usize() as *mut T) = value; } }
   
    /// Is the value of this address equivalent to the (universal) null value?
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn is_zero(&self) -> bool { self.as_usize() == 0 }
    
    /// Align this pointer up (increasing value) to the nearest address with a value
    /// aligned to `align` bytes. For example the following properties hold:
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let a = WordAddr::from_usize(0xff);
    /// assert!(a.align_up::<WordAddr>(2).as_usize() == 0x100);
    /// assert!(a.align_up::<WordAddr>(16).as_usize() == 0x100);
    /// assert!(a.align_up::<WordAddr>(512).as_usize() == 0x200);
    /// let b = WordAddr::from_usize(0x100);
    /// assert!(b.align_up::<WordAddr>(256).as_usize() == 0x100);
    /// ```
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn align_up<A : Address>(&self, align: usize) -> A {
        Address::from_usize((self.as_usize() + align - 1) & !(align - 1))
    }
    
    /// Is the value of this address greater than or equal to the value of the given address?
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn gte<A: Address>(&self, addr: A) -> bool { self.as_usize() >= addr.as_usize() }
    
    /// Is the value of this address greater than the value of the given address?
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn greater<A: Address>(&self, addr: A) -> bool { self.as_usize() > addr.as_usize() }
    
    /// Is the value of this address less than or equal to the value of the given address?
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn lte<A: Address>(&self, addr: A) -> bool { self.as_usize() <= addr.as_usize() }
    
    /// Is the value of this address less than the value of the given address?
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn less<A: Address>(&self, addr: A) -> bool { self.as_usize() < addr.as_usize() }
    
    /// Is the value of this address exactly aligned to the given alignment?
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// assert!(WordAddr::from_usize(0xFF).is_aligned_to(2) == false);
    /// assert!(WordAddr::from_usize(0xF0).is_aligned_to(2) == true);
    /// ```
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn is_aligned_to(&self, align: usize) -> bool { self.as_usize() % align == 0 }
    
    /// Construct an address from an immutable constant Rust pointer type.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn from_ptr<T> (ptr: *const T) -> Self { unsafe {Address::from_usize(mem::transmute(ptr))} }
    
    /// Deconstruct an address into an immutable constant Rust pointer type.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn to_ptr<T> (&self) -> *const T { unsafe {mem::transmute(self.as_usize())} }
    
    /// Construct an address from a **mutable** constant Rust pointer type.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn to_ptr_mut<T> (&self) -> *mut T { unsafe {mem::transmute(self.as_usize())} }
    
    /// Construct the (universal) null address.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn zero() -> Self { Address::from_usize(0) }
    
    /// Compute the number of bytes before this address (exclusive) and after
    /// `another` address (inclusive).
    ///
    /// ```rust
    /// # use flp_framework::*;
    /// let wa1 : WordAddr = WordAddr::from_usize(0xFF);
    /// let wa2 : WordAddr = WordAddr::zero(); // The null address
    /// assert!(wa1.diff(wa2) == 0xFF)
    /// ```
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    #[inline(always)] fn diff<A : Address>(&self, another: A) -> usize {
        debug_assert!(self.as_usize() >= another.as_usize(), "for a.diff(b), a needs to be larger than b");
        self.as_usize() - another.as_usize()
    }

    /// Set the first `length` bytes pointed to by this address to the byte `val`.
    ///
    /// *Accessing this function directly from [memory management code is
    /// unsupported](https://redlineresearch.github.io/floorplan/unsupported-operations)*
    fn memset(&self, val: u8, length: usize) {
        let mut cur : *mut u8 = self.as_usize() as *mut u8;
        for _ in 0..length {
            unsafe {
                *cur = val;
                cur = cur.offset(1);
            }
        }
    }
}

#[allow(unused_macros)]
#[macro_export]
macro_rules! deriveAddrSized {
    ( $addr:ident, $addrSized:ident ) => {

        impl $addrSized {
            /// Construct a sized address from the given unsized address type.
            pub fn from_addr(ptr : $addr, size : usize) -> $addrSized {
                $addrSized {
                    start: ptr,
                    end:   ptr.plus(size)
                }
            }

            /// Deconstruct a sized address into just its unsized type.
            pub fn to_addr(&self) -> $addr { self.start }

            /// Construct a void address pointing to the first byte of memory
            /// past the end of this sized type.
            pub fn end(&self) -> VoidAddr { self.end }
        }

    };
}

/// Derives just the necessary pieces of code to satisfy the Rust type
/// checker when it comes to the default implementation of an instance
/// of a type supporting the `Address` trait. This includes address comparisons
/// and printf formatting support.
#[macro_export]
macro_rules! deriveAddrReqs {
    ( $addr:ident ) => {

        impl Ord for $addr {
            /// Address ordinality delegates to usize ordinality.
            #[inline(always)] fn cmp(&self, other: &$addr) -> cmp::Ordering {
                self.as_usize().cmp(& other.as_usize())
            }
        }

        impl PartialOrd for $addr {
            /// Address ordinality delegates to usize ordinality.
            #[inline(always)] fn partial_cmp(&self, other: &$addr) -> Option<cmp::Ordering> {
                Some(self.as_usize().cmp(& other.as_usize()))
            }
        }

        impl PartialEq for $addr {
            /// Address equality delegates to `usize` equality
            #[inline(always)] fn eq(&self, other: &$addr) -> bool {
                self.as_usize() == other.as_usize()
            }
            /// Address inequality delegates to `usize` equality
            #[inline(always)] fn ne(&self, other: &$addr) -> bool {
                self.as_usize() != other.as_usize()
            }
        }

        impl fmt::UpperHex for $addr {
            /// Format this address as an alphanumeric hex string.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:X}", self.as_usize())
            }
        }

        impl fmt::Display for $addr {
            /// Display this address as a prettified alphanumeric hex string.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "0x{:X}", self.as_usize())
            }
        }

        impl fmt::Debug for $addr {
            /// Display this address as a prettified alphanumeric hex string for debugging.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "0x{:X}", self.as_usize())
            }
        }
        impl fmt::Pointer for $addr {
            /// Display this address as a prettified alphanumeric hex string.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "0x{:X}", self.as_usize())
            }
        }
    };
}

/// Derives just the necessary pieces of code for the `Address` trait
/// so that all functions have a definition. This formulates the most
/// basic implementation of an address type to get functioning code.
#[macro_export]
macro_rules! deriveAddrTrait {
    ( $addr:ident, $align:expr ) => {
        impl Address for $addr {

            /// Construct an address of this type from a raw `usize` value.
            fn from_usize(val : usize) -> $addr { $addr(val) }

            /// Deconstruct this address into a raw `usize` value.
            fn as_usize(&self) -> usize { self.0 }

            /// Default verification of proper address alignment.
            fn verify(self) -> bool { self.0 % ($align) == 0 }

        }

    };
}

/// Derive all the boilerplate traits necessary to make a Floorplan-generated
/// address type support all the necessary basic operations. These operations include:
///
/// - Arithmetic operations
/// - Comparison operations
/// - Alignment verification
/// - Construction and deconstruction from a `usize`
/// - Pretty printing and other formatters
///
/// This macro is the main entrypoint for all Floorplan types, and will
/// suffice in general for any new address types.
#[macro_export]
macro_rules! deriveAddr {
    ( $addr:ident, $align:expr ) => {
        deriveAddrReqs!($addr);
        deriveAddrTrait!($addr, $align);
    };
}

pub mod address;
pub use address::*;

