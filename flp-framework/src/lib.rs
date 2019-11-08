#![allow(dead_code)]
use std::mem;
use std::marker::Sized;

pub trait Address: PartialOrd + Copy + Sized { //where Self: Sized {
    #[inline(always)] fn as_usize(&self) -> usize;
    #[inline(always)] fn from_usize(usize) -> Self;
    // Does this address "look" valid, according to the layout
    fn verify(self) -> bool;

    #[inline(always)] fn plus<A : Address>(&self, bytes: usize) -> A { Address::from_usize(self.as_usize() + bytes) }
    #[allow(dead_code)]
    #[inline(always)] fn sub<A : Address>(&self, bytes: usize) -> A { Address::from_usize(self.as_usize() - bytes) }
    #[inline(always)] fn offset<T, A : Address>(&self, offset: usize) -> A { Address::from_usize(self.as_usize() + (mem::size_of::<T>() as usize) * offset) }
    #[inline(always)] fn load<T: Copy> (&self) -> T { unsafe { *(self.as_usize() as *mut T) } }
    #[inline(always)] fn store<T> (&self, value: T) { unsafe { *(self.as_usize() as *mut T) = value; } }
    #[inline(always)] fn is_zero(&self) -> bool { self.as_usize() == 0 }
    #[inline(always)] fn align_up<A : Address>(&self, align: usize) -> A { Address::from_usize((self.as_usize() + align - 1) & !(align - 1)) }
    #[inline(always)] fn gte<A: Address>(&self, addr: A) -> bool { self.as_usize() >= addr.as_usize() }
    #[inline(always)] fn greater<A: Address>(&self, addr: A) -> bool { self.as_usize() > addr.as_usize() }
    #[inline(always)] fn lte<A: Address>(&self, addr: A) -> bool { self.as_usize() <= addr.as_usize() }
    #[inline(always)] fn less<A: Address>(&self, addr: A) -> bool { self.as_usize() < addr.as_usize() }
    #[inline(always)] fn is_aligned_to(&self, align: usize) -> bool { self.as_usize() % align == 0 }
    #[inline(always)] fn from_ptr<T> (ptr: *const T) -> Self { unsafe {Address::from_usize(mem::transmute(ptr))} }
    #[inline(always)] fn to_ptr<T> (&self) -> *const T { unsafe {mem::transmute(self.as_usize())} }
    #[inline(always)] fn to_ptr_mut<T> (&self) -> *mut T { unsafe {mem::transmute(self.as_usize())} }
    #[inline(always)] fn zero() -> Self { Address::from_usize(0) }

    #[inline(always)] fn diff<A : Address>(&self, another: A) -> usize {
        debug_assert!(self.as_usize() >= another.as_usize(), "for a.diff(b), a needs to be larger than b");
        self.as_usize() - another.as_usize()
    }

    fn memset(&self, char: u8, length: usize) {
        let mut cur : *mut u8 = self.as_usize() as *mut u8;
        for _ in 0..length {
            unsafe {
                *cur = char;
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
            pub fn from_addr(ptr : $addr, size : usize) -> $addrSized {
                $addrSized {
                    start: ptr,
                    end:   ptr.plus(size)
                }
            }
            pub fn to_addr(&self) -> $addr {
                self.start
            }
            pub fn end(&self) -> VoidAddr {
                self.end
            }
        }

    };
}

#[macro_export]
macro_rules! deriveAddrReqs {
    ( $addr:ident ) => {

        impl Ord for $addr {
            #[inline(always)] fn cmp(&self, other: &$addr) -> cmp::Ordering {
                self.as_usize().cmp(& other.as_usize())
            }
        }

        impl PartialOrd for $addr {
            #[inline(always)] fn partial_cmp(&self, other: &$addr) -> Option<cmp::Ordering> {
                Some(self.as_usize().cmp(& other.as_usize()))
            }
        }

        impl PartialEq for $addr {
            #[inline(always)] fn eq(&self, other: &$addr) -> bool {
                self.as_usize() == other.as_usize()
            }
            #[inline(always)] fn ne(&self, other: &$addr) -> bool {
                self.as_usize() != other.as_usize()
            }
        }

        impl fmt::UpperHex for $addr {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:X}", self.as_usize())
            }
        }

        impl fmt::Display for $addr {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "0x{:X}", self.as_usize())
            }
        }

        impl fmt::Debug for $addr {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "0x{:X}", self.as_usize())
            }
        }
        impl fmt::Pointer for $addr {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "0x{:X}", self.as_usize())
            }
        }
    };
}

#[macro_export]
macro_rules! deriveAddrTrait {
    ( $addr:ident, $align:expr ) => {
        impl Address for $addr {

            fn from_usize(val : usize) -> $addr {
                $addr(val)
            }

            fn as_usize(&self) -> usize {
                self.0
            }

            fn verify(self) -> bool {
                self.0 % ($align) == 0
            }

        }

    };
}

#[macro_export]
macro_rules! deriveAddr {
    ( $addr:ident, $align:expr ) => {
        deriveAddrReqs!($addr);
        deriveAddrTrait!($addr, $align);
    };
}

pub mod address;
pub use address::*;

