#![allow(dead_code)]

use std::{
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

pub struct ArrayVec<T, const N: usize> {
    arr: [MaybeUninit<T>; N],
    len: usize,
}

impl<T: Clone, const N: usize> Clone for ArrayVec<T, N> {
    fn clone(&self) -> Self {
        let mut new: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for (i, val) in self[0..self.len].iter().cloned().enumerate() {
            new[i] = MaybeUninit::new(val);
        }
        ArrayVec {
            arr: new,
            len: self.len,
        }
    }
}

impl<T: Copy, const N: usize> Copy for ArrayVec<T, N> {}

impl<T, const N: usize> AsRef<[T]> for ArrayVec<T, N> {
    fn as_ref(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.arr.as_ptr().cast(), self.len) }
    }
}

impl<T, const N: usize> AsMut<[T]> for ArrayVec<T, N> {
    fn as_mut(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.arr.as_mut_ptr().cast(), self.len) }
    }
}

impl<T, const N: usize> Deref for ArrayVec<T, N> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T, const N: usize> DerefMut for ArrayVec<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T, const N: usize> Default for ArrayVec<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> ArrayVec<T, N> {
    pub fn new() -> Self {
        ArrayVec {
            arr: unsafe { MaybeUninit::uninit().assume_init() },
            len: 0,
        }
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn push(&mut self, new: T) {
        if self.len == N {
            panic!("tried to push to full arrayvec");
        } else {
            self.arr[self.len] = MaybeUninit::new(new);
            self.len += 1;
        }
    }
}
