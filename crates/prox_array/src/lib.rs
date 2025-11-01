//! A run length encoded dynamic array.

use core::default;
use core::iter;

/// A data entry.
#[derive(Debug)]
struct Entry<T> {
    /// The actual value.
    data: T,
    /// How many times this value is repeated.
    length: usize,
}

/// A run length encoded dynamic array.
#[derive(Debug)]
pub struct RunArray<T> {
    /// The underlying data.
    data: Vec<Entry<T>>,
}

impl<T> RunArray<T>
where
    T: PartialEq,
{
    /// Push a value.
    pub fn push(&mut self, value: T) {
        match self.data.last_mut() {
            Some(last) if last.data == value => {
                last.length += 1;
            }
            _ => self.data.push(Entry {
                data: value,
                length: 1,
            }),
        }
    }

    /// Push multiple of the same value.
    pub fn push_multiple(&mut self, value: T, count: usize) {
        match self.data.last_mut() {
            Some(last) if last.data == value => {
                last.length += count;
            }
            _ => self.data.push(Entry {
                data: value,
                length: count,
            }),
        }
    }
}

impl<T> default::Default for RunArray<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T> RunArray<T> {
    /// Create a new run length encoded array.
    #[must_use]
    pub const fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Get a reference to the item at the given index.
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&T> {
        let mut remaining = index;
        for entry in self.data.iter() {
            if remaining < entry.length {
                return Some(&entry.data);
            }
            remaining -= entry.length;
        }
        None
    }

    /// Return an iterator over the elements of the array.
    #[must_use]
    pub const fn iter(&self) -> RunArrayIterator<'_, T> {
        RunArrayIterator {
            inner: self,
            index: 0,
        }
    }
}

impl<'data, T> iter::IntoIterator for &'data RunArray<T> {
    type Item = &'data T;
    type IntoIter = RunArrayIterator<'data, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct RunArrayIterator<'data, T> {
    /// The run length array.
    inner: &'data RunArray<T>,
    /// The current index.
    index: usize,
}

impl<'data, T> iter::Iterator for RunArrayIterator<'data, T> {
    type Item = &'data T;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.index;
        self.index += 1;
        self.inner.get(index)
    }
}
