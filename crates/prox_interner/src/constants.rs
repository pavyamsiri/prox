use core::convert;
use core::fmt;
use core::hash;
use core::marker;
use std::collections::HashMap;
use std::hash::RandomState;

/// A symbol representing an interned string.
#[derive(PartialEq, Eq, Hash)]
pub struct ConstantIndex<T> {
    index: u32,
    _marker: marker::PhantomData<T>,
}

impl<T> fmt::Debug for ConstantIndex<T> {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ConstantIndex")
            .field("index", &self.index)
            .finish_non_exhaustive()
    }
}

impl<T> Clone for ConstantIndex<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for ConstantIndex<T> {}

impl<T> ConstantIndex<T> {
    /// Return the index as u32.
    #[must_use]
    pub const fn to_u32(self) -> u32 {
        self.index
    }

    /// Return the index as usize.
    #[must_use]
    const fn to_usize(self) -> usize {
        self.index as usize
    }
}

impl<T> From<u32> for ConstantIndex<T> {
    fn from(value: u32) -> Self {
        Self {
            index: value,
            _marker: marker::PhantomData,
        }
    }
}

/// Trait for generating hash keys from values.
pub trait ToHashKey {
    /// Hash itself into its key.
    fn to_hash_key(&self, hasher_builder: &impl hash::BuildHasher) -> u64;
}

/// Specialized implementation for f64 with () hasher.
impl ToHashKey for f64 {
    fn to_hash_key(&self, _hasher_builder: &impl hash::BuildHasher) -> u64 {
        self.to_bits()
    }
}

#[derive(Debug, Clone)]
pub struct ConstantPool<T, S = RandomState> {
    /// Map from constant (after hashing) to a light weight symbol.
    constant_to_symbol: HashMap<u64, ConstantIndex<T>>,
    /// Buffer storing all constants in sequence.
    data: Vec<T>,
    /// The hasher used to convert constants to a hash to save on memory.
    hasher_builder: S,
}

impl<T, S> ConstantPool<T, S> {
    /// Create a new interner.
    pub fn with_hasher(hasher: S) -> Self {
        Self {
            constant_to_symbol: HashMap::new(),
            data: Vec::new(),
            hasher_builder: hasher,
        }
    }

    /// Resolve an constant index into a value.
    /// Returns `None` if the given index is invalid.
    pub fn resolve(&self, index: ConstantIndex<T>) -> Option<&T> {
        self.data.get(index.to_usize())
    }
}

impl<T, S> ConstantPool<T, S>
where
    T: ToHashKey,
    S: hash::BuildHasher,
{
    /// Return an index representing the interned constant.
    ///
    /// # Panics
    /// This function will panic if the pool has reached capacity.
    pub fn intern(&mut self, value: T) -> ConstantIndex<T> {
        let key = value.to_hash_key(&self.hasher_builder);
        // Return interned constant.
        if let Some(index) = self.constant_to_symbol.get(&key) {
            *index
        }
        // Intern constant.
        else {
            let index = ConstantIndex::<T>::from(
                convert::TryInto::<u32>::try_into(self.data.len())
                    .expect("constant pool has reached capacity."),
            );
            self.data.push(value);
            self.constant_to_symbol.insert(key, index);

            index
        }
    }
}

#[macro_export]
macro_rules! impl_to_hash_key {
    ($name:ty) => {
        impl $crate::ToHashKey for $name {
            fn to_hash_key(&self, hasher_builder: &impl core::hash::BuildHasher) -> u64 {
                use core::hash::Hash as _;
                use core::hash::Hasher as _;
                let mut hasher = hasher_builder.build_hasher();
                self.hash(&mut hasher);
                hasher.finish()
            }
        }
    };
}
