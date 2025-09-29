use core::ops;
use std::collections::HashMap;
use std::hash;

/// A symbol representing an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Symbol(u32);

/// A span over bytes.
#[derive(Debug, Clone, Copy)]
struct Span {
    /// The byte index of the start of the span.
    start: u32,
    /// The number of bytes in the span.
    length: u32,
}

impl Span {
    /// Convert the span into a range.
    #[must_use]
    const fn range(self) -> ops::Range<usize> {
        let start = self.start as usize;
        let length = self.length as usize;
        start..(start + length)
    }
}

type HashedString = u64;

#[derive(Debug, Clone)]
pub struct Interner<S = hash::DefaultHasher> {
    /// Map from string (after hashing) to a light weight symbol.
    string_to_symbol: HashMap<HashedString, Symbol>,
    /// Map from symbol (as index) to it span over the string buffer.
    symbol_to_span: Vec<Span>,
    /// Buffer storing all interned strings in sequence.
    data: String,
    /// The hasher used to convert strings to a hash to save on memory.
    hasher: S,
}

impl<S> Interner<S> {
    /// Resolve an interned symbol into a string.
    /// Returns `None` if the given symbol is invalid.
    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        let span = self.symbol_to_span.get(symbol.0 as usize)?;
        self.data.get(span.range())
    }
}

impl<S: hash::BuildHasher> Interner<S> {
    /// Return a symbol representing the interned string.
    ///
    /// # Panics
    /// This function will panic if the interner has reached capacity.
    pub fn intern(&mut self, text: &str) -> Symbol {
        let key = self.hash_string(text);
        // Return interned string.
        if let Some(symbol) = self.string_to_symbol.get(&key) {
            *symbol
        }
        // Intern string.
        else {
            let start: u32 = self
                .data
                .len()
                .try_into()
                .expect("interner has reached capacity.");
            let length: u32 = text
                .len()
                .try_into()
                .expect("interner has reached capacity.");
            let span = Span { start, length };

            let symbol = Symbol(
                self.symbol_to_span
                    .len()
                    .try_into()
                    .expect("interner has reached capacity."),
            );
            self.symbol_to_span.push(span);
            self.data.push_str(text);
            self.string_to_symbol.insert(key, symbol);

            symbol
        }
    }

    /// Hash a string.
    fn hash_string(&self, text: &str) -> HashedString {
        self.hasher.hash_one(text)
    }
}
