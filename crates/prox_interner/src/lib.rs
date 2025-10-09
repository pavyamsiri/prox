use core::fmt;
use core::hash;
use core::ops;
use std::collections::HashMap;
use std::hash::RandomState;

/// A symbol representing an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Symbol {
    /// Return the raw symbol.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

impl From<u32> for Symbol {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

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

    /// Return the end index.
    #[must_use]
    const fn end(self) -> u32 {
        self.start + self.length
    }
}

type HashedString = u64;

#[derive(Debug, Clone)]
pub struct Interner<S = RandomState> {
    /// Map from string (after hashing) to a light weight symbol.
    string_to_symbol: HashMap<HashedString, Symbol>,
    /// Map from symbol (as index) to it span over the string buffer.
    symbol_to_span: Vec<Span>,
    /// Buffer storing all interned strings in sequence.
    data: String,
    /// The hasher used to convert strings to a hash to save on memory.
    hasher_builder: S,
}

impl<S> Interner<S> {
    /// Create a new interner.
    pub fn with_hasher(hasher: S) -> Self {
        Self {
            string_to_symbol: HashMap::new(),
            symbol_to_span: Vec::new(),
            data: String::new(),
            hasher_builder: hasher,
        }
    }

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
        self.hasher_builder.hash_one(text.as_bytes())
    }
}

impl fmt::Display for Interner {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = " ".repeat(4);
        writeln!(f, "Interner {{")?;
        writeln!(f, "{indent}symbol_to_span: [")?;
        for (sym, span) in self.symbol_to_span.iter().enumerate() {
            let lexeme = self
                .resolve(Symbol(sym.try_into().map_err(|_err| fmt::Error)?))
                .ok_or(fmt::Error)?;
            writeln!(
                f,
                "{indent}{indent}{sym} -> {}..{} -> {lexeme},",
                span.start,
                span.end()
            )?;
        }
        writeln!(f, "{indent}],")?;
        writeln!(f, "{indent}data = {:?},", self.data)?;
        write!(f, "{indent}       ^")?;
        for index in 0..self.data.chars().count() {
            write!(f, "{}", index % 10)?;
        }
        writeln!(f, "^")?;

        writeln!(f, "}}")?;

        Ok(())
    }
}
