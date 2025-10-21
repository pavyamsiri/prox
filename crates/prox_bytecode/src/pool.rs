use crate::function::Closure;
use prox_interner::{ConstantIndex, ConstantPool, Interner, Symbol};
use std::hash::RandomState;

pub struct ConstantInterner {
    strings: Interner,
    numbers: ConstantPool<f64>,
    closures: ConstantPool<Closure>,
}

impl ConstantInterner {
    #[must_use]
    pub fn with_interner(interner: Interner) -> Self {
        Self {
            strings: interner,
            numbers: ConstantPool::with_hasher(RandomState::new()),
            closures: ConstantPool::with_hasher(RandomState::new()),
        }
    }

    #[must_use]
    pub fn resolve_string(&self, symbol: Symbol) -> Option<&str> {
        self.strings.resolve(symbol)
    }

    #[must_use]
    pub fn resolve_number(&self, index: ConstantIndex<f64>) -> Option<f64> {
        self.numbers.resolve(index).copied()
    }

    #[must_use]
    pub fn resolve_closure(&self, index: ConstantIndex<Closure>) -> Option<&Closure> {
        self.closures.resolve(index)
    }
}

impl ConstantInterner {
    #[must_use]
    pub fn intern(&mut self, text: &str) -> Symbol {
        self.strings.intern(text)
    }

    #[must_use]
    pub fn add_number(&mut self, number: f64) -> ConstantIndex<f64> {
        self.numbers.intern(number)
    }

    #[must_use]
    pub fn add_closure(&mut self, function: Closure) -> ConstantIndex<Closure> {
        self.closures.intern(function)
    }
}
