use crate::function::Function;
use prox_interner::{ConstantIndex, ConstantPool, Interner, Symbol};
use std::hash::RandomState;

pub struct ConstantInterner {
    strings: Interner,
    numbers: ConstantPool<f64>,
    functions: ConstantPool<Function>,
}

impl ConstantInterner {
    #[must_use]
    pub fn with_interner(interner: Interner) -> Self {
        Self {
            strings: interner,
            numbers: ConstantPool::with_hasher(RandomState::new()),
            functions: ConstantPool::with_hasher(RandomState::new()),
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
    pub fn resolve_function(&self, index: ConstantIndex<Function>) -> Option<&Function> {
        self.functions.resolve(index)
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
    pub fn add_function(&mut self, function: Function) -> ConstantIndex<Function> {
        self.functions.intern(function)
    }
}
