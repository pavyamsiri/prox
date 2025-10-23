use prox_interner::{Symbol, impl_to_hash_key};

#[derive(Debug, Hash)]
pub struct Class {
    /// The name of the class.
    pub name: Symbol,
}

impl_to_hash_key!(Class);
