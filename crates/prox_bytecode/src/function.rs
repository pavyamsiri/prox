use crate::chunk::ChunkId;
use prox_interner::{Symbol, impl_to_hash_key};

#[derive(Debug, Hash)]
pub struct Function {
    /// The name of the function.
    pub name: Symbol,
    /// The chunk ID of the function body.
    pub chunk: ChunkId,
    /// The arity of the function.
    pub arity: u8,
}

impl_to_hash_key!(Function);
