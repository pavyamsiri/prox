use crate::{StackSlot, UpvalueIndex, chunk::ChunkId};
use prox_interner::{Symbol, impl_to_hash_key};

#[derive(Debug, Hash)]
pub struct Closure {
    /// The name of the function.
    pub name: Symbol,
    /// The chunk ID of the function body.
    pub chunk: ChunkId,
    /// The arity of the function.
    pub arity: u8,
    /// The upvalues.
    pub upvalues: Vec<Upvalue>,
}

impl_to_hash_key!(Closure);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Upvalue {
    Local(StackSlot),
    Upvalue(UpvalueIndex),
}
