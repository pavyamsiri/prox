use crate::{StackSlot, function::Function, index::InstructionOffset, pool::ConstantInterner};
use core::fmt;
use prox_interner::{ConstantIndex, Symbol};

const PRINT: u8 = 0;
const LOAD_FALSE: u8 = 1;
const LOAD_TRUE: u8 = 2;
const LOAD_NIL: u8 = 3;
const LOAD_NUMBER: u8 = 4;
const LOAD_STRING: u8 = 5;
const LOAD_FUNCTION: u8 = 6;
const MUL: u8 = 7;
const DIV: u8 = 8;
const ADD: u8 = 9;
const SUB: u8 = 10;
const LT: u8 = 11;
const LE: u8 = 12;
const GT: u8 = 13;
const GE: u8 = 14;
const NE: u8 = 15;
const EQ: u8 = 16;
const SET_GLOBAL: u8 = 17;
const POP: u8 = 18;
const GET_LOCAL: u8 = 19;
const SET_LOCAL: u8 = 20;
const GET_GLOBAL: u8 = 21;
const NEG: u8 = 22;
const NOT: u8 = 23;
const JUMP: u8 = 24;
const JUMP_IF_FALSE: u8 = 25;
const RETURN: u8 = 26;
const CALL: u8 = 27;

/// Check whether the given byte corresponds to a jump opcode.
#[must_use]
pub const fn is_jump_opcode(opcode: u8) -> bool {
    matches!(opcode, JUMP | JUMP_IF_FALSE)
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    /// Call the value on top of the stack.
    Call(u8),
    /// Print the value on top of the stack.
    Print,
    /// Return value.
    Return,
    /// Pop the value on top of the stack.
    Pop,
    /// Get a global variable.
    GetGlobal(Symbol),
    /// Set a global variable.
    SetGlobal(Symbol),
    /// Push a local value to the top of the stack.
    GetLocal(StackSlot),
    /// Set a local value to the value that is on top of the stack.
    SetLocal(StackSlot),
    /// Relative jump.
    Jump(InstructionOffset),
    /// Relative jump if top stack value is falsy.
    JumpIfFalse(InstructionOffset),

    // Load constants.
    /// Push a false value onto the stack.
    LoadFalse,
    /// Push a true value onto the stack.
    LoadTrue,
    /// Push a nil value onto the stack.
    LoadNil,

    // Unary operations.
    /// Numeric negation of the topmost stack value.
    Neg,
    /// Boolean not of the topmost stack value.
    Not,

    // Binary operations.
    /// Multiply the two topmost values on the stack.
    Mul,
    /// Divide the two topmost values on the stack.
    Div,
    /// Add the two topmost values on the stack.
    Add,
    /// Subtract the two topmost values on the stack.
    Sub,
    /// Evaluate less than between the two topmost values on the stack.
    Lt,
    /// Evaluate less than or equal between the two topmost values on the stack.
    Le,
    /// Evaluate greater than between the two topmost values on the stack.
    Gt,
    /// Evaluate greater than or equal between the two topmost values on the stack.
    Ge,
    /// Evaluate inequality between the two topmost values on the stack.
    Ne,
    /// Evaluate equality between the two topmost values on the stack.
    Eq,

    // Single operand instructions.
    /// Read a 1-byte index to the number constant pool
    /// and then push that number onto the stack.
    LoadNumber(ConstantIndex<f64>),
    /// Read a 1-byte index to the string constant pool
    /// and then push that string onto the stack.
    LoadString(Symbol),
    /// Read a 1-byte index to the function constant pool
    /// and then push that function onto the stack.
    LoadFunction(ConstantIndex<Function>),
}

pub trait OpcodeEmitter {
    /// Emit a byte into the opcode stream.
    fn emit_u8(&mut self, value: u8);
    /// Emit a u32 into the opcode stream in little endian.
    fn emit_u32(&mut self, value: u32);
    /// Emit a i32 into the opcode stream in little endian.
    fn emit_i32(&mut self, value: i32);
}

impl Opcode {
    pub fn encode(self, chunk: &mut impl OpcodeEmitter) {
        match self {
            Opcode::Print => chunk.emit_u8(PRINT),
            Opcode::Return => chunk.emit_u8(RETURN),
            Opcode::Pop => chunk.emit_u8(POP),
            Opcode::LoadFalse => chunk.emit_u8(LOAD_FALSE),
            Opcode::LoadTrue => chunk.emit_u8(LOAD_TRUE),
            Opcode::LoadNil => chunk.emit_u8(LOAD_NIL),
            Opcode::Mul => chunk.emit_u8(MUL),
            Opcode::Div => chunk.emit_u8(DIV),
            Opcode::Add => chunk.emit_u8(ADD),
            Opcode::Sub => chunk.emit_u8(SUB),
            Opcode::Lt => chunk.emit_u8(LT),
            Opcode::Le => chunk.emit_u8(LE),
            Opcode::Gt => chunk.emit_u8(GT),
            Opcode::Ge => chunk.emit_u8(GE),
            Opcode::Ne => chunk.emit_u8(NE),
            Opcode::Eq => chunk.emit_u8(EQ),
            Opcode::Neg => chunk.emit_u8(NEG),
            Opcode::Not => chunk.emit_u8(NOT),
            Opcode::Call(arity) => {
                chunk.emit_u8(CALL);
                chunk.emit_u8(arity);
            }
            Opcode::LoadNumber(index) => {
                chunk.emit_u8(LOAD_NUMBER);
                chunk.emit_u32(index.to_u32());
            }
            Opcode::LoadFunction(index) => {
                chunk.emit_u8(LOAD_FUNCTION);
                chunk.emit_u32(index.to_u32());
            }
            Opcode::LoadString(symbol) => {
                chunk.emit_u8(LOAD_STRING);
                chunk.emit_u32(symbol.raw());
            }
            Opcode::SetGlobal(symbol) => {
                chunk.emit_u8(SET_GLOBAL);
                chunk.emit_u32(symbol.raw());
            }
            Opcode::GetGlobal(symbol) => {
                chunk.emit_u8(GET_GLOBAL);
                chunk.emit_u32(symbol.raw());
            }
            Opcode::GetLocal(slot) => {
                chunk.emit_u8(GET_LOCAL);
                chunk.emit_u32(slot.to_u32());
            }
            Opcode::SetLocal(slot) => {
                chunk.emit_u8(SET_LOCAL);
                chunk.emit_u32(slot.to_u32());
            }
            Opcode::Jump(offset) => {
                chunk.emit_u8(JUMP);
                chunk.emit_i32(offset.to_i32());
            }
            Opcode::JumpIfFalse(offset) => {
                chunk.emit_u8(JUMP_IF_FALSE);
                chunk.emit_i32(offset.to_i32());
            }
        }
    }
}

struct ByteIterator<'data> {
    data: &'data [u8],
    index: usize,
}

impl<'data> From<&'data [u8]> for ByteIterator<'data> {
    fn from(value: &'data [u8]) -> Self {
        Self {
            data: value,
            index: 0,
        }
    }
}

impl ByteIterator<'_> {
    fn next_u8(&mut self) -> Option<u8> {
        let value = self.data.get(self.index).copied();
        self.index += 1;
        value
    }

    fn next_u32(&mut self) -> Option<u32> {
        let first = *self.data.get(self.index)?;
        let second = *self.data.get(self.index + 1)?;
        let third = *self.data.get(self.index + 2)?;
        let fourth = *self.data.get(self.index + 3)?;
        self.index += 4;
        Some(u32::from_le_bytes([first, second, third, fourth]))
    }

    fn next_i32(&mut self) -> Option<i32> {
        let first = *self.data.get(self.index)?;
        let second = *self.data.get(self.index + 1)?;
        let third = *self.data.get(self.index + 2)?;
        let fourth = *self.data.get(self.index + 3)?;
        self.index += 4;
        Some(i32::from_le_bytes([first, second, third, fourth]))
    }
}

impl Opcode {
    /// Decode a byte stream into an opcode if possible.
    #[must_use]
    pub fn decode(stream: &[u8]) -> Option<Opcode> {
        let mut stream = ByteIterator::from(stream);
        match stream.next_u8()? {
            PRINT => Some(Opcode::Print),
            RETURN => Some(Opcode::Return),
            POP => Some(Opcode::Pop),
            LOAD_FALSE => Some(Opcode::LoadFalse),
            LOAD_TRUE => Some(Opcode::LoadTrue),
            LOAD_NIL => Some(Opcode::LoadNil),
            MUL => Some(Opcode::Mul),
            DIV => Some(Opcode::Div),
            ADD => Some(Opcode::Add),
            SUB => Some(Opcode::Sub),
            LT => Some(Opcode::Lt),
            LE => Some(Opcode::Le),
            GT => Some(Opcode::Gt),
            GE => Some(Opcode::Ge),
            NE => Some(Opcode::Ne),
            EQ => Some(Opcode::Eq),
            NEG => Some(Opcode::Neg),
            NOT => Some(Opcode::Not),
            CALL => {
                let arity = stream.next_u8()?;
                Some(Opcode::Call(arity))
            }
            LOAD_NUMBER => {
                let index = stream.next_u32()?;
                Some(Opcode::LoadNumber(index.into()))
            }
            LOAD_FUNCTION => {
                let index = stream.next_u32()?;
                Some(Opcode::LoadFunction(index.into()))
            }
            LOAD_STRING => {
                let symbol = stream.next_u32()?;
                Some(Opcode::LoadString(symbol.into()))
            }
            GET_GLOBAL => {
                let symbol = stream.next_u32()?;
                Some(Opcode::GetGlobal(symbol.into()))
            }
            SET_GLOBAL => {
                let symbol = stream.next_u32()?;
                Some(Opcode::SetGlobal(symbol.into()))
            }
            GET_LOCAL => {
                let symbol = stream.next_u32()?;
                Some(Opcode::GetLocal(symbol.into()))
            }
            SET_LOCAL => {
                let symbol = stream.next_u32()?;
                Some(Opcode::SetLocal(symbol.into()))
            }
            JUMP => {
                let offset = stream.next_i32()?;
                Some(Opcode::Jump(offset.into()))
            }
            JUMP_IF_FALSE => {
                let offset = stream.next_i32()?;
                Some(Opcode::JumpIfFalse(offset.into()))
            }
            _ => None,
        }
    }
}

impl Opcode {
    /// Format an opcode by writing into the given buffer.
    ///
    /// # Errors
    /// This function will error if it can not write into the buffer.
    pub fn format(
        self,
        buffer: &mut impl fmt::Write,
        resolver: &ConstantInterner,
        address: usize,
    ) -> Result<(), fmt::Error> {
        match self {
            Opcode::Print => write!(buffer, "print"),
            Opcode::Return => write!(buffer, "return"),
            Opcode::Pop => write!(buffer, "pop"),
            Opcode::LoadFalse => write!(buffer, "loadfalse"),
            Opcode::LoadTrue => write!(buffer, "loadtrue"),
            Opcode::LoadNil => write!(buffer, "loadnil"),
            Opcode::Mul => write!(buffer, "mul"),
            Opcode::Div => write!(buffer, "div"),
            Opcode::Add => write!(buffer, "add"),
            Opcode::Sub => write!(buffer, "sub"),
            Opcode::Lt => write!(buffer, "lt"),
            Opcode::Le => write!(buffer, "le"),
            Opcode::Gt => write!(buffer, "gt"),
            Opcode::Ge => write!(buffer, "ge"),
            Opcode::Ne => write!(buffer, "ne"),
            Opcode::Eq => write!(buffer, "eq"),
            Opcode::Neg => write!(buffer, "neg"),
            Opcode::Not => write!(buffer, "not"),
            Opcode::Call(arity) => {
                write!(buffer, "call {arity}")
            }
            Opcode::LoadNumber(index) => {
                write!(
                    buffer,
                    "loadnumber {}",
                    resolver.resolve_number(index).ok_or(fmt::Error)?
                )
            }
            Opcode::LoadFunction(index) => write!(buffer, "loadfunction ${}", index.to_u32()),
            Opcode::LoadString(symbol) => write!(
                buffer,
                "loadstring \"{}\"",
                resolver.resolve_string(symbol).ok_or(fmt::Error)?
            ),
            Opcode::GetGlobal(symbol) => write!(
                buffer,
                "getglob {}",
                resolver.resolve_string(symbol).ok_or(fmt::Error)?
            ),
            Opcode::SetGlobal(symbol) => write!(
                buffer,
                "setglob {}",
                resolver.resolve_string(symbol).ok_or(fmt::Error)?
            ),
            Opcode::GetLocal(slot) => write!(buffer, "getlocal ${}", slot.to_u32()),
            Opcode::SetLocal(slot) => write!(buffer, "setlocal ${}", slot.to_u32()),
            Opcode::Jump(offset) => write!(
                buffer,
                "jp ${:04x}",
                address
                    .checked_add_signed(offset.to_i32() as isize)
                    .ok_or(fmt::Error)?
            ),
            Opcode::JumpIfFalse(offset) => write!(
                buffer,
                "jne ${:04x}",
                address
                    .checked_add_signed(offset.to_i32() as isize)
                    .ok_or(fmt::Error)?
            ),
        }
    }
}
