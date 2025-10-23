use crate::{
    StackSlot, UpvalueIndex,
    class::Class,
    function::{Closure, Upvalue},
    index::InstructionOffset,
    pool::ConstantInterner,
};
use core::fmt;
use prox_interner::{ConstantIndex, Symbol};

const PRINT: u8 = 0;
const LOAD_FALSE: u8 = 1;
const LOAD_TRUE: u8 = 2;
const LOAD_NIL: u8 = 3;
const LOAD_NUMBER: u8 = 4;
const LOAD_STRING: u8 = 5;
const LOAD_CLOSURE: u8 = 6;
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
const GET_UPVALUE: u8 = 28;
const SET_UPVALUE: u8 = 29;
const CLOSE_UPVALUE: u8 = 30;
const LOAD_CLASS: u8 = 31;
const GET_PROPERTY: u8 = 32;
const SET_PROPERTY: u8 = 33;
const INHERIT: u8 = 34;
const ATTACH_METHOD: u8 = 35;
const GET_SUPER: u8 = 36;

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
    /// Inherit from superclass.
    /// This instruction assumes the topmost values are:
    /// 1. the subclass.
    /// 2. the superclass.
    Inherit,
    /// Add a method to the current class.
    /// Assumes that the methody body (closure) is on top of the stack.
    AttachMethod,
    /// Pop the value on top of the stack.
    Pop,
    /// Get the superclass's method.
    GetSuper(Symbol),
    /// Get a global variable.
    GetGlobal(Symbol),
    /// Set a global variable.
    SetGlobal(Symbol),
    /// Push a local value to the top of the stack.
    GetLocal(StackSlot),
    /// Set a local value to the value that is on top of the stack.
    SetLocal(StackSlot),
    /// Get an instance's property.
    GetProperty(Symbol),
    /// Set an instance's property.
    SetProperty(Symbol),
    /// Get an upvalue.
    GetUpvalue(UpvalueIndex),
    /// Set an upvalue.
    SetUpvalue(UpvalueIndex),
    /// Hoist a local variable to the heap as an upvalue.
    CloseUpvalue,
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
    /// Read a 1-byte index to the closure constant pool
    /// and then push that closure onto the stack.
    LoadClosure(ConstantIndex<Closure>),
    /// Read a 1-byte index to the class constant pool
    /// and then push that class onto the stack.
    LoadClass(ConstantIndex<Class>),
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
            Opcode::AttachMethod => chunk.emit_u8(ATTACH_METHOD),
            Opcode::Inherit => chunk.emit_u8(INHERIT),
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
            Opcode::CloseUpvalue => chunk.emit_u8(CLOSE_UPVALUE),
            Opcode::Call(arity) => {
                chunk.emit_u8(CALL);
                chunk.emit_u8(arity);
            }
            Opcode::LoadNumber(index) => {
                chunk.emit_u8(LOAD_NUMBER);
                chunk.emit_u32(index.to_u32());
            }
            Opcode::LoadClosure(index) => {
                chunk.emit_u8(LOAD_CLOSURE);
                chunk.emit_u32(index.to_u32());
            }
            Opcode::LoadString(symbol) => {
                chunk.emit_u8(LOAD_STRING);
                chunk.emit_u32(symbol.raw());
            }
            Opcode::LoadClass(index) => {
                chunk.emit_u8(LOAD_CLASS);
                chunk.emit_u32(index.to_u32());
            }
            Opcode::GetSuper(symbol) => {
                chunk.emit_u8(GET_SUPER);
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
            Opcode::SetProperty(symbol) => {
                chunk.emit_u8(SET_PROPERTY);
                chunk.emit_u32(symbol.raw());
            }
            Opcode::GetProperty(symbol) => {
                chunk.emit_u8(GET_PROPERTY);
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
            Opcode::GetUpvalue(index) => {
                chunk.emit_u8(GET_UPVALUE);
                chunk.emit_u32(index.to_u32());
            }
            Opcode::SetUpvalue(index) => {
                chunk.emit_u8(SET_UPVALUE);
                chunk.emit_u32(index.to_u32());
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
            ATTACH_METHOD => Some(Opcode::AttachMethod),
            INHERIT => Some(Opcode::Inherit),
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
            CLOSE_UPVALUE => Some(Opcode::CloseUpvalue),
            CALL => {
                let arity = stream.next_u8()?;
                Some(Opcode::Call(arity))
            }
            LOAD_NUMBER => {
                let index = stream.next_u32()?;
                Some(Opcode::LoadNumber(index.into()))
            }
            LOAD_CLOSURE => {
                let index = stream.next_u32()?;
                Some(Opcode::LoadClosure(index.into()))
            }
            LOAD_CLASS => {
                let index = stream.next_u32()?;
                Some(Opcode::LoadClass(index.into()))
            }
            LOAD_STRING => {
                let symbol = stream.next_u32()?;
                Some(Opcode::LoadString(symbol.into()))
            }
            GET_SUPER => {
                let symbol = stream.next_u32()?;
                Some(Opcode::GetSuper(symbol.into()))
            }
            GET_GLOBAL => {
                let symbol = stream.next_u32()?;
                Some(Opcode::GetGlobal(symbol.into()))
            }
            SET_GLOBAL => {
                let symbol = stream.next_u32()?;
                Some(Opcode::SetGlobal(symbol.into()))
            }
            GET_PROPERTY => {
                let symbol = stream.next_u32()?;
                Some(Opcode::GetProperty(symbol.into()))
            }
            SET_PROPERTY => {
                let symbol = stream.next_u32()?;
                Some(Opcode::SetProperty(symbol.into()))
            }
            GET_LOCAL => {
                let slot = stream.next_u32()?;
                Some(Opcode::GetLocal(slot.into()))
            }
            SET_LOCAL => {
                let slot = stream.next_u32()?;
                Some(Opcode::SetLocal(slot.into()))
            }
            GET_UPVALUE => {
                let index = stream.next_u32()?;
                Some(Opcode::GetUpvalue(index.into()))
            }
            SET_UPVALUE => {
                let index = stream.next_u32()?;
                Some(Opcode::SetUpvalue(index.into()))
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
        prefix: &str,
        resolver: &ConstantInterner,
        address: usize,
    ) -> Result<(), fmt::Error> {
        match self {
            Opcode::AttachMethod => write!(buffer, "attachmethod"),
            Opcode::Inherit => write!(buffer, "inherit"),
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
            Opcode::LoadClosure(index) => {
                let closure = resolver.resolve_closure(index).ok_or(fmt::Error)?;
                let name = resolver.resolve_string(closure.name).ok_or(fmt::Error)?;
                write!(buffer, "loadclosure {name}")?;

                format_closure(buffer, prefix, closure)?;
                Ok(())
            }
            Opcode::LoadString(symbol) => write!(
                buffer,
                "loadstring \"{}\"",
                resolver.resolve_string(symbol).ok_or(fmt::Error)?
            ),
            Opcode::LoadClass(index) => {
                let class = resolver.resolve_class(index).ok_or(fmt::Error)?;
                write!(
                    buffer,
                    "loadclass {}",
                    resolver.resolve_string(class.name).ok_or(fmt::Error)?
                )?;
                Ok(())
            }
            Opcode::GetSuper(symbol) => write!(
                buffer,
                "getsuper {}",
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
            Opcode::GetProperty(symbol) => write!(
                buffer,
                "getprop {}",
                resolver.resolve_string(symbol).ok_or(fmt::Error)?
            ),
            Opcode::SetProperty(symbol) => write!(
                buffer,
                "setprop {}",
                resolver.resolve_string(symbol).ok_or(fmt::Error)?
            ),
            Opcode::GetLocal(slot) => write!(buffer, "getlocal ${}", slot.to_u32()),
            Opcode::SetLocal(slot) => write!(buffer, "setlocal ${}", slot.to_u32()),
            Opcode::GetUpvalue(index) => write!(buffer, "getupvalue ${}", index.to_u32()),
            Opcode::SetUpvalue(index) => write!(buffer, "setupvalue ${}", index.to_u32()),
            Opcode::CloseUpvalue => write!(buffer, "closeupvalue"),
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

fn format_closure(
    buffer: &mut impl fmt::Write,
    prefix: &str,
    closure: &Closure,
) -> Result<(), fmt::Error> {
    if !closure.upvalues.is_empty() {
        writeln!(buffer)?;
        for (line_index, upvalue) in closure.upvalues.iter().enumerate() {
            match *upvalue {
                Upvalue::Local(slot) => {
                    write!(buffer, "{prefix}local {}", slot.to_u32())?;
                }
                Upvalue::Upvalue(upvalue_index) => {
                    write!(buffer, "{prefix}upvalue {}", upvalue_index.to_u32())?;
                }
            }
            if line_index < (closure.upvalues.len() - 1) {
                writeln!(buffer)?;
            }
        }
    }
    Ok(())
}
