use core::convert;

/// Error when conversion from usize to `StackSlot` fails.
#[derive(Debug)]
pub struct TryFromStackSlotError;

/// The stack slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackSlot {
    /// The pointer into the stack.
    index: u32,
}

impl StackSlot {
    /// Convert to u32.
    #[must_use]
    pub const fn to_u32(self) -> u32 {
        self.index
    }

    /// Convert to usize.
    #[must_use]
    pub const fn to_usize(self) -> usize {
        self.index as usize
    }
}

impl convert::TryFrom<usize> for StackSlot {
    type Error = TryFromStackSlotError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let index: u32 = value.try_into().map_err(|_err| TryFromStackSlotError)?;
        Ok(Self { index })
    }
}

impl convert::From<u32> for StackSlot {
    fn from(value: u32) -> Self {
        Self { index: value }
    }
}

/// Offset in instruction stream.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionOffset {
    /// The relative offset.
    offset: i32,
}

impl convert::From<i32> for InstructionOffset {
    fn from(value: i32) -> Self {
        Self::from_i32(value)
    }
}

impl InstructionOffset {
    /// Convert to i32.
    #[must_use]
    pub const fn to_i32(self) -> i32 {
        self.offset
    }

    /// Convert from i32.
    #[must_use]
    pub const fn from_i32(offset: i32) -> Self {
        Self { offset }
    }
}
