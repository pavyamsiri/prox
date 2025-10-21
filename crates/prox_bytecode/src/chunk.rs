use crate::Opcode;
use crate::pool::ConstantInterner;
use core::convert;
use core::fmt;
use core::iter;
use prox_interner::Symbol;
use prox_lexer::SourceCode;
use prox_span::Span;

/// Error when conversion from usize to `ChunkId` fails.
#[derive(Debug)]
pub struct TryFromChunkIdError;

/// An index to a chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChunkId(u32);

impl ChunkId {
    /// Convert to usize.
    #[must_use]
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl convert::From<u32> for ChunkId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl convert::TryFrom<usize> for ChunkId {
    type Error = TryFromChunkIdError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let index: u32 = value.try_into().map_err(|_err| TryFromChunkIdError)?;
        Ok(Self(index))
    }
}

#[derive(Debug)]
pub struct Chunk {
    pub name: Symbol,
    pub stream: Box<[u8]>,
    pub starts: Box<[usize]>,
    pub spans: Box<[Span]>,
}

pub struct OpcodeIterator<'op> {
    starts: &'op [usize],
    stream: &'op [u8],
    index: usize,
}

impl iter::Iterator for OpcodeIterator<'_> {
    type Item = (Option<Opcode>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let start = *self.starts.get(self.index)?;
        self.index += 1;
        Opcode::decode(&self.stream[start..]).map(|op| (Some(op), start))
    }
}

impl<'op> iter::IntoIterator for &'op Chunk {
    type Item = (Option<Opcode>, usize);

    type IntoIter = OpcodeIterator<'op>;

    fn into_iter(self) -> Self::IntoIter {
        OpcodeIterator {
            starts: &self.starts,
            stream: &self.stream,
            index: 0,
        }
    }
}

impl Chunk {
    fn iter(&self) -> OpcodeIterator<'_> {
        self.into_iter()
    }

    /// Disassemble a chunk's bytecode into the given buffer.
    ///
    /// # Errors
    /// This function will error if it can not write into the buffer or if the chunk bytecode is malformed.
    ///
    /// # Panics
    /// May panic if certain invariants are violated. This is a programmer error if it ever occurs.
    pub fn disassemble(
        &self,
        buffer: &mut impl fmt::Write,
        source: &SourceCode<'_>,
        resolver: &ConstantInterner,
    ) -> Result<(), fmt::Error> {
        const INDENT: &str = "  ";
        let max_line = source.get_max_line();
        let num_digits = 4usize.max((max_line.checked_ilog10().unwrap_or(0) + 1) as usize);
        let name = resolver.resolve_string(self.name).ok_or(fmt::Error)?;
        // 2 space indent + 4 hexdigit address + 1 colon + 1 space + 1 L
        let prefix = format!(
            "{:>width$}- ",
            " ",
            width = INDENT.len() + 4 + 1 + 1 + num_digits
        );

        writeln!(buffer, "Chunk <{name}>:")?;
        let mut previous_line_number: Option<usize> = None;
        for (instruction_index, (opcode, offset)) in self.iter().enumerate() {
            let span = self
                .spans
                .get(instruction_index)
                .expect("# of spans == # of opcodes");
            let line_number = source.get_line(span).start;
            write!(buffer, "{INDENT}{offset:04x}:")?;

            let same = previous_line_number == Some(line_number);
            if same {
                write!(buffer, "{:>width$}| ", " ", width = num_digits - 1)?;
            } else {
                write!(
                    buffer,
                    "{:>width$} ",
                    format!("L{line_number}"),
                    width = num_digits
                )?;
            }
            if let Some(opcode) = opcode {
                opcode.format(buffer, &prefix, resolver, offset)?;
            } else {
                write!(buffer, "invalid")?;
            }
            writeln!(buffer)?;

            previous_line_number = Some(line_number);
        }
        writeln!(buffer)?;

        Ok(())
    }
}
