extern crate alloc;

use crate::span::Span;
use alloc::collections::VecDeque;
use core::str::CharIndices;

/// Represents a source character.
#[derive(Debug, Clone, Copy)]
pub struct SourceChar {
    /// The character itself.
    pub value: char,
    /// The byte offset of the character in the source.
    pub offset: usize,
}

impl SourceChar {
    /// Calculates the offset of the next character in the source assuming UTF-8 encoding.
    #[inline]
    pub const fn next_offset(&self) -> usize {
        self.offset + self.value.len_utf8()
    }
}

/// An iterator over a source text to be used with lexers.
pub struct SourceLookup<'src> {
    /// The source text.
    text: &'src str,
    /// An iterator over the characters.
    chars: CharIndices<'src>,
    /// A lookahead cache used to backtrack.
    lookahead: VecDeque<SourceChar>,
}

impl<'src> SourceLookup<'src> {
    /// Create from a source string.
    pub fn new(source: &'src str) -> Self {
        Self {
            text: source,
            chars: source.char_indices(),
            lookahead: VecDeque::new(),
        }
    }

    /// Return the next character.
    pub fn next_char(&mut self) -> Option<SourceChar> {
        if let Some(src) = self.lookahead.pop_front() {
            Some(src)
        } else {
            let (offset, value) = self.chars.next()?;
            Some(SourceChar { value, offset })
        }
    }

    /// Populate the lookahead buffer.
    pub fn put_back(&mut self, chars: &[SourceChar]) {
        self.lookahead.extend(chars);
    }

    /// Return the source.
    pub const fn get_text(&self) -> &'src str {
        self.text
    }

    /// Return the lexeme associated with the given span if the span is valid.
    pub fn get_lexeme(&self, span: &Span) -> Option<&'src str> {
        let range = span.range();
        if range.end > self.text.len() {
            return None;
        }
        Some(&self.text[range])
    }
}
