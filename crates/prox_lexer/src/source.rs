extern crate alloc;

use crate::span::Span;
use alloc::collections::VecDeque;
use core::cmp;
use core::ops;
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
    /// A cache for line numbers.
    line_breaks: Vec<ops::Range<usize>>,
}

impl<'src> SourceLookup<'src> {
    /// Create from a source string.
    pub fn new(source: &'src str) -> Self {
        let mut line_breaks = Vec::new();
        if source.is_empty() {
            line_breaks.push(0..1);
        } else {
            let mut cursor = 0;
            for (offset, byte) in source.bytes().enumerate() {
                let offset = offset + 1;
                if byte == b'\n' {
                    line_breaks.push(cursor..offset);
                    cursor = offset;
                }
            }
            if !source.ends_with('\n') {
                line_breaks.push(cursor..(source.len() + 1));
            }
        }

        Self {
            text: source,
            chars: source.char_indices(),
            lookahead: VecDeque::new(),
            line_breaks,
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

    /// Return the line numbers of a span.
    ///
    /// If the span is invalid, the maximum line number is returned.
    pub fn get_line(&self, span: &Span) -> ops::Range<usize> {
        let max_line = self.line_breaks.len() + 1;
        let range = span.range();
        let create_closure = move |offset: usize| {
            move |rge: &ops::Range<usize>| {
                if offset < rge.start {
                    cmp::Ordering::Greater
                } else if offset >= rge.end {
                    cmp::Ordering::Less
                } else {
                    cmp::Ordering::Equal
                }
            }
        };

        let start = self
            .line_breaks
            .binary_search_by(create_closure(range.start))
            .map(|val| val + 1)
            .unwrap_or(max_line);
        let end = self
            .line_breaks
            .binary_search_by(create_closure(range.end))
            .map(|val| val + 1)
            .ok()
            .unwrap_or(max_line);
        start..end
    }
}
