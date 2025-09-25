use crate::source::SourceChar;
use crate::span::Span;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};

/// Information to lex a double character token.
#[derive(Debug)]
pub struct DoubleCharacterSpec {
    /// The second character of the double character token.
    pub second: char,
    /// The token corresponding to the first character.
    pub single: TokenKind,
    /// The double character token.
    pub double: TokenKind,
}

/// The state after seeing the first character of a double character token.
#[derive(Debug)]
pub struct DoubleCharacterState {
    /// The byte offset of the first character.
    pub start: usize,
    /// The information to lex a double character token.
    pub spec: DoubleCharacterSpec,
}

impl DoubleCharacterState {
    /// Execute a cycle of the lexer state machine.
    pub const fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<State, Token> {
        let Some(next_char) = next_char else {
            // EOF
            return LexerTransition {
                new_state: Some(State::finished()),
                token: Some(Token {
                    tag: self.spec.single,
                    span: self.span(text.len()),
                }),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;
        let offset = next_char.offset;

        if ch == self.spec.second {
            LexerTransition {
                new_state: Some(State::initial(next_char.next_offset())),
                token: Some(Token {
                    tag: self.spec.double,
                    span: self.span(next_char.next_offset()),
                }),
                put_back: LexerPutBack::None,
            }
        } else {
            LexerTransition {
                new_state: Some(State::initial(next_char.offset)),
                token: Some(Token {
                    tag: self.spec.single,
                    span: self.span(offset),
                }),
                put_back: LexerPutBack::One([next_char]),
            }
        }
    }

    /// Create a span from the start of the state to the given offset.
    /// The offset must be strictly greater than the starting index.
    const fn span(&self, offset: usize) -> Span {
        assert!(
            self.start < offset,
            "the ending byte offset can't be before the start of token."
        );
        Span {
            start: self.start,
            length: offset - self.start,
        }
    }
}
