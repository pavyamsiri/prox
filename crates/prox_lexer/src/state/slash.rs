use crate::source::SourceChar;
use crate::span::Span;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};

/// The state after seeing a slash.
#[derive(Debug)]
pub struct SlashState {
    /// The byte offset of the slash.
    pub start: usize,
}

impl SlashState {
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
                    tag: TokenKind::Slash,
                    span: self.span(text.len()),
                }),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;
        let offset = next_char.offset;

        if ch == '/' {
            LexerTransition {
                new_state: Some(State::Comment(CommentState)),
                token: None,
                put_back: LexerPutBack::None,
            }
        } else {
            LexerTransition {
                new_state: Some(State::initial(offset)),
                token: Some(Token {
                    tag: TokenKind::Slash,
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

/// The state after seeing a double slash.
#[derive(Debug)]
pub struct CommentState;

impl CommentState {
    /// Execute a cycle of the lexer state machine.
    #[expect(clippy::unused_self, reason = "This signature mirrors other states.")]
    pub const fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<State, Token> {
        let _ = text;
        let Some(next_char) = next_char else {
            // EOF
            return LexerTransition {
                new_state: Some(State::finished()),
                token: None,
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;

        if ch == '\n' {
            LexerTransition {
                new_state: Some(State::initial(next_char.next_offset())),
                token: None,
                put_back: LexerPutBack::None,
            }
        } else {
            LexerTransition {
                new_state: None,
                token: None,
                put_back: LexerPutBack::None,
            }
        }
    }
}
