use crate::source::SourceChar;
use crate::state::numeric::PeriodState;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};
use prox_span::Span;

/// The state after seeing a digit.
#[derive(Debug)]
pub(crate) struct IntegerState {
    /// The byte offset of the first digit.
    pub(crate) start: usize,
}

impl IntegerState {
    /// Execute a cycle of the lexer state machine.
    pub(crate) const fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<State, Token> {
        let Some(next_char) = next_char else {
            // EOF
            return LexerTransition {
                new_state: Some(State::finished()),
                token: Some(Token {
                    tag: TokenKind::NumericLiteral,
                    span: self.span(text.len()),
                }),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;
        let offset = next_char.offset;

        if ch.is_ascii_digit() {
            LexerTransition {
                new_state: None,
                token: None,
                put_back: LexerPutBack::None,
            }
        } else if ch == '.' {
            LexerTransition {
                new_state: Some(State::Period(PeriodState {
                    start: self.start,
                    period: next_char,
                })),
                token: None,
                put_back: LexerPutBack::None,
            }
        } else {
            LexerTransition {
                new_state: Some(State::initial(offset)),
                token: Some(Token {
                    tag: TokenKind::NumericLiteral,
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
