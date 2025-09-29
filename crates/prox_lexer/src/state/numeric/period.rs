use crate::source::SourceChar;
use crate::span::Span;
use crate::state::numeric::decimal::DecimalState;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};

/// The state after seeing digit(s) and then a dot.
#[derive(Debug)]
pub(crate) struct PeriodState {
    /// The byte offset of the first digit.
    pub(crate) start: usize,
    /// The dot and its offset.
    pub(crate) period: SourceChar,
}

impl PeriodState {
    /// Execute a cycle of the lexer state machine.
    pub(crate) const fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<State, Token> {
        let _ = text;
        let Some(next_char) = next_char else {
            // EOF
            return LexerTransition {
                new_state: Some(State::initial(self.period.offset)),
                token: Some(Token {
                    tag: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: self.period.offset - self.start,
                    },
                }),
                put_back: LexerPutBack::One([self.period]),
            };
        };

        let ch = next_char.value;

        if ch.is_ascii_digit() {
            LexerTransition {
                new_state: Some(State::Decimal(DecimalState { start: self.start })),
                token: None,
                put_back: LexerPutBack::None,
            }
        } else {
            LexerTransition {
                new_state: Some(State::initial(self.period.offset)),
                token: Some(Token {
                    tag: TokenKind::NumericLiteral,
                    span: self.span(self.period.offset),
                }),
                put_back: LexerPutBack::Two([self.period, next_char]),
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
