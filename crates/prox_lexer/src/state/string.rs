use crate::source::SourceChar;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};
use prox_span::Span;

/// The state after seeing an opening double quote.
#[derive(Debug)]
pub(crate) struct StringState {
    /// The byte offset of the opening quote.
    pub(crate) start: usize,
}

impl StringState {
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
                    tag: TokenKind::ErrorUnterminatedString,
                    span: self.span(text.len()),
                }),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;

        if ch == '"' {
            LexerTransition {
                new_state: Some(State::initial(next_char.next_offset())),
                token: Some(Token {
                    tag: TokenKind::StringLiteral,
                    span: self.span(next_char.next_offset()),
                }),
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
