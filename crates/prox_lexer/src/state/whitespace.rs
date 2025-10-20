use crate::{
    source::SourceChar,
    state::{LexerPutBack, LexerTransition, State},
    token::{Token, TokenKind},
};
use prox_span::Span;

/// The state after seeing a whitespace.
#[derive(Debug)]
pub(crate) struct WhitespaceState {
    /// The byte offset of the first whitespace character.
    pub(crate) start: usize,
}

impl WhitespaceState {
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
                new_state: Some(State::finished()),
                token: Some(Token {
                    tag: TokenKind::Whitespace,
                    span: Span {
                        start: self.start,
                        length: text.len() - self.start,
                    },
                }),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;
        let offset = next_char.offset;

        if ch.is_ascii_whitespace() {
            LexerTransition {
                new_state: None,
                token: None,
                put_back: LexerPutBack::None,
            }
        } else {
            LexerTransition {
                new_state: Some(State::initial(offset)),
                token: Some(Token {
                    tag: TokenKind::Whitespace,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                }),
                put_back: LexerPutBack::One([next_char]),
            }
        }
    }
}
