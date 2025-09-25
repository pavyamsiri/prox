use crate::source::SourceChar;
use crate::span::Span;
use crate::state::double::{DoubleCharacterSpec, DoubleCharacterState};
use crate::state::ident::IdentState;
use crate::state::numeric::IntegerState;
use crate::state::slash::SlashState;
use crate::state::string::StringState;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};

/// The start state of the lexer state machine.
#[derive(Debug)]
pub struct InitialState {
    /// The current byte offset.
    pub start: usize,
}

impl InitialState {
    /// Execute a cycle of the lexer state machine.
    pub fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<State, Token> {
        let Some(next_char) = next_char else {
            // EOF
            return LexerTransition {
                new_state: Some(State::finished()),
                token: Some(Token::eof(text.len())),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;
        let offset = next_char.offset;
        let len_utf8 = ch.len_utf8();

        let create_single = |tag: TokenKind| -> LexerTransition<State, Token> {
            LexerTransition {
                new_state: Some(Self::next(&next_char)),
                token: Some(Token {
                    tag,
                    span: self.span(len_utf8),
                }),
                put_back: LexerPutBack::None,
            }
        };

        let create_double = |spec: DoubleCharacterSpec| -> LexerTransition<State, Token> {
            LexerTransition {
                new_state: Some(State::DoubleCharacter(DoubleCharacterState {
                    start: self.start,
                    spec,
                })),
                token: None,
                put_back: LexerPutBack::None,
            }
        };
        match ch {
            '\t' | '\x0C' | '\r' | ' ' | '\n' => LexerTransition {
                new_state: Some(State::initial(next_char.next_offset())),
                token: None,
                put_back: LexerPutBack::None,
            },
            '0'..='9' => LexerTransition {
                new_state: Some(State::Integer(IntegerState { start: self.start })),
                token: None,
                put_back: LexerPutBack::None,
            },
            '"' => LexerTransition {
                new_state: Some(State::String(StringState { start: self.start })),
                token: None,
                put_back: LexerPutBack::None,
            },
            '(' => create_single(TokenKind::LeftParenthesis),
            ')' => create_single(TokenKind::RightParenthesis),
            '{' => create_single(TokenKind::LeftBrace),
            '}' => create_single(TokenKind::RightBrace),
            ',' => create_single(TokenKind::Comma),
            '.' => create_single(TokenKind::Dot),
            '-' => create_single(TokenKind::Minus),
            '+' => create_single(TokenKind::Plus),
            ';' => create_single(TokenKind::Semicolon),
            '*' => create_single(TokenKind::Star),
            '!' => create_double(DoubleCharacterSpec {
                second: '=',
                single: TokenKind::Bang,
                double: TokenKind::BangEqual,
            }),
            '=' => create_double(DoubleCharacterSpec {
                second: '=',
                single: TokenKind::Equal,
                double: TokenKind::EqualEqual,
            }),
            '<' => create_double(DoubleCharacterSpec {
                second: '=',
                single: TokenKind::LessThan,
                double: TokenKind::LessThanEqual,
            }),
            '>' => create_double(DoubleCharacterSpec {
                second: '=',
                single: TokenKind::GreaterThan,
                double: TokenKind::GreaterThanEqual,
            }),
            '/' => LexerTransition {
                new_state: Some(State::Slash(SlashState { start: self.start })),
                token: None,
                put_back: LexerPutBack::None,
            },
            ch => {
                if ch.is_alphabetic() || ch == '_' {
                    LexerTransition {
                        new_state: Some(State::Ident(IdentState { start: offset })),
                        token: None,
                        put_back: LexerPutBack::None,
                    }
                } else {
                    LexerTransition {
                        new_state: Some(Self::next(&next_char)),
                        token: Some(Token {
                            tag: TokenKind::ErrorUnknownChar,
                            span: self.span(len_utf8),
                        }),
                        put_back: LexerPutBack::None,
                    }
                }
            }
        }
    }

    /// Return the next initial state.
    const fn next(next_char: &SourceChar) -> State {
        State::initial(next_char.next_offset())
    }

    /// Return the span from the start of the state with the given `length`.
    const fn span(&self, length: usize) -> Span {
        Span {
            start: self.start,
            length,
        }
    }
}
