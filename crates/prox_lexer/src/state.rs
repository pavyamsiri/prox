mod double;
mod ident;
mod initial;
mod numeric;
mod slash;
mod string;

use crate::source::SourceChar;
use crate::span::Span;
use crate::state::double::DoubleCharacterState;
use crate::state::string::StringState;
use crate::token::{Token, TokenKind};
use ident::IdentState;
use initial::InitialState;
use numeric::{DecimalState, IntegerState, PeriodState};
use slash::{CommentState, SlashState};

/// Represents a state transition in the lexer.
pub struct LexerTransition<S, T> {
    /// The new state to transition to unless it is `None` then no state change will take place.
    pub new_state: Option<S>,
    /// A token or an error if it is lexed.
    pub token: Option<T>,
    /// The characters to put back.
    pub put_back: LexerPutBack,
}

/// The characters to put back into the lexer in cases where the state machine needs to backtrack.
pub enum LexerPutBack {
    /// No characters to put back.
    None,
    /// One character to put back.
    One([SourceChar; 1]),
    /// Two characters to put back.
    Two([SourceChar; 2]),
}

impl LexerPutBack {
    /// Return a slice of characters.
    pub const fn to_chars(&self) -> &[SourceChar] {
        match *self {
            Self::None => &[],
            Self::One(ref chars) => chars,
            Self::Two(ref chars) => chars,
        }
    }
}

/// The state of the lexer.
#[derive(Debug)]
pub enum State {
    /// The state while parsing single-character tokens.
    Initial(InitialState),
    /// The state when lexing identifiers or keywords.
    Ident(IdentState),
    /// The state after seeing a digit signifying the start of a numeric literal.
    Integer(IntegerState),
    /// The state after seeing digit(s) and then a dot.
    Period(PeriodState),
    /// The state after seeing digit(s), a dot and at least one digit.
    Decimal(DecimalState),
    /// The state after seeing a double quote.
    String(StringState),
    /// The state after seeing a the first character of a double character token.
    DoubleCharacter(DoubleCharacterState),
    /// The state after seeing a slash.
    Slash(SlashState),
    /// The state after seeing a double slash.
    Comment(CommentState),
    /// The state when lexing is finished.
    Finished,
}

impl State {
    pub(crate) const fn initial(offset: usize) -> Self {
        Self::Initial(InitialState { start: offset })
    }

    pub(crate) const fn finished() -> Self {
        Self::Finished
    }

    pub(crate) fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<Self, Token> {
        match *self {
            Self::Finished => LexerTransition {
                new_state: None,
                token: Some(Token::eof(text.len())),
                put_back: LexerPutBack::None,
            },
            Self::Initial(ref state) => state.execute(text, next_char),
            Self::Ident(ref state) => state.execute(text, next_char),
            Self::Integer(ref state) => state.execute(text, next_char),
            Self::Period(ref state) => state.execute(text, next_char),
            Self::Decimal(ref state) => state.execute(text, next_char),
            Self::String(ref state) => state.execute(text, next_char),
            Self::DoubleCharacter(ref state) => state.execute(text, next_char),
            Self::Slash(ref state) => state.execute(text, next_char),
            Self::Comment(ref state) => state.execute(text, next_char),
        }
    }
}
