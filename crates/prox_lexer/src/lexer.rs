use crate::source::SourceLookup;
use crate::state::{LexerTransition, State};
use crate::token::{Token, TokenKind};
use core::fmt;

pub struct Lexer<'src> {
    /// The source text.
    source: SourceLookup<'src>,
    /// The lexer state.
    state: State,
    /// Lookahead token.
    lookahead: Option<Token>,
}

impl<'src> Lexer<'src> {
    /// Create a lexer.
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        Self {
            source: SourceLookup::new(source),
            state: State::initial(0),
            lookahead: None,
        }
    }

    /// Lex the next token.
    pub fn next_token(&mut self) -> Token {
        if let Some(next_token) = self.lookahead.take() {
            return next_token;
        }

        self.next_token_impl()
    }

    /// Lex the next token ignoring peeked token.
    fn next_token_impl(&mut self) -> Token {
        loop {
            let text = self.source.get_text();
            let next_char = self.source.next_char();
            let LexerTransition {
                new_state,
                token,
                put_back,
            } = self.state.execute(text, next_char);

            // Change state
            if let Some(new_state) = new_state {
                self.state = new_state;
            }

            // Put back
            self.source.put_back(put_back.to_chars());

            // Return token or error
            if let Some(token) = token {
                return token;
            }
        }
    }

    /// Return the lexeme of a token.
    #[must_use]
    pub fn lexeme(&self, token: &Token) -> Option<&'src str> {
        match token.tag {
            TokenKind::Eof => Some("'eof'"),
            _ => self.source.get_lexeme(&token.span),
        }
    }

    /// Dump a token in CC format.
    ///
    /// # Errors
    /// This function will only error if writes into the buffer error.
    ///
    /// # Panics
    /// This function will panic if the given token does represent a valid span in the lexer.
    pub fn dump_token_cc(
        &self,
        buffer: &mut impl fmt::Write,
        token: &Token,
    ) -> Result<(), fmt::Error> {
        let lexeme = if token.is_eof() {
            String::new()
        } else if token.is_whitespace() {
            let lexeme = self.lexeme(token).expect("no support for invalid tokens.");
            lexeme
                .replace('\n', "\\n")
                .replace('\t', "\\t")
                .replace(' ', "_")
        } else {
            self.lexeme(token)
                .expect("no support for invalid tokens.")
                .to_owned()
        };
        let line = self.source.get_line(&token.span).start;

        match token.tag {
            TokenKind::ErrorUnterminatedString => {
                write!(buffer, "[line {line}] Error: Unterminated string.")?;
            }
            TokenKind::ErrorUnknownChar => {
                write!(
                    buffer,
                    "[line {line}] Error: Unexpected character: {lexeme}"
                )?;
            }
            _ => {
                let tag = token.tag.format_cc();
                write!(buffer, "{tag} {lexeme} ")?;
                match token.tag {
                    TokenKind::NumericLiteral => write!(
                        buffer,
                        "{:?}",
                        lexeme
                            .parse::<f64>()
                            .expect("numeric tokens should always be parseable.")
                    )?,
                    TokenKind::StringLiteral => {
                        let value = &lexeme[1..lexeme.len() - 1];
                        write!(buffer, "{value}")?;
                    }
                    _ => write!(buffer, "null")?,
                }
            }
        }

        Ok(())
    }
}
