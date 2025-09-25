use crate::source::SourceLookup;
use crate::state::{LexerTransition, State};
use crate::token::{Token, TokenKind};

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
    pub fn lexeme(&self, token: &Token) -> Option<&'src str> {
        match token.tag {
            TokenKind::Eof => Some("'eof'"),
            _ => self.source.get_lexeme(&token.span),
        }
    }
}
