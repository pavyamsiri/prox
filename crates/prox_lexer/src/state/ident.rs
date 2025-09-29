extern crate alloc;

use crate::source::SourceChar;
use crate::span::Span;
use crate::state::{LexerPutBack, LexerTransition, State};
use crate::token::{Token, TokenKind};
use alloc::collections::BTreeMap;
use std::sync::LazyLock;

/// The hashmap for keywords
pub(crate) static KEYWORD_HASHMAP: LazyLock<BTreeMap<&'static str, TokenKind>> =
    LazyLock::new(|| {
        let mut map = BTreeMap::new();
        map.insert("and", TokenKind::KeywordAnd);
        map.insert("class", TokenKind::KeywordClass);
        map.insert("else", TokenKind::KeywordElse);
        map.insert("false", TokenKind::KeywordFalse);
        map.insert("for", TokenKind::KeywordFor);
        map.insert("fun", TokenKind::KeywordFun);
        map.insert("if", TokenKind::KeywordIf);
        map.insert("nil", TokenKind::KeywordNil);
        map.insert("or", TokenKind::KeywordOr);
        map.insert("print", TokenKind::KeywordPrint);
        map.insert("return", TokenKind::KeywordReturn);
        map.insert("super", TokenKind::KeywordSuper);
        map.insert("this", TokenKind::KeywordThis);
        map.insert("true", TokenKind::KeywordTrue);
        map.insert("var", TokenKind::KeywordVar);
        map.insert("while", TokenKind::KeywordWhile);
        map
    });

/// The state after seeing the first character of an identifier.
#[derive(Debug)]
pub(crate) struct IdentState {
    /// The byte offset of the beginning of an identifier.
    pub start: usize,
}

impl IdentState {
    /// Execute a cycle of the lexer state machine.
    pub(crate) fn execute(
        &self,
        text: &str,
        next_char: Option<SourceChar>,
    ) -> LexerTransition<State, Token> {
        let Some(next_char) = next_char else {
            // EOF
            let token = self.lex_ident(text, text.len());
            return LexerTransition {
                new_state: Some(State::finished()),
                token: Some(token),
                put_back: LexerPutBack::None,
            };
        };

        let ch = next_char.value;
        let offset = next_char.offset;

        if ch.is_alphanumeric() || ch == '_' {
            LexerTransition {
                new_state: None,
                token: None,
                put_back: LexerPutBack::None,
            }
        } else {
            let token = self.lex_ident(text, offset);
            LexerTransition {
                new_state: Some(State::initial(offset)),
                token: Some(token),
                put_back: LexerPutBack::One([next_char]),
            }
        }
    }

    /// Lex an identifier or keyword.
    fn lex_ident(&self, text: &str, offset: usize) -> Token {
        assert!(
            self.start < offset,
            "the ending byte offset can't be before the start of token."
        );

        let span = Span {
            start: self.start,
            length: offset - self.start,
        };

        let lexeme = &text[span.range()];

        let tag = KEYWORD_HASHMAP
            .get(lexeme)
            .copied()
            .unwrap_or(TokenKind::Ident);
        Token { tag, span }
    }
}
