use crate::span::Span;

/// A language token.
#[derive(Debug, Clone, Copy)]
pub struct Token {
    /// The token's type.
    pub tag: TokenKind,
    /// The span of the token in the text.
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    // Parentheses
    /// Left parenthesis `(`.
    LeftParenthesis,
    /// Right parenthesis `)`.
    RightParenthesis,

    // Braces
    /// Left curly brace `{`.
    LeftBrace,
    /// Right curly brace `}`.
    RightBrace,

    // Miscellaneous
    /// Comma `,`.
    Comma,
    /// Dot `.`.
    Dot,
    /// Minus `-`.
    Minus,
    /// Plus `+`.
    Plus,
    /// Semicolon `;`.
    Semicolon,
    /// Asterisk `*`.
    Star,
    /// Exclamation mark `!`.
    Bang,
    /// Equals sign `=`.
    Equal,
    /// Less than `<`.
    LessThan,
    /// Greater than `>`.
    GreaterThan,
    /// Forward slash `/`.
    Slash,

    // Multi-character operators.
    /// Not equals `!=`.
    BangEqual,
    /// Equals equals `==`.
    EqualEqual,
    /// Less than or equals `<=`.
    LessThanEqual,
    /// Greater than or equals `>=`.
    GreaterThanEqual,

    // Literals
    /// Numeric literals.
    NumericLiteral,
    /// String literals.
    StringLiteral,
    /// Identifiers.
    Ident,

    // Keywords
    /// The `and` keyword.
    KeywordAnd,
    /// The `class` keyword.
    KeywordClass,
    /// The `else` keyword.
    KeywordElse,
    /// The `false` keyword.
    KeywordFalse,
    /// The `for` keyword.
    KeywordFor,
    /// The `fun` keyword.
    KeywordFun,
    /// The `if` keyword.
    KeywordIf,
    /// The `nil` keyword.
    KeywordNil,
    /// The `or` keyword.
    KeywordOr,
    /// The `print` keyword.
    KeywordPrint,
    /// The `return` keyword.
    KeywordReturn,
    /// The `super` keyword.
    KeywordSuper,
    /// The `this` keyword.
    KeywordThis,
    /// The `true` keyword.
    KeywordTrue,
    /// The `var` keyword.
    KeywordVar,
    /// The `while` keyword.
    KeywordWhile,

    /// End of file.
    Eof,

    // Error tokens.
    ErrorUnterminatedString,
    ErrorUnknownChar,
}

impl Token {
    /// Create an EOF token.
    pub const fn eof(offset: usize) -> Self {
        Self {
            tag: TokenKind::Eof,
            span: Span {
                start: offset,
                length: 0,
            },
        }
    }

    /// Check if the token is EOF.
    pub const fn is_eof(&self) -> bool {
        matches!(self.tag, TokenKind::Eof)
    }

    /// Check if the token is an error.
    pub const fn is_error(&self) -> bool {
        matches!(
            self.tag,
            TokenKind::ErrorUnterminatedString | TokenKind::ErrorUnknownChar
        )
    }
}
