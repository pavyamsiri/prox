use crate::span::Span;

/// A language token.
#[derive(Debug, Clone, Copy)]
pub struct Token {
    /// The token's type.
    pub tag: TokenKind,
    /// The span of the token in the text.
    pub span: Span,
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

    /// Check if the token is whitespace.
    pub const fn is_whitespace(&self) -> bool {
        matches!(self.tag, TokenKind::Whitespace)
    }

    /// Check if the token is an error.
    pub const fn is_error(&self) -> bool {
        matches!(
            self.tag,
            TokenKind::ErrorUnterminatedString | TokenKind::ErrorUnknownChar
        )
    }
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

    // Trivia tokens.
    Whitespace,
    Comment,

    // Error tokens.
    /// The error when a string is not properly terminated.
    /// Can really only happen once when the string ends in EOF as multiline strings
    /// are allowed.
    ErrorUnterminatedString,
    /// The error when the text contains an unknown character.
    ErrorUnknownChar,
}

impl TokenKind {
    /// Return the token's CC format representation.
    pub const fn format_cc(self) -> &'static str {
        match self {
            Self::LeftParenthesis => "LEFT_PAREN",
            Self::RightParenthesis => "RIGHT_PAREN",
            Self::LeftBrace => "LEFT_BRACE",
            Self::RightBrace => "RIGHT_BRACE",
            Self::Comma => "COMMA",
            Self::Dot => "DOT",
            Self::Minus => "MINUS",
            Self::Plus => "PLUS",
            Self::Semicolon => "SEMICOLON",
            Self::Star => "STAR",
            Self::Bang => "BANG",
            Self::Equal => "EQUAL",
            Self::LessThan => "LESS_THAN",
            Self::GreaterThan => "GREATER_THAN",
            Self::Slash => "SLASH",
            Self::BangEqual => "BANG_EQUAL",
            Self::EqualEqual => "EQUAL_EQUAL",
            Self::LessThanEqual => "LESS_EQUAL",
            Self::GreaterThanEqual => "GREATER_EQUAL",
            Self::NumericLiteral => "NUMBER",
            Self::StringLiteral => "STRING",
            Self::Ident => "IDENTIFIER",
            Self::KeywordAnd => "AND",
            Self::KeywordClass => "CLASS",
            Self::KeywordElse => "ELSE",
            Self::KeywordFalse => "FALSE",
            Self::KeywordFor => "FOR",
            Self::KeywordFun => "FUN",
            Self::KeywordIf => "IF",
            Self::KeywordNil => "NIL",
            Self::KeywordOr => "OR",
            Self::KeywordPrint => "PRINT",
            Self::KeywordReturn => "RETURN",
            Self::KeywordSuper => "SUPER",
            Self::KeywordThis => "THIS",
            Self::KeywordTrue => "TRUE",
            Self::KeywordVar => "VAR",
            Self::KeywordWhile => "WHILE",
            Self::Eof => "EOF",
            Self::ErrorUnterminatedString => "ERROR_UNTERMINATED_STRING",
            Self::ErrorUnknownChar => "ERROR_UNKNOWN_CHAR",
            Self::Whitespace => "TRIVIA_WHITESPACE",
            Self::Comment => "TRIVIA_COMMENT",
        }
    }
}
