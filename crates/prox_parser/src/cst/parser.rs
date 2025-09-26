use crate::cst::operator::{
    assignment_infix_binding_power, infix_binding_power, postfix_binding_power,
    prefix_binding_power, short_circuit_infix_binding_power,
};
use crate::cst::tree::{Node, Tree, TreeKind};
use core::cell;
use core::fmt;
use prox_lexer::{
    Lexer, SourceLookup,
    span::Span,
    token::{Token, TokenKind},
};

/// The only valid first tokens of an expression.
const EXPR_FIRST: &[TokenKind] = &[
    // Atoms
    TokenKind::NumericLiteral,
    TokenKind::KeywordTrue,
    TokenKind::KeywordFalse,
    TokenKind::KeywordNil,
    TokenKind::Ident,
    TokenKind::StringLiteral,
    TokenKind::KeywordThis,
    TokenKind::KeywordSuper,
    // Groupings
    TokenKind::LeftParenthesis,
    // Unary operators
    TokenKind::Bang,
    TokenKind::Minus,
];

/// The tokens which can only be binary ops.
const BINARY_OP_ONLY: &[TokenKind] = &[
    TokenKind::Plus,
    TokenKind::Star,
    TokenKind::Slash,
    TokenKind::KeywordAnd,
    TokenKind::KeywordOr,
    TokenKind::LessThan,
    TokenKind::LessThanEqual,
    TokenKind::GreaterThan,
    TokenKind::GreaterThanEqual,
];

/// The only valid first tokens of a statement.
const STMT_FIRST: &[TokenKind] = &[
    // For
    TokenKind::KeywordFor,
    // If
    TokenKind::KeywordIf,
    // Print
    TokenKind::KeywordPrint,
    // Return
    TokenKind::KeywordReturn,
    // While
    TokenKind::KeywordWhile,
    // Block
    TokenKind::LeftBrace,
    // Atoms
    TokenKind::NumericLiteral,
    TokenKind::KeywordTrue,
    TokenKind::KeywordFalse,
    TokenKind::KeywordNil,
    TokenKind::Ident,
    TokenKind::StringLiteral,
    TokenKind::KeywordThis,
    TokenKind::KeywordSuper,
    // Groupings
    TokenKind::LeftParenthesis,
    // Unary operators
    TokenKind::Bang,
    TokenKind::Minus,
];

/// The only valid first tokens of a declaration.
const DECL_FIRST: &[TokenKind] = &[
    // Var
    TokenKind::KeywordVar,
    // Class
    TokenKind::KeywordClass,
    // Fun
    TokenKind::KeywordFun,
    // For
    TokenKind::KeywordFor,
    // If
    TokenKind::KeywordIf,
    // Print
    TokenKind::KeywordPrint,
    // Return
    TokenKind::KeywordReturn,
    // While
    TokenKind::KeywordWhile,
    // Block
    TokenKind::LeftBrace,
    // Atoms
    TokenKind::NumericLiteral,
    TokenKind::KeywordTrue,
    TokenKind::KeywordFalse,
    TokenKind::KeywordNil,
    TokenKind::Ident,
    TokenKind::StringLiteral,
    TokenKind::KeywordThis,
    TokenKind::KeywordSuper,
    // Groupings
    TokenKind::LeftParenthesis,
    // Unary operators
    TokenKind::Bang,
    TokenKind::Minus,
];

/// All valid first tokens of an expression as a pattern.
macro_rules! expr_first {
    () => {
        // Atoms
        prox_lexer::token::TokenKind::NumericLiteral
            | prox_lexer::token::TokenKind::KeywordTrue
            | prox_lexer::token::TokenKind::KeywordFalse
            | prox_lexer::token::TokenKind::KeywordNil
            | prox_lexer::token::TokenKind::Ident
            | prox_lexer::token::TokenKind::StringLiteral
            | prox_lexer::token::TokenKind::KeywordThis
            | prox_lexer::token::TokenKind::KeywordSuper
            // Groupings
            | prox_lexer::token::TokenKind::LeftParenthesis
            // Unary operators
            | prox_lexer::token::TokenKind::Bang
            | prox_lexer::token::TokenKind::Minus
    };
}

/// All valid first tokens of a statement as a pattern.
macro_rules! stmt_first {
    () => {
        prox_lexer::token::TokenKind::KeywordFor
            | prox_lexer::token::TokenKind::KeywordIf
            | prox_lexer::token::TokenKind::KeywordPrint
            | prox_lexer::token::TokenKind::KeywordReturn
            | prox_lexer::token::TokenKind::KeywordWhile
            | prox_lexer::token::TokenKind::LeftBrace
            | prox_lexer::token::TokenKind::NumericLiteral
            | prox_lexer::token::TokenKind::KeywordTrue
            | prox_lexer::token::TokenKind::KeywordFalse
            | prox_lexer::token::TokenKind::KeywordNil
            | prox_lexer::token::TokenKind::Ident
            | prox_lexer::token::TokenKind::StringLiteral
            | prox_lexer::token::TokenKind::KeywordThis
            | prox_lexer::token::TokenKind::KeywordSuper
            | prox_lexer::token::TokenKind::LeftParenthesis
            | prox_lexer::token::TokenKind::Bang
            | prox_lexer::token::TokenKind::Minus
    };
}

/// The recovery set for statements.
const STMT_RECOVERY: &[TokenKind] = &[
    // Declarations
    TokenKind::KeywordClass,
    TokenKind::KeywordVar,
    TokenKind::KeywordFun,
];

#[derive(Debug)]
pub enum ParseError {
    Expected {
        actual: Token,
        expected: String,
        context: String,
    },
    Custom(String),
    Multispan(Vec<(Span, String)>),
    InvalidAssignment {
        lvalue: (Span, TreeKind),
        op: Span,
        value: (Span, TreeKind),
    },
}

impl fmt::Display for ParseError {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Expected {
                actual,
                ref expected,
                ref context,
            } => {
                write!(
                    f,
                    "expected {expected}, found {:?} while parsing {context}",
                    actual.tag
                )
            }
            Self::Custom(ref msg) => {
                write!(f, "{msg}")
            }
            Self::Multispan(ref items) => {
                write!(f, "{items:?}")
            }
            Self::InvalidAssignment { lvalue, value, .. } => {
                write!(
                    f,
                    "assigning {} to an invalid l-value {} is not a valid l-value.",
                    value.1.name(),
                    lvalue.1.name()
                )
            }
        }
    }
}

#[derive(Debug)]
enum Event {
    Open {
        kind: TreeKind,
    },
    Close,
    Advance,
    UnexpectedToken {
        expected: String,
        context: String,
    },
    InvalidAssignment {
        lvalue: (Span, TreeKind),
        op: Span,
        value: (Span, TreeKind),
    },
    MultispanError(Vec<(Span, String)>),
}

#[derive(Debug, Clone, Copy)]
struct MarkOpened {
    index: usize,
}

#[derive(Debug, Clone, Copy)]
struct MarkClosed {
    index: usize,
}

pub struct Parser<'src> {
    source: SourceLookup<'src>,
    tokens: Vec<Token>,
    current_index: usize,
    fuel: cell::Cell<u32>,
    events: Vec<Event>,
    stmt_context: Vec<&'static str>,
    expr_context: Vec<&'static str>,
}

impl<'src> Parser<'src> {
    #[must_use]
    pub fn new(text: &'src str) -> Self {
        let lexer = Lexer::new(text);
        let source = lexer.get_source().clone();
        let tokens: Vec<_> = lexer.into_iter().collect();
        Self {
            tokens,
            current_index: 0,
            fuel: cell::Cell::new(256),
            events: Vec::new(),
            source,
            stmt_context: Vec::new(),
            expr_context: Vec::new(),
        }
    }
}

impl Parser<'_> {
    fn push_expr_context(&mut self, context: &'static str) {
        self.expr_context.push(context);
    }

    fn pop_expr_context(&mut self) {
        self.expr_context.pop();
    }

    fn current_expr_context(&self) -> Option<&'static str> {
        self.expr_context.last().copied()
    }

    fn push_stmt_context(&mut self, context: &'static str) {
        self.stmt_context.push(context);
    }

    fn pop_stmt_context(&mut self) {
        self.stmt_context.pop();
    }

    fn open(&mut self, target: TreeKind) -> MarkOpened {
        let _ = target;
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: TreeKind::Error,
        });
        mark
    }

    fn open_before(&mut self, old_mark: MarkClosed) -> MarkOpened {
        let mark = MarkOpened {
            index: old_mark.index,
        };
        // TODO(pavyamsiri): It is unoptimal to insert like this ~O(N) to rearrange elements.
        // Should use an index based link list approach to be able to just insert at the end.
        self.events.insert(
            old_mark.index,
            Event::Open {
                kind: TreeKind::Error,
            },
        );
        mark
    }

    fn report_error_after(
        &mut self,
        old_mark: MarkClosed,
        lvalue: (Span, TreeKind),
        op: Span,
        value: (Span, TreeKind),
    ) -> MarkClosed {
        self.events.insert(
            old_mark.index + 1,
            Event::InvalidAssignment { lvalue, op, value },
        );
        MarkClosed {
            index: old_mark.index + 1,
        }
    }

    fn close(&mut self, mark: MarkOpened, kind: TreeKind) -> MarkClosed {
        self.events[mark.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: mark.index }
    }

    fn advance(&mut self) -> MarkClosed {
        assert!(!self.is_eof(), "can't advance a token at EOF.");
        // Reset fuel
        let mark = MarkClosed {
            index: self.events.len(),
        };
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.current_index += 1;
        mark
    }

    fn consume_trivia(&mut self) {
        while self.peek_kind(0).is_trivia() {
            self.advance();
        }
    }

    fn current_context_msg(&self) -> String {
        let stmt_context = self
            .stmt_context
            .last()
            .expect("should always have a context on stack.");
        let expr_context = self.current_expr_context();
        expr_context.map_or_else(
            || (*stmt_context).to_owned(),
            |expr_context| format!("{expr_context} {stmt_context}"),
        )
    }

    fn report_error(&mut self, expected: &'static str) {
        tracing::debug!("\tReporting error {expected:}");
        self.events.push(Event::UnexpectedToken {
            expected: expected.to_owned(),
            context: self.current_context_msg(),
        });
    }

    fn advance_with_error(&mut self, expected: &'static str) {
        let msg = self.current_context_msg();
        let mark = self.open(TreeKind::Error);

        // Add error reporting here
        self.events.push(Event::UnexpectedToken {
            expected: expected.to_owned(),
            context: msg,
        });
        self.advance();
        self.close(mark, TreeKind::Error);
    }

    fn peek_token(&self, lookahead: usize) -> Token {
        assert!(
            self.fuel.get() != 0,
            "this only happens if parser is stuck."
        );
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.current_index + lookahead)
            .copied()
            .unwrap_or_else(|| Token {
                tag: TokenKind::Eof,
                span: Span {
                    start: self.source.get_text().len(),
                    length: 0,
                },
            })
    }

    fn peek_kind(&self, lookahead: usize) -> TokenKind {
        self.peek_token(lookahead).tag
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.peek_kind(0) == kind
    }

    fn at_any(&self, kinds: &[TokenKind]) -> bool {
        kinds.contains(&self.peek_kind(0))
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.eat(kind) {
            return;
        }

        let msg = self.current_context_msg();
        self.events.push(Event::UnexpectedToken {
            expected: kind.name().to_owned(),
            context: msg,
        });
    }

    fn is_eof(&self) -> bool {
        self.current_index >= self.tokens.len() || self.at(TokenKind::Eof)
    }

    fn get_tree_kind(&self, index: usize) -> Option<TreeKind> {
        if let Event::Open { kind } = self.events[index] {
            Some(kind)
        } else {
            None
        }
    }

    fn get_span_between(&self, lhs: MarkClosed, rhs: Option<MarkClosed>) -> Span {
        let mut span: Option<Span> = None;
        let mut token_iter = self.tokens.iter();
        let end = rhs.map_or(self.events.len(), |val| val.index);
        for (index, event) in self.events.iter().enumerate() {
            let token = match *event {
                Event::Advance => {
                    let token = token_iter.next().unwrap();
                    Some(token)
                }
                _ => None,
            };
            if let Some(token) = token
                && (lhs.index..end).contains(&index)
                && !token.is_whitespace()
            {
                span = span.map_or(Some(token.span), |existing_span| {
                    Some(existing_span.merge(token.span))
                });
            }
        }
        span.expect("not sure how we can not get a span.")
    }
}

impl<'src> Parser<'src> {
    /// Construct the CST.
    fn build_tree(self) -> (SourceLookup<'src>, Tree, Vec<ParseError>) {
        let mut tokens = self.tokens.into_iter().peekable();
        let mut events = self.events;
        let mut stack = Vec::new();
        let mut errors = Vec::new();

        // Popping last event to ensure that we have a non-empty stack after loop.
        assert!(
            matches!(events.pop(), Some(Event::Close)),
            "the last event should always be a close event."
        );

        for event in events {
            match event {
                Event::Open { kind } => {
                    stack.push(Tree {
                        tag: kind,
                        children: Vec::new(),
                    });
                }
                Event::Close => {
                    let tree = stack.pop().expect("close event follows an open event.");
                    stack
                        .last_mut()
                        .expect("there is one less close event than open events.")
                        .children
                        .push(Node::Tree(tree));
                }
                Event::Advance => {
                    let token = tokens
                        .next()
                        .expect("there should be as many tokens as there are advance events.");
                    stack
                        .last_mut()
                        .expect("there is always at least one open event before an advance event.")
                        .children
                        .push(Node::Token(token));
                }
                Event::UnexpectedToken { context, expected } => {
                    errors.push(ParseError::Expected {
                        actual: tokens
                            .peek()
                            .copied()
                            .expect("shouldn't have run out of tokens yet."),
                        context,
                        expected,
                    });
                }
                Event::MultispanError(items) => {
                    errors.push(ParseError::Multispan(items));
                }
                Event::InvalidAssignment { lvalue, op, value } => {
                    errors.push(ParseError::InvalidAssignment { lvalue, op, value });
                }
            }
        }

        assert!(
            matches!(
                tokens.next(),
                Some(Token {
                    tag: TokenKind::Eof,
                    ..
                })
            ),
            "there should be the last EOF token."
        );
        assert_eq!(
            stack.len(),
            1,
            "there should always be a single tree by the end."
        );

        (
            self.source,
            stack
                .pop()
                .expect("we just asserted that there was one left in the stack."),
            errors,
        )
    }
}

impl<'src> Parser<'src> {
    pub fn parse(mut self) -> (SourceLookup<'src>, Tree, Vec<ParseError>) {
        self.program();
        self.build_tree()
    }
}

// Parsing rules
impl Parser<'_> {
    /// Parses a program.
    /// ```grammar
    /// program -> declaration* ;
    /// ```
    /// Leading and trailing trivia should be consumed.
    fn program(&mut self) {
        tracing::debug!("PROGRAM");
        self.push_stmt_context("program");
        let mark = self.open(TreeKind::Program);

        // Leading trivia
        self.consume_trivia();

        while !self.is_eof() {
            let lexeme = Lexer::lexeme(&self.source, &self.tokens[self.current_index]).unwrap();
            tracing::debug!(
                "PROGRAM: AT {:?} ({}) [{}, {}/{}]",
                self.peek_kind(0),
                lexeme,
                self.is_eof(),
                self.current_index,
                self.tokens.len()
            );

            if self.at_any(DECL_FIRST) {
                self.decl();
            } else {
                self.advance_with_error("declaration");
            }
            self.consume_trivia();
        }
        self.pop_stmt_context();
        self.close(mark, TreeKind::Program);
    }

    /// Parses a declaration
    /// ```grammar
    /// declaration -> classDecl
    ///              | funDecl
    ///              | varDecl
    ///              | statement ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl(&mut self) {
        tracing::debug!("DECL");
        match self.peek_kind(0) {
            TokenKind::KeywordVar => self.decl_var(),
            TokenKind::KeywordClass => self.decl_class(),
            TokenKind::KeywordFun => self.decl_fun(),
            stmt_first!() => self.stmt(),
            _ => {
                self.advance_with_error("declaration");
            }
        }
    }

    /// Parses a statement
    /// ```grammar
    /// statement -> exprStmt
    ///            | forStmt
    ///            | ifStmt
    ///            | printStmt
    ///            | returnStmt
    ///            | whileStmt
    ///            | block ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt(&mut self) {
        tracing::debug!("STMT");
        match self.peek_kind(0) {
            TokenKind::KeywordFor => self.stmt_for(),
            TokenKind::KeywordIf => self.stmt_if(),
            TokenKind::KeywordPrint => self.stmt_print(),
            TokenKind::KeywordReturn => self.stmt_return(),
            TokenKind::KeywordWhile => self.stmt_while(),
            TokenKind::LeftBrace => self.stmt_block(),
            expr_first!() => self.stmt_expr(),
            _ => {
                self.advance_with_error("statement");
            }
        }
    }

    /// Parses a function declaration
    /// ```grammar
    /// funDecl -> "fun" function ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_fun(&mut self) {
        tracing::debug!("DECL FUN");
        assert!(
            self.at(TokenKind::KeywordFun),
            "function decls always begin with `fun`."
        );
        let mark = self.open(TreeKind::StmtFnDecl);
        self.expect(TokenKind::KeywordFun);
        self.consume_trivia();

        if self.at(TokenKind::Ident) {
            self.function();
        }

        self.close(mark, TreeKind::StmtFnDecl);
    }

    /// Parses a class declaration.
    /// ```grammar
    /// classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_class(&mut self) {
        tracing::debug!("DECL CLASS");
        assert!(
            self.at(TokenKind::KeywordClass),
            "class decls always begin with `class`."
        );
        let mark = self.open(TreeKind::StmtClassDecl);
        self.expect(TokenKind::KeywordClass);
        self.consume_trivia();

        // Class name
        self.expect(TokenKind::Ident);
        self.consume_trivia();

        if self.at(TokenKind::LessThan) {
            self.expect(TokenKind::LessThan);
            self.consume_trivia();

            // Super class
            if self.at(TokenKind::Ident) {
                self.expect(TokenKind::Ident);
                self.consume_trivia();
            } else if self.at_any(EXPR_FIRST) {
                // TODO(pavyamsiri): Dedicated error
                self.report_error("super class");
                self.expr();
                self.consume_trivia();
            }
        }

        // Open brace
        self.expect(TokenKind::LeftBrace);
        self.consume_trivia();

        // Methods
        while !self.at(TokenKind::RightBrace) && !self.is_eof() {
            // Consume method + trailing trivia
            if self.at(TokenKind::Ident) {
                let method_mark = self.open(TreeKind::StmtMethodDecl);
                self.function();
                self.close(method_mark, TreeKind::StmtMethodDecl);
                self.consume_trivia();
            } else {
                break;
            }
        }

        // Close brace
        self.expect(TokenKind::RightBrace);
        self.close(mark, TreeKind::StmtClassDecl);
    }

    /// Parses a function.
    /// ```grammar
    /// function -> IDENTIFIER param_list block ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn function(&mut self) {
        tracing::debug!("FUNCTION");
        assert!(
            self.at(TokenKind::Ident),
            "function/method declarations always begin with an identifier."
        );
        // Function/method name
        self.expect(TokenKind::Ident);
        self.consume_trivia();

        // Parameter list
        if self.at(TokenKind::LeftParenthesis) {
            self.param_list();
            self.consume_trivia();
        }

        // Function body
        if self.at(TokenKind::LeftBrace) {
            self.stmt_block();
        } else {
            self.report_error("function body block");
        }
    }

    /// Parses a parameter list.
    /// ```grammar
    /// param_list -> "(" param? ") ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn param_list(&mut self) {
        const PARAM_LIST_RECOVERY: &[TokenKind] = &[TokenKind::LeftBrace, TokenKind::KeywordFun];
        tracing::debug!("PARAM LIST");
        assert!(
            self.at(TokenKind::LeftParenthesis),
            "parameter lists always begin with `(`."
        );

        let mark = self.open(TreeKind::ParamList);

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        // Parameters
        let mut param_count = 0;
        while !self.at(TokenKind::RightParenthesis) && !self.is_eof() {
            // Consume parameter name + trailing trivia
            if self.at(TokenKind::Ident) {
                self.param();
                param_count += 1;
            } else {
                // Probably not in a parameter list anymore
                if self.at_any(PARAM_LIST_RECOVERY) {
                    break;
                }
                // Skip until we find a parameter again
                self.advance_with_error("parameter");
            }
            self.consume_trivia();
        }
        // TODO(pavyamsiri): Add a proper dedicated error type.
        if param_count > 255 {
            self.report_error("too many parameters");
        }
        // Closing parenthesis
        self.expect(TokenKind::RightParenthesis);
        self.close(mark, TreeKind::ParamList);
    }

    /// Parses a parameter.
    /// ```grammar
    /// param -> IDENTIFIER ( "," IDENTIFIER )* ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn param(&mut self) {
        tracing::debug!("PARAM");
        assert!(
            self.at(TokenKind::Ident),
            "parameters always begin with an identifier."
        );
        let mark = self.open(TreeKind::Param);

        self.expect(TokenKind::Ident);
        self.consume_trivia();

        // At closing parenthesis or comma
        if self.at(TokenKind::Comma) {
            self.expect(TokenKind::Comma);
        } else if self.at(TokenKind::RightParenthesis) {
        } else if self.at(TokenKind::Ident) {
            // TODO(pavyamsiri): Add custom missing comma error.
            self.report_error("missing a comma?");
            self.param();
            self.consume_trivia();
        }
        self.close(mark, TreeKind::Param);
    }

    /// Parses a block statement
    /// ```grammar
    /// block -> "{" declaration* "}" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_block(&mut self) {
        tracing::debug!("STMT BLOCK");
        assert!(
            self.at(TokenKind::LeftBrace),
            "blocks always begin with `{{`."
        );
        let mark = self.open(TreeKind::StmtBlock);

        self.expect(TokenKind::LeftBrace);
        self.consume_trivia();

        // Consume declarations if any
        while !self.at(TokenKind::RightBrace) && !self.is_eof() {
            self.decl();
            self.consume_trivia();
        }
        self.expect(TokenKind::RightBrace);
        self.close(mark, TreeKind::StmtBlock);
    }

    /// Parses a while statement.
    /// ```grammar
    /// whileStmt -> "while" "(" expression ")" statement ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_while(&mut self) {
        tracing::debug!("STMT WHILE");
        assert!(
            self.at(TokenKind::KeywordWhile),
            "whiles always begin with `while`."
        );
        let mark = self.open(TreeKind::StmtWhile);

        self.expect(TokenKind::KeywordWhile);
        self.consume_trivia();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        if self.at_any(STMT_FIRST) {
            self.stmt();
        } else if self.at_any(DECL_FIRST) {
            self.report_error("statement");
            self.decl();
        }

        self.close(mark, TreeKind::StmtWhile);
    }

    /// Parses a for statement.
    /// ```grammar
    /// forStmt -> "for" "(" (varDecl | exprStmt | ";") expression? ";" expression? ")" statement ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_for(&mut self) {
        tracing::debug!("STMT FOR");
        self.push_stmt_context("for");
        assert!(
            self.at(TokenKind::KeywordFor),
            "fors always begin with `for`."
        );
        let mark = self.open(TreeKind::StmtFor);

        // Keyword for
        self.expect(TokenKind::KeywordFor);
        self.consume_trivia();

        // Open bracket
        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        // Initializer
        match self.peek_kind(0) {
            TokenKind::KeywordVar => self.decl_var(),
            TokenKind::Semicolon => {
                self.expect(TokenKind::Semicolon);
            }
            expr_first!() => self.stmt_expr(),
            TokenKind::LeftBrace => {
                self.report_error("for initializer");
                self.stmt_block();
                self.consume_trivia();
                self.expect(TokenKind::Semicolon);
            }
            token if STMT_FIRST.contains(&token) => {
                self.report_error("for initializer");
                self.stmt();
            }
            _ => {}
        }
        self.consume_trivia();

        // Condition
        if self.at_any(EXPR_FIRST) {
            self.expr();
            self.consume_trivia();
            self.expect(TokenKind::Semicolon);
        } else if self.at(TokenKind::LeftBrace) {
            self.report_error("for condition");
            self.stmt_block();
            self.consume_trivia();
            self.expect(TokenKind::Semicolon);
        } else if self.at_any(DECL_FIRST) {
            self.report_error("for condition");
            self.decl();
        } else if self.at(TokenKind::Semicolon) {
            self.expect(TokenKind::Semicolon);
        }
        self.consume_trivia();

        // Increment
        if self.at_any(EXPR_FIRST) {
            self.expr();
            self.consume_trivia();
        } else if self.at_any(DECL_FIRST) {
            self.report_error("for increment");
            self.decl();
        }
        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        // Body
        if self.at_any(STMT_FIRST) {
            self.stmt();
        } else {
            self.report_error("for body");
        }
        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtFor);
    }

    /// Parses a variable declaration.
    /// ```grammar
    /// varDecl -> "var" IDENTIFIER varDeclInit? ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_var(&mut self) {
        tracing::debug!("DECL VAR");
        assert!(
            self.at(TokenKind::KeywordVar),
            "var decls always begin with `var`."
        );
        let mark = self.open(TreeKind::StmtVarDecl);

        self.expect(TokenKind::KeywordVar);
        self.consume_trivia();

        // Variable name
        if self.at(TokenKind::Ident) {
            self.expect(TokenKind::Ident);
        } else {
            self.advance_with_error("ident");
        }
        self.consume_trivia();

        if self.at(TokenKind::Equal) {
            self.stmt_var_init();
            self.consume_trivia();
        }

        self.expect(TokenKind::Semicolon);
        self.close(mark, TreeKind::StmtVarDecl);
    }

    /// Parses a variable declaration initializer.
    /// ```grammar
    /// varDeclInit -> "=" expression ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_var_init(&mut self) {
        tracing::debug!("VAR DECL INIT");
        assert!(
            self.at(TokenKind::Equal),
            "var initializers always begin with `=`."
        );
        let mark = self.open(TreeKind::VarDeclInitializer);

        self.expect(TokenKind::Equal);
        self.consume_trivia();

        self.expr();
        self.close(mark, TreeKind::VarDeclInitializer);
    }

    /// Parses a return statement e.g.
    /// ```grammar
    /// returnStmt -> "return" expression? ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_return(&mut self) {
        tracing::debug!("STMT RETURN");
        assert!(
            self.at(TokenKind::KeywordReturn),
            "returns always begin with `return`."
        );
        let mark = self.open(TreeKind::StmtReturn);

        self.expect(TokenKind::KeywordReturn);
        self.consume_trivia();

        if !self.at(TokenKind::Semicolon) {
            self.expr();
            self.consume_trivia();
        }

        self.expect(TokenKind::Semicolon);
        self.close(mark, TreeKind::StmtReturn);
    }

    /// Parses a print statement e.g.
    /// ```grammar
    /// printStmt -> "print" expression ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_print(&mut self) {
        tracing::debug!("STMT PRINT");
        assert!(
            self.at(TokenKind::KeywordPrint),
            "prints always begin with `print`."
        );
        let mark = self.open(TreeKind::StmtPrint);

        self.expect(TokenKind::KeywordPrint);
        self.consume_trivia();

        if self.at_any(EXPR_FIRST) {
            self.expr();
        } else {
            self.report_error("expression");
        }
        self.consume_trivia();

        self.expect(TokenKind::Semicolon);
        self.close(mark, TreeKind::StmtPrint);
    }

    /// Parses an expression statement e.g.
    /// ```grammar
    /// exprStmt -> expression ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_expr(&mut self) {
        tracing::debug!("STMT EXPR");
        let mark = self.open(TreeKind::StmtExpr);

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::Semicolon);
        self.close(mark, TreeKind::StmtExpr);
    }

    /// Parses an expression e.g.
    /// ```grammar
    /// expression -> assignment ;
    /// ...
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr(&mut self) {
        tracing::debug!("EXPR");
        self.push_stmt_context("expression");
        self.expr_recursive(0);
        self.pop_stmt_context();
    }

    /// Parses an expression using the Pratt parsing algorithm.
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_recursive(&mut self, min_bp: u8) -> Option<MarkClosed> {
        let mut lhs = self.expr_delimited()?;
        self.consume_trivia();

        // NOTE(pavyamsiri): Technically we should break if the min_bp is >= the binding power
        // if the operator is valid but as there are no overlaps this works as well.
        loop {
            let right = self.peek_kind(0);

            // Call operator
            if self.at(TokenKind::LeftParenthesis)
                && min_bp
                    < postfix_binding_power(right)
                        .expect("left parenthesis is the postfix call operator.")
            {
                self.push_expr_context("call");
                let mark = self.open_before(lhs);
                self.arg_list();
                self.pop_expr_context();
                lhs = self.close(mark, TreeKind::ExprCall);
                self.consume_trivia();
                continue;
            }

            // Field accessors
            if self.at(TokenKind::Dot)
                && min_bp
                    < postfix_binding_power(right).expect("dot is the postfix get/set operator.")
            {
                self.push_expr_context("get/set");
                let mark = self.open_before(lhs);

                self.expect(TokenKind::Dot);
                self.consume_trivia();

                self.expect(TokenKind::Ident);
                self.consume_trivia();

                // Set
                let kind = if self.at(TokenKind::Equal) {
                    self.expect(TokenKind::Equal);
                    self.consume_trivia();

                    self.expr();
                    self.consume_trivia();
                    TreeKind::ExprSet
                }
                // Get
                else {
                    TreeKind::ExprGet
                };

                self.pop_expr_context();
                lhs = self.close(mark, kind);
                self.consume_trivia();
                continue;
            }

            // Assignment infix
            if let Some((l_bp, r_bp)) = assignment_infix_binding_power(right)
                && min_bp < l_bp
            {
                lhs = self.expr_assignment(lhs, r_bp);
                continue;
            }

            // Short circuit infix
            if let Some((l_bp, r_bp)) = short_circuit_infix_binding_power(right)
                && min_bp < l_bp
            {
                self.push_expr_context("and/or");
                let mark = self.open_before(lhs);
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);
                self.pop_expr_context();
                lhs = self.close(mark, TreeKind::ExprInfixShortCircuit);
                self.consume_trivia();
                continue;
            }

            // Normal infix
            if let Some((l_bp, r_bp)) = infix_binding_power(right)
                && min_bp < l_bp
            {
                tracing::debug!("parse binary op");
                self.push_expr_context("binary");
                let mark = self.open_before(lhs);
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);
                self.pop_expr_context();
                lhs = self.close(mark, TreeKind::ExprInfix);
                self.consume_trivia();
                continue;
            }

            break;
        }
        Some(lhs)
    }

    fn expr_assignment(&mut self, lhs: MarkClosed, r_bp: u8) -> MarkClosed {
        self.push_expr_context("assignment");
        let mark = self.open_before(lhs);
        let op = self.advance();
        self.consume_trivia();

        let inner_lhs = MarkClosed {
            index: mark.index + 1,
        };

        let rhs = self.expr_recursive(r_bp);
        self.pop_expr_context();
        let new_lhs = self.close(mark, TreeKind::ExprInfixAssignment);
        self.consume_trivia();

        let lhs_open = &self.events[lhs.index];

        if !matches!(
            lhs_open,
            Event::Open {
                kind: TreeKind::ExprIdent
            }
        ) {
            let lvalue = self.get_span_between(inner_lhs, Some(op));
            let op_span = self.get_span_between(op, rhs);
            let value_span = self.get_span_between(rhs.unwrap(), None);
            let lhs_kind = self.get_tree_kind(mark.index + 1).unwrap();
            let _ = self.report_error_after(
                lhs,
                (lvalue, lhs_kind),
                op_span,
                (value_span, self.get_tree_kind(rhs.unwrap().index).unwrap()),
            );
        }
        new_lhs
    }

    /// Parses a delimited expression meaning any literals or groupings.
    /// Returns a mark of where to patch up.
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_delimited(&mut self) -> Option<MarkClosed> {
        let result = match self.peek_kind(0) {
            // Atoms
            TokenKind::NumericLiteral
            | TokenKind::KeywordTrue
            | TokenKind::KeywordFalse
            | TokenKind::KeywordNil
            | TokenKind::StringLiteral
            | TokenKind::KeywordThis => {
                self.push_expr_context("atom");
                let mark = self.open(TreeKind::ExprAtom);
                self.advance();
                self.pop_expr_context();
                self.close(mark, TreeKind::ExprAtom)
            }
            // Identifiers
            TokenKind::Ident => {
                self.push_expr_context("identifier");
                let mark = self.open(TreeKind::ExprIdent);
                self.expect(TokenKind::Ident);
                self.pop_expr_context();
                self.close(mark, TreeKind::ExprIdent)
            }
            // Super
            TokenKind::KeywordSuper => {
                self.push_expr_context("super");
                let mark = self.open(TreeKind::ExprIdent);

                self.expect(TokenKind::KeywordSuper);
                self.consume_trivia();

                let mut failed = false;
                if self.at(TokenKind::Dot) {
                    self.expect(TokenKind::Dot);
                    self.consume_trivia();
                } else {
                    // TODO(pavyamsiri): Dedicated error
                    self.report_error("dot");
                    failed = true;
                }

                if self.at(TokenKind::Ident) {
                    self.expect(TokenKind::Ident);
                } else {
                    // TODO(pavyamsiri): Dedicated error
                    if !failed {
                        self.report_error("method name");
                    }
                }

                self.pop_expr_context();
                self.close(mark, TreeKind::ExprSuperCall)
            }
            TokenKind::LeftParenthesis => {
                self.push_expr_context("group");
                let mark = self.open(TreeKind::ExprGroup);
                self.expect(TokenKind::LeftParenthesis);
                self.consume_trivia();
                self.expr();
                self.consume_trivia();
                self.expect(TokenKind::RightParenthesis);
                self.pop_expr_context();
                self.close(mark, TreeKind::ExprGroup)
            }
            op @ (TokenKind::Bang | TokenKind::Minus) => {
                self.push_expr_context("unary");
                let mark = self.open(TreeKind::ExprPrefix);
                let r_bp = prefix_binding_power(op).expect("! and - are prefix operators");
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);
                self.pop_expr_context();
                self.close(mark, TreeKind::ExprPrefix)
            }
            _ => {
                assert!(
                    !self.at_any(EXPR_FIRST),
                    "all match arms should have caught valid expressions"
                );
                if self.at_any(BINARY_OP_ONLY) {
                    self.advance_with_error("expression");
                }

                return None;
            }
        };
        Some(result)
    }

    /// Parses an argument list.
    /// ```grammar
    /// arg_list -> "(" arg? ") ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn arg_list(&mut self) {
        tracing::debug!("ARG LIST");
        assert!(
            self.at(TokenKind::LeftParenthesis),
            "argument lists always begin with `(`."
        );
        let mark = self.open(TreeKind::ArgList);
        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        let mut arg_count = 0;
        while !self.at(TokenKind::RightParenthesis) && !self.is_eof() {
            if self.at_any(EXPR_FIRST) {
                self.arg();
                self.consume_trivia();
            } else {
                self.advance_with_error("an arg");
            }
            arg_count += 1;
        }
        // TODO(pavyamsiri): Add a proper dedicated error type.
        if arg_count > 255 {
            self.report_error("too many arguments");
        }

        self.expect(TokenKind::RightParenthesis);
        self.close(mark, TreeKind::ArgList);
    }

    /// Parses an argument e.g.
    /// ```grammar
    /// arg -> expression ( "," expression )* ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn arg(&mut self) {
        tracing::debug!("ARG");
        let mark = self.open(TreeKind::Arg);

        self.expr();
        self.consume_trivia();

        if self.at(TokenKind::RightParenthesis) {
        } else if self.at(TokenKind::Comma) {
            self.expect(TokenKind::Comma);
        } else {
            self.advance_with_error("comma");
        }
        self.close(mark, TreeKind::Arg);
    }

    /// Parses an if statement
    /// ```grammar
    /// ifStmt -> "if" "(" expression ")" statement ( "else" statement )? ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_if(&mut self) {
        tracing::debug!("STMT IF");
        self.push_stmt_context("stmt_if");
        assert!(
            self.at(TokenKind::KeywordIf),
            "if statements always begin with `if`."
        );
        let mark = self.open(TreeKind::StmtIf);

        self.expect(TokenKind::KeywordIf);
        self.consume_trivia();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        tracing::debug!("STMT IF CONDITION");

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        tracing::debug!("STMT IF BODY");
        if self.at_any(STMT_FIRST) {
            self.stmt();
            self.consume_trivia();
        } else if self.at_any(STMT_RECOVERY) {
            self.report_error("statement");
        }
        self.consume_trivia();

        if self.at(TokenKind::KeywordElse) {
            self.expect(TokenKind::KeywordElse);
            self.consume_trivia();

            tracing::debug!("STMT IF ELSE BODY");
            if self.at_any(STMT_FIRST) {
                self.stmt();
                self.consume_trivia();
            } else if self.at_any(STMT_RECOVERY) {
                self.report_error("statement");
            }
        }

        self.close(mark, TreeKind::StmtIf);
    }
}

#[cfg(test)]
mod test {
    use crate::cst::parser::Parser;

    #[test]
    fn smoke() {
        let text = "
fun f1(x,
fun f2(x,, z) {}
fun f3() {}
        ";
        let parser = Parser::new(text);
        let (lookup, res, errors) = parser.parse();
        let mut buffer = String::new();
        res.dump(&lookup, &mut buffer, 0, true)
            .expect("can't handle formatting errors.");
        println!("{text}");
        println!("{buffer}");
        println!("{errors:?}");
    }
}
