mod sets;

use crate::cst::operator::{
    assignment_infix_binding_power, infix_binding_power, postfix_binding_power,
    prefix_binding_power, short_circuit_infix_binding_power,
};
use crate::cst::tree::{Cst, Node, TreeKind};
use core::fmt;
use core::{cell, iter};
use prox_lexer::SourceCode;
use prox_lexer::{
    Lexer,
    span::Span,
    token::{Token, TokenKind},
};
use sets::{
    BINARY_OP_ONLY, DECL_FIRST, EXPR_FIRST, STMT_FIRST, STMT_RECOVERY, expr_first, stmt_first,
};

/// A result of a parse.
#[derive(Debug)]
pub struct ParseResult<'src> {
    /// The program root.
    pub root: Cst,
    /// The source code.
    pub source: SourceCode<'src>,
    /// The parse errors.
    pub errors: Vec<ParseError>,
}

impl ParseResult<'_> {
    /// Dump the CST.
    ///
    /// # Panics
    /// If parsed tree does not correspond to the given the source lookup.
    ///
    /// # Errors
    /// Can error if the given buffer becomes full.
    pub fn dump(
        &self,
        buffer: &mut impl fmt::Write,
        level: usize,
        skip_trivia: bool,
    ) -> Result<(), fmt::Error> {
        self.root.dump(&self.source, buffer, level, skip_trivia)
    }
}

/// The parsing context at the declaration/statement level.
#[derive(Debug)]
enum StmtContext {
    /// Parsing the program.
    Program,
    /// Parsing a declaration.
    Decl,
    /// Parsing a statement.
    Stmt,
    /// Parsing a class declaration.
    ClassDecl,
    /// Parsing a class declaration.
    VarDecl,
    /// Parsing a function declaration.
    FunDecl,
    /// Parsing a expression statement.
    ExprStmt,
    /// Parsing a for statement.
    For,
    /// Parsing a print statement.
    Print,
    /// Parsing a return statement.
    Return,
    /// Parsing a while statement.
    While,
    /// Parsing a block statement.
    Block,
    /// Parsing an if statement.
    If,
    /// Parsing an expression.
    Expr,
}

impl fmt::Display for StmtContext {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            StmtContext::Program => write!(f, "program"),
            StmtContext::ClassDecl => write!(f, "class declaration"),
            StmtContext::VarDecl => write!(f, "variable declaration"),
            StmtContext::FunDecl => write!(f, "function declaration"),
            StmtContext::Expr => write!(f, "expression statement"),
            StmtContext::For => write!(f, "for statement"),
            StmtContext::Print => write!(f, "print statement"),
            StmtContext::Return => write!(f, "return statement"),
            StmtContext::While => write!(f, "while statement"),
            StmtContext::Block => write!(f, "block statement"),
            StmtContext::Decl => write!(f, "declaration"),
            StmtContext::Stmt => write!(f, "statement"),
            StmtContext::ExprStmt => write!(f, "expression"),
            StmtContext::If => write!(f, "if statement"),
        }
    }
}

/// The context during expression parsing.
#[derive(Debug)]
enum ExprContext {
    /// Parsing a call.
    Call,
    /// Parsing either get or set.
    GetSet,
    /// Parsing either and or or.
    AndOr,
    /// Parsing a binary expression.
    Binary,
    /// Parsing an assignment.
    Assignment,
    /// Parsing a super method call.
    Super,
    /// Parsing a group.
    Group,
    /// Parsing a unary.
    Unary,
    /// Parsing an identifier.
    Ident,
    /// Parsing keyword this.
    This,
    /// Parsing literal true.
    True,
    /// Parsing literal false.
    False,
    /// Parsing literal nil.
    Nil,
    /// Parsing string literal.
    StringLiteral,
    /// Parsing numeric literal.
    NumericLiteral,
}

impl fmt::Display for ExprContext {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ExprContext::Call => write!(f, "call expression"),
            ExprContext::GetSet => write!(f, "set/get expression"),
            ExprContext::AndOr => write!(f, "and/or expression"),
            ExprContext::Binary => write!(f, "binary expression"),
            ExprContext::Assignment => write!(f, "assignment expression"),
            ExprContext::Ident => write!(f, "identifier"),
            ExprContext::Super => write!(f, "super"),
            ExprContext::Group => write!(f, "group expression"),
            ExprContext::Unary => write!(f, "unary expression"),
            ExprContext::This => write!(f, "keyword this"),
            ExprContext::True => write!(f, "literal true"),
            ExprContext::False => write!(f, "literal false"),
            ExprContext::Nil => write!(f, "literal nil"),
            ExprContext::StringLiteral => write!(f, "string literal"),
            ExprContext::NumericLiteral => write!(f, "numeric literal"),
        }
    }
}

/// An error encountered when parsing.
#[derive(Debug)]
pub enum ParseError {
    /// The parser expected a token but got another token instead.
    Expected {
        /// The actual token.
        actual: Token,
        /// The expected token.
        expected: String,
        /// The parsing context at the time.
        context: String,
    },
    /// Attempted to assign to an invalid l-value.
    InvalidAssignment {
        /// The invalid l-value.
        lvalue: Span,
        /// The value being assigned.
        value: Span,
    },
    /// Missing dot after `super`.
    MissingDotAfterSuper {
        /// The super token.
        super_token: Token,
        /// The non-dot token.
        actual: Token,
    },
    /// Missing method name after `super.`.
    MissingSuperMethod {
        /// The super token.
        super_token: Token,
        /// The non-method name token.
        actual: Token,
    },
    /// Too many arguments.
    TooManyArguments {
        /// The `(` token.
        list_start: Token,
        /// The `)` token.
        list_end: Token,
    },
    /// Too many parameters.
    TooManyParameters {
        /// The `(` token.
        list_start: Token,
        /// The `)` token.
        list_end: Token,
    },
    /// Missing a comma while parsing an argument or parameter list.
    MissingComma {
        /// The parse context.
        context: &'static str,
        /// The non-comma token.
        actual: Token,
    },
    /// When inheriting from a super class, the class name is invalid.
    InvalidSuperclass {
        /// The class declaration.
        class_decl: Span,
        /// The tree that isn't the super class name.
        actual: Span,
    },
}

/// Events emitted during parsing.
#[derive(Debug)]
enum Event {
    /// Open a new syntax tree.
    Open {
        /// The type of tree being constructed.
        kind: TreeKind,
        /// Pointer to open event that logically occurs before this event.
        open_before: Option<usize>,
    },
    /// Close the currently open tree.
    Close,
    /// Consume token and add to currently open tree.
    Advance,
}

#[derive(Debug, Clone, Copy)]
struct MarkOpened {
    index: usize,
}

#[derive(Debug, Clone, Copy)]
struct MarkClosed {
    index: usize,
}

struct EventIterator<'event> {
    events: &'event [Event],
    visited: Vec<bool>,
    stack: Vec<usize>,
}

impl<'event> EventIterator<'event> {
    fn new(events: &'event [Event]) -> Self {
        let visited = vec![false; events.len()];
        let stack = (0..events.len()).rev().collect();
        Self {
            events,
            visited,
            stack,
        }
    }
}

impl<'event> iter::Iterator for EventIterator<'event> {
    type Item = (usize, &'event Event);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(event_index) = self.stack.pop() {
            if self.visited[event_index] {
                continue;
            }

            let event = &self.events[event_index];
            match event {
                &Event::Open {
                    open_before: Some(next_index),
                    ..
                } if !self.visited[next_index] => {
                    self.stack.push(event_index);
                    self.stack.push(next_index);
                }
                ev => {
                    self.visited[event_index] = true;
                    return Some((event_index, ev));
                }
            }
        }
        None
    }
}

pub struct Parser<'src> {
    source: SourceCode<'src>,
    tokens: Vec<Token>,
    current_index: usize,
    fuel: cell::Cell<u32>,
    events: Vec<Event>,
    errors: Vec<ParseError>,
    stmt_context: Vec<StmtContext>,
    expr_context: Vec<ExprContext>,
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
            errors: Vec::new(),
        }
    }
}

impl Parser<'_> {
    fn push_expr_context(&mut self, context: ExprContext) {
        self.expr_context.push(context);
    }

    fn pop_expr_context(&mut self) {
        self.expr_context.pop();
    }

    fn current_expr_context(&self) -> Option<&ExprContext> {
        self.expr_context.last()
    }

    fn push_stmt_context(&mut self, context: StmtContext) {
        self.stmt_context.push(context);
    }

    fn pop_stmt_context(&mut self) {
        self.stmt_context.pop();
    }

    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: TreeKind::Error,
            open_before: None,
        });
        mark
    }

    fn open_before(&mut self, old_mark: MarkClosed) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        let old_kind = if let Event::Open { kind, .. } = self.events[old_mark.index] {
            kind
        } else {
            tracing::warn!("Opening event that is not an open event!");
            TreeKind::Error
        };

        self.events.push(Event::Open {
            kind: TreeKind::Error,
            open_before: None,
        });
        self.events[old_mark.index] = Event::Open {
            kind: old_kind,
            open_before: Some(mark.index),
        };
        mark
    }

    /// Report given error.
    fn report_error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    fn close(&mut self, mark: MarkOpened, kind: TreeKind) -> MarkClosed {
        let open_before = if let Event::Open { open_before, .. } = self.events[mark.index] {
            open_before
        } else {
            None
        };
        self.events[mark.index] = Event::Open { kind, open_before };
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
            || (*stmt_context).to_string(),
            |expr_context| format!("{expr_context} {stmt_context}"),
        )
    }

    fn report_error_with_message(&mut self, expected: &'static str) {
        tracing::debug!("\tReporting error {expected:}");
        self.errors.push(ParseError::Expected {
            expected: expected.to_owned(),
            context: self.current_context_msg(),
            actual: self.peek_token(0),
        });
    }

    fn advance_with_error(&mut self, expected: &'static str) {
        let msg = self.current_context_msg();
        let mark = self.open();

        // Add error reporting here
        self.errors.push(ParseError::Expected {
            expected: expected.to_owned(),
            context: msg,
            actual: self.peek_token(0),
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
        self.errors.push(ParseError::Expected {
            expected: kind.name().to_owned(),
            context: msg,
            actual: self.peek_token(0),
        });
    }

    fn is_eof(&self) -> bool {
        self.current_index >= self.tokens.len() || self.at(TokenKind::Eof)
    }

    fn get_span_between(&self, lhs: MarkClosed, rhs: Option<MarkClosed>) -> Span {
        let mut span: Option<Span> = None;
        let mut token_iter = self.tokens.iter();
        let end = rhs.map_or(self.events.len(), |val| val.index);
        let event_iterator = EventIterator::new(&self.events);
        let mut in_span = false;
        for (index, event) in event_iterator {
            let token = match *event {
                Event::Advance => {
                    let token = token_iter.next().unwrap();
                    Some(token)
                }
                _ => None,
            };
            if index == lhs.index {
                in_span = true;
            } else if index == end {
                in_span = false;
            }
            if let Some(token) = token
                && in_span
                && !token.is_trivia()
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
    #[expect(
        clippy::result_large_err,
        reason = "neither the ok and err variants are unlikely."
    )]
    fn build_tree(self) -> ParseResult<'src> {
        let mut tokens = self.tokens.into_iter();
        let mut events = self.events;
        let mut stack = Vec::new();

        // Popping last event to ensure that we have a non-empty stack after loop.
        assert!(
            matches!(events.pop(), Some(Event::Close)),
            "the last event should always be a close event."
        );

        let event_iterator = EventIterator::new(&events);
        for (_, event) in event_iterator {
            match *event {
                Event::Open { kind, .. } => {
                    stack.push(Cst {
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

        let root = stack
            .pop()
            .expect("we just asserted that there was one left in the stack.");
        ParseResult {
            root,
            source: self.source,
            errors: self.errors,
        }
    }
}

impl<'src> Parser<'src> {
    /// Parse the source code and return either a correct or incorrect parse tree.
    ///
    /// # Errors
    /// If the given source code is malformed then the parse will not succeed.
    pub fn parse(mut self) -> ParseResult<'src> {
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
        self.push_stmt_context(StmtContext::Program);
        let mark = self.open();

        // Leading trivia
        self.consume_trivia();

        while !self.is_eof() {
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
        self.push_stmt_context(StmtContext::Decl);
        match self.peek_kind(0) {
            TokenKind::KeywordVar => self.decl_var(),
            TokenKind::KeywordClass => self.decl_class(),
            TokenKind::KeywordFun => self.decl_fun(),
            stmt_first!() => self.stmt(),
            _ => {
                self.advance_with_error("declaration");
            }
        }
        self.pop_stmt_context();
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
        self.push_stmt_context(StmtContext::Stmt);
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
        self.pop_stmt_context();
    }

    /// Parses a function declaration
    /// ```grammar
    /// funDecl -> "fun" function ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_fun(&mut self) {
        assert!(
            self.at(TokenKind::KeywordFun),
            "function decls always begin with `fun`."
        );

        self.push_stmt_context(StmtContext::FunDecl);
        let mark = self.open();

        self.expect(TokenKind::KeywordFun);
        self.consume_trivia();

        if self.at(TokenKind::Ident) {
            self.function();
        }

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtFnDecl);
    }

    /// Parses a class declaration.
    /// ```grammar
    /// classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_class(&mut self) {
        assert!(
            self.at(TokenKind::KeywordClass),
            "class decls always begin with `class`."
        );
        self.push_stmt_context(StmtContext::ClassDecl);
        let mark = self.open();

        let class_start = self.peek_token(0);
        self.expect(TokenKind::KeywordClass);
        self.consume_trivia();

        // Class name
        let class_end = self.peek_token(0);
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
                let expr_mark = self
                    .expr()
                    .expect("already checked we are at a expression boundary.");

                self.report_error(ParseError::InvalidSuperclass {
                    class_decl: class_start.span.merge(class_end.span),
                    actual: self.get_span_between(expr_mark, None),
                });
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
                let method_mark = self.open();
                self.function();
                self.close(method_mark, TreeKind::StmtMethodDecl);
                self.consume_trivia();
            } else {
                break;
            }
        }
        // Close brace
        self.expect(TokenKind::RightBrace);

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtClassDecl);
    }

    /// Parses a function.
    /// ```grammar
    /// function -> IDENTIFIER param_list block ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn function(&mut self) {
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
            self.report_error_with_message("function body block");
        }
    }

    /// Parses a parameter list.
    /// ```grammar
    /// param_list -> "(" param? ") ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn param_list(&mut self) {
        const PARAM_LIST_RECOVERY: &[TokenKind] = &[TokenKind::LeftBrace, TokenKind::KeywordFun];
        assert!(
            self.at(TokenKind::LeftParenthesis),
            "parameter lists always begin with `(`."
        );

        let mark = self.open();

        let list_start = self.peek_token(0);
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

        if param_count > 255 {
            let list_end = self.peek_token(0);
            self.report_error(ParseError::TooManyParameters {
                list_start,
                list_end,
            });
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
        assert!(
            self.at(TokenKind::Ident),
            "parameters always begin with an identifier."
        );
        let mark = self.open();

        self.expect(TokenKind::Ident);
        let actual = self.peek_token(0);
        self.consume_trivia();

        // At closing parenthesis or comma
        if self.at(TokenKind::Comma) {
            self.expect(TokenKind::Comma);
        } else if self.at(TokenKind::RightParenthesis) {
        } else if self.at(TokenKind::Ident) {
            self.report_error(ParseError::MissingComma {
                context: "parameter",
                actual,
            });
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
        assert!(
            self.at(TokenKind::LeftBrace),
            "blocks always begin with `{{`."
        );

        self.push_stmt_context(StmtContext::Block);
        let mark = self.open();

        self.expect(TokenKind::LeftBrace);
        self.consume_trivia();

        // Consume declarations if any
        while !self.at(TokenKind::RightBrace) && !self.is_eof() {
            self.decl();
            self.consume_trivia();
        }
        self.expect(TokenKind::RightBrace);

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtBlock);
    }

    /// Parses a while statement.
    /// ```grammar
    /// whileStmt -> "while" "(" expression ")" statement ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_while(&mut self) {
        assert!(
            self.at(TokenKind::KeywordWhile),
            "whiles always begin with `while`."
        );

        self.push_stmt_context(StmtContext::While);
        let mark = self.open();

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
            self.report_error_with_message("statement");
            self.decl();
        }

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtWhile);
    }

    /// Parses a for statement.
    /// ```grammar
    /// forStmt -> "for" "(" (varDecl | exprStmt)? ";" expression? ";" expression? ")" statement ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_for(&mut self) {
        assert!(
            self.at(TokenKind::KeywordFor),
            "fors always begin with `for`."
        );

        self.push_stmt_context(StmtContext::For);
        let mark = self.open();

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
                self.report_error_with_message("for initializer");
                self.stmt_block();
                self.consume_trivia();
                self.expect(TokenKind::Semicolon);
            }
            token if STMT_FIRST.contains(&token) => {
                self.report_error_with_message("for initializer");
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
            self.report_error_with_message("for condition");
            self.stmt_block();
            self.consume_trivia();
            self.expect(TokenKind::Semicolon);
        } else if self.at_any(DECL_FIRST) {
            self.report_error_with_message("for condition");
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
            self.report_error_with_message("for increment");
            self.decl();
        }
        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        // Body
        if self.at_any(STMT_FIRST) {
            self.stmt();
        } else {
            self.report_error_with_message("for body");
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
        assert!(
            self.at(TokenKind::KeywordVar),
            "var decls always begin with `var`."
        );

        self.push_stmt_context(StmtContext::VarDecl);
        let mark = self.open();

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

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtVarDecl);
    }

    /// Parses a variable declaration initializer.
    /// ```grammar
    /// varDeclInit -> "=" expression ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_var_init(&mut self) {
        assert!(
            self.at(TokenKind::Equal),
            "var initializers always begin with `=`."
        );
        let mark = self.open();

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
        assert!(
            self.at(TokenKind::KeywordReturn),
            "returns always begin with `return`."
        );

        self.push_stmt_context(StmtContext::Return);
        let mark = self.open();

        self.expect(TokenKind::KeywordReturn);
        self.consume_trivia();

        if !self.at(TokenKind::Semicolon) {
            self.expr();
            self.consume_trivia();
        }

        self.expect(TokenKind::Semicolon);

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtReturn);
    }

    /// Parses a print statement e.g.
    /// ```grammar
    /// printStmt -> "print" expression ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_print(&mut self) {
        assert!(
            self.at(TokenKind::KeywordPrint),
            "prints always begin with `print`."
        );

        self.push_stmt_context(StmtContext::Print);
        let mark = self.open();

        self.expect(TokenKind::KeywordPrint);
        self.consume_trivia();

        if self.at_any(EXPR_FIRST) {
            self.expr();
        } else {
            self.report_error_with_message("expression");
        }
        self.consume_trivia();

        self.expect(TokenKind::Semicolon);

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtPrint);
    }

    /// Parses an expression statement e.g.
    /// ```grammar
    /// exprStmt -> expression ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_expr(&mut self) {
        self.push_stmt_context(StmtContext::ExprStmt);
        let mark = self.open();

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::Semicolon);

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtExpr);
    }

    /// Parses an expression e.g.
    /// ```grammar
    /// expression -> assignment ;
    /// ...
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr(&mut self) -> Option<MarkClosed> {
        self.push_stmt_context(StmtContext::Expr);
        let mark = self.expr_recursive(0);
        self.pop_stmt_context();
        mark
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
                self.push_expr_context(ExprContext::Call);
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
                self.push_expr_context(ExprContext::GetSet);
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
            if let Some((tree_kind, l_bp, r_bp)) = assignment_infix_binding_power(right)
                && min_bp < l_bp
            {
                lhs = self.expr_assignment(lhs, tree_kind, r_bp);
                continue;
            }

            // Short circuit infix
            if let Some((tree_kind, l_bp, r_bp)) = short_circuit_infix_binding_power(right)
                && min_bp < l_bp
            {
                self.push_expr_context(ExprContext::AndOr);
                let mark = self.open_before(lhs);

                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);

                self.pop_expr_context();
                lhs = self.close(mark, tree_kind);
                self.consume_trivia();
                continue;
            }

            // Normal infix
            if let Some((tree_kind, l_bp, r_bp)) = infix_binding_power(right)
                && min_bp < l_bp
            {
                self.push_expr_context(ExprContext::Binary);
                let mark = self.open_before(lhs);
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);

                self.pop_expr_context();
                lhs = self.close(mark, tree_kind);
                self.consume_trivia();
                continue;
            }

            break;
        }
        Some(lhs)
    }

    fn expr_assignment(&mut self, lhs: MarkClosed, kind: TreeKind, r_bp: u8) -> MarkClosed {
        self.push_expr_context(ExprContext::Assignment);
        let mark = self.open_before(lhs);

        let op = self.advance();
        self.consume_trivia();
        let rhs = self.expr_recursive(r_bp);

        self.pop_expr_context();
        let new_lhs = self.close(mark, kind);
        self.consume_trivia();

        // Check that the assignment target is valid.
        if !matches!(
            &self.events[lhs.index],
            Event::Open {
                kind: TreeKind::ExprIdent,
                ..
            }
        ) {
            let lvalue = self.get_span_between(lhs, Some(op));
            let value = self.get_span_between(rhs.unwrap(), None);
            self.report_error(ParseError::InvalidAssignment { lvalue, value });
        }
        new_lhs
    }

    /// Parses a delimited expression meaning any literals or groupings.
    /// Returns a mark of where to patch up.
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_delimited(&mut self) -> Option<MarkClosed> {
        macro_rules! make_atom {
            ($context:expr, $tree:expr, $token:expr) => {{
                self.push_expr_context($context);
                let mark = self.open();

                self.expect($token);

                self.pop_expr_context();
                self.close(mark, $tree)
            }};
        }
        let result = match self.peek_kind(0) {
            // This
            TokenKind::KeywordThis => {
                make_atom!(
                    ExprContext::This,
                    TreeKind::ExprThis,
                    TokenKind::KeywordThis
                )
            }
            // Identifiers
            TokenKind::Ident => {
                make_atom!(ExprContext::Ident, TreeKind::ExprIdent, TokenKind::Ident)
            }
            // Literal true
            TokenKind::KeywordTrue => {
                make_atom!(
                    ExprContext::True,
                    TreeKind::ExprTrue,
                    TokenKind::KeywordTrue
                )
            }
            // Literal false
            TokenKind::KeywordFalse => {
                make_atom!(
                    ExprContext::False,
                    TreeKind::ExprFalse,
                    TokenKind::KeywordFalse
                )
            }
            // String literal
            TokenKind::StringLiteral => {
                make_atom!(
                    ExprContext::StringLiteral,
                    TreeKind::ExprStringLiteral,
                    TokenKind::StringLiteral
                )
            }
            // Literal nil
            TokenKind::KeywordNil => {
                make_atom!(ExprContext::Nil, TreeKind::ExprNil, TokenKind::KeywordNil)
            }
            // Numeric literal
            TokenKind::NumericLiteral => {
                make_atom!(
                    ExprContext::NumericLiteral,
                    TreeKind::ExprNumericLiteral,
                    TokenKind::NumericLiteral
                )
            }
            // Super
            TokenKind::KeywordSuper => self.expr_super(),
            // Group
            TokenKind::LeftParenthesis => self.expr_group(),
            op @ (TokenKind::Bang | TokenKind::Minus) => {
                self.push_expr_context(ExprContext::Unary);
                let mark = self.open();

                let (tree_kind, r_bp) =
                    prefix_binding_power(op).expect("! and - are prefix operators");
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);

                self.pop_expr_context();
                self.close(mark, tree_kind)
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

    /// Parses a super method call.
    /// ```grammar
    /// super_method -> "super." IDENTIFIER
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_super(&mut self) -> MarkClosed {
        self.push_expr_context(ExprContext::Super);
        let mark = self.open();

        let super_token = self.peek_token(0);
        self.expect(TokenKind::KeywordSuper);
        self.consume_trivia();

        let mut failed = false;
        if self.at(TokenKind::Dot) {
            self.expect(TokenKind::Dot);
            self.consume_trivia();
        } else {
            let actual = self.peek_token(0);
            self.report_error(ParseError::MissingDotAfterSuper {
                super_token,
                actual,
            });
            failed = true;
        }

        if self.at(TokenKind::Ident) {
            self.expect(TokenKind::Ident);
        } else if !failed {
            let actual = self.peek_token(0);
            self.report_error(ParseError::MissingSuperMethod {
                super_token,
                actual,
            });
        }

        self.pop_expr_context();
        self.close(mark, TreeKind::ExprSuperCall)
    }

    /// Parses a group expression.
    /// ```grammar
    /// group -> "(" expression ")"
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_group(&mut self) -> MarkClosed {
        self.push_expr_context(ExprContext::Group);
        let mark = self.open();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::RightParenthesis);

        self.pop_expr_context();
        self.close(mark, TreeKind::ExprGroup)
    }

    /// Parses an argument list.
    /// ```grammar
    /// arg_list -> "(" arg? ") ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn arg_list(&mut self) {
        assert!(
            self.at(TokenKind::LeftParenthesis),
            "argument lists always begin with `(`."
        );
        let mark = self.open();
        let list_start = self.peek_token(0);
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
        let list_end = self.peek_token(0);
        if arg_count > 255 {
            self.report_error(ParseError::TooManyArguments {
                list_start,
                list_end,
            });
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
        let mark = self.open();

        self.expr();
        let actual = self.peek_token(0);
        self.consume_trivia();

        if self.at(TokenKind::Comma) {
            self.expect(TokenKind::Comma);
        } else if self.at(TokenKind::RightParenthesis) {
        } else if self.at(TokenKind::Ident) {
            self.report_error(ParseError::MissingComma {
                context: "argument",
                actual,
            });
            self.param();
            self.consume_trivia();
        }
        self.close(mark, TreeKind::Arg);
    }

    /// Parses an if statement
    /// ```grammar
    /// ifStmt -> "if" "(" expression ")" statement ( "else" statement )? ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_if(&mut self) {
        assert!(
            self.at(TokenKind::KeywordIf),
            "if statements always begin with `if`."
        );

        self.push_stmt_context(StmtContext::If);
        let mark = self.open();

        self.expect(TokenKind::KeywordIf);
        self.consume_trivia();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        if self.at_any(STMT_FIRST) {
            self.stmt();
            self.consume_trivia();
        } else if self.at_any(STMT_RECOVERY) {
            self.report_error_with_message("statement");
        }
        self.consume_trivia();

        if self.at(TokenKind::KeywordElse) {
            self.expect(TokenKind::KeywordElse);
            self.consume_trivia();

            if self.at_any(STMT_FIRST) {
                self.stmt();
                self.consume_trivia();
            } else if self.at_any(STMT_RECOVERY) {
                self.report_error_with_message("statement");
            }
        }

        self.pop_stmt_context();
        self.close(mark, TreeKind::StmtIf);
    }
}
