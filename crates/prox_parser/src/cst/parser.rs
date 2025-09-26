use crate::cst::{
    operator::{
        assignment_infix_binding_power, infix_binding_power, postfix_binding_power,
        prefix_binding_power, short_circuit_infix_binding_power,
    },
    tree::{Node, Tree, TreeKind},
};
use core::cell;
use prox_lexer::{
    Lexer, SourceLookup,
    token::{Token, TokenKind},
};

#[derive(Debug)]
enum Event {
    Open { kind: TreeKind },
    Close,
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

struct Parser<'src> {
    source: SourceLookup<'src>,
    tokens: Vec<Token>,
    current_index: usize,
    fuel: cell::Cell<u32>,
    events: Vec<Event>,
}

impl<'src> Parser<'src> {
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
        }
    }
}

impl Parser<'_> {
    fn open(&mut self) -> MarkOpened {
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

    fn close(&mut self, mark: MarkOpened, kind: TreeKind) -> MarkClosed {
        self.events[mark.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: mark.index }
    }

    fn advance(&mut self) {
        assert!(!self.is_eof(), "can't advance a token at EOF.");
        // Reset fuel
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.current_index += 1;
    }

    fn consume_trivia(&mut self) {
        while self.peek(0).is_trivia() {
            self.advance();
        }
    }

    fn advance_with_error(&mut self, error: &str) {
        let mark = self.open();
        // Add error reporting here
        eprintln!("Error: {error}");
        self.advance();
        self.close(mark, TreeKind::Error);
    }

    fn peek(&self, lookahead: usize) -> TokenKind {
        assert!(
            self.fuel.get() != 0,
            "this only happens if parser is stuck."
        );
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.current_index + lookahead)
            .map_or(TokenKind::Eof, |tok| tok.tag)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.peek(0) == kind
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
        println!("Expecting {kind:?}...");
        if self.eat(kind) {
            return;
        }

        // Error reporting here.
        let token = self.peek(0);
        println!("FAIL: Got {token:?} instead...");
    }

    fn is_eof(&self) -> bool {
        self.current_index >= self.tokens.len() || self.at(TokenKind::Eof)
    }
}

impl Parser<'_> {
    /// Construct the CST.
    fn build_tree(self) -> Tree {
        let mut tokens = self.tokens.into_iter();
        let mut events = self.events;
        let mut stack = Vec::new();

        // Popping last event to ensure that we have a non-empty stack after loop.
        assert!(
            matches!(events.pop(), Some(Event::Close)),
            "the last event should always be a close event."
        );

        for event in events {
            match event {
                // Start a new tree.
                Event::Open { kind } => stack.push(Tree {
                    tag: kind,
                    children: Vec::new(),
                }),
                // Complete a tree and pop it off the stack.
                // Append it to the tree that is now on top of the stack.
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
                        .expect("there should be as many tokens as there advance events.");
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

        stack
            .pop()
            .expect("we just asserted that there was one left in the stack.")
    }
}

// Parsing rules
impl Parser<'_> {
    fn parse(mut self) -> Tree {
        self.program();
        self.build_tree()
    }

    /// Parses a program.
    /// ```grammar
    /// program -> declaration* ;
    /// ```
    /// Leading and trailing trivia should be consumed.
    fn program(&mut self) {
        println!("PROGRAM");
        let mark = self.open();

        // Leading trivia
        self.consume_trivia();

        while !self.is_eof() {
            let lexeme = Lexer::lexeme(&self.source, &self.tokens[self.current_index]).unwrap();
            println!(
                "PROGRAM: AT {:?} ({}) [{}, {}/{}]",
                self.peek(0),
                lexeme,
                self.is_eof(),
                self.current_index,
                self.tokens.len()
            );

            self.decl();
            self.consume_trivia();
        }
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
        println!("DECL");
        match self.peek(0) {
            TokenKind::KeywordVar => self.decl_var(),
            TokenKind::KeywordClass => self.decl_class(),
            TokenKind::KeywordFun => self.decl_fun(),
            _ => self.stmt(),
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
        println!("STMT");
        match self.peek(0) {
            TokenKind::KeywordFor => self.stmt_for(),
            TokenKind::KeywordIf => self.stmt_if(),
            TokenKind::KeywordPrint => self.stmt_print(),
            TokenKind::KeywordReturn => self.stmt_return(),
            TokenKind::KeywordWhile => self.stmt_while(),
            TokenKind::LeftBrace => self.stmt_block(),
            _ => self.stmt_expr(),
        }
    }

    /// Parses a function declaration
    /// ```grammar
    /// funDecl -> "fun" function ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_fun(&mut self) {
        println!("FUNCTION DECL");
        assert!(
            self.at(TokenKind::KeywordFun),
            "function decls always begin with `fun`."
        );
        let mark = self.open();
        self.expect(TokenKind::KeywordFun);
        self.consume_trivia();

        self.function();

        self.close(mark, TreeKind::StmtFnDecl);
    }

    /// Parses a class declaration.
    /// ```grammar
    /// classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_class(&mut self) {
        println!("CLASS DECL");
        assert!(
            self.at(TokenKind::KeywordClass),
            "class decls always begin with `class`."
        );
        let mark = self.open();
        self.expect(TokenKind::KeywordClass);
        self.consume_trivia();

        // Class name
        self.expect(TokenKind::Ident);
        self.consume_trivia();

        if self.at(TokenKind::LessThan) {
            self.expect(TokenKind::LessThan);
            self.consume_trivia();

            // Super class
            self.expect(TokenKind::Ident);
            self.consume_trivia();
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
        self.close(mark, TreeKind::StmtClassDecl);
    }

    /// Parses a function.
    /// ```grammar
    /// function -> IDENTIFIER param_list block ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn function(&mut self) {
        println!("FUNCTION");
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
        }
    }

    /// Parses a parameter list.
    /// ```grammar
    /// param_list -> "(" param? ") ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn param_list(&mut self) {
        println!("PARAM LIST");
        assert!(
            self.at(TokenKind::LeftParenthesis),
            "parameter lists always begin with `(`."
        );
        let mark = self.open();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        // Parameters
        while !self.at(TokenKind::RightParenthesis) && !self.is_eof() {
            // Consume parameter name + trailing trivia
            if self.at(TokenKind::Ident) {
                self.param();
                self.consume_trivia();
            } else {
                break;
            }
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
        println!("PARAM");
        assert!(
            self.at(TokenKind::Ident),
            "parameters always begin with an identifier."
        );
        let mark = self.open();

        self.expect(TokenKind::Ident);
        self.consume_trivia();

        // At closing parenthesis or comma
        if !self.at(TokenKind::RightParenthesis) {
            self.expect(TokenKind::Comma);
        }
        self.close(mark, TreeKind::Param);
    }

    /// Parses a block statement
    /// ```grammar
    /// block -> "{" declaration* "}" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_block(&mut self) {
        println!("BLOCK");
        assert!(
            self.at(TokenKind::LeftBrace),
            "blocks always begin with `{{`."
        );
        let mark = self.open();

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
        println!("WHILE");
        assert!(
            self.at(TokenKind::KeywordWhile),
            "whiles always begin with `while`."
        );
        let mark = self.open();

        self.expect(TokenKind::KeywordWhile);
        self.consume_trivia();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        self.stmt();

        self.close(mark, TreeKind::StmtWhile);
    }

    /// Parses a for statement.
    /// ```grammar
    /// forStmt -> "for" "(" (varDecl | exprStmt | ";") expression? ";" expression? ")" statement ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn stmt_for(&mut self) {
        println!("FOR");
        assert!(
            self.at(TokenKind::KeywordFor),
            "fors always begin with `for`."
        );
        let mark = self.open();

        // Keyword for
        self.expect(TokenKind::KeywordFor);
        self.consume_trivia();

        // Open bracket
        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        // Initializer
        match self.peek(0) {
            TokenKind::KeywordVar => self.decl_var(),
            TokenKind::Semicolon => {}
            _ => self.stmt_expr(),
        }
        self.consume_trivia();

        // Condition
        if !self.at(TokenKind::Semicolon) {
            self.expr();
            self.consume_trivia();
        }
        self.expect(TokenKind::Semicolon);
        self.consume_trivia();

        // Increment
        if !self.at(TokenKind::RightParenthesis) {
            self.expr();
            self.consume_trivia();
        }
        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        // Body
        self.stmt();
        self.close(mark, TreeKind::StmtFor);
    }

    /// Parses a variable declaration.
    /// ```grammar
    /// varDecl -> "var" IDENTIFIER varDeclInit? ";" ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn decl_var(&mut self) {
        println!("VAR DECL");
        assert!(
            self.at(TokenKind::KeywordVar),
            "var decls always begin with `var`."
        );
        let mark = self.open();

        self.expect(TokenKind::KeywordVar);
        self.consume_trivia();

        // Variable name
        self.expect(TokenKind::Ident);
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
        println!("VAR DECL INIT");
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
        println!("RETURN");
        assert!(
            self.at(TokenKind::KeywordReturn),
            "returns always begin with `return`."
        );
        let mark = self.open();

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
        println!("PRINT");
        assert!(
            self.at(TokenKind::KeywordPrint),
            "prints always begin with `print`."
        );
        let mark = self.open();

        self.expect(TokenKind::KeywordPrint);
        self.consume_trivia();

        self.expr();
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
        println!("EXPR STMT");
        let mark = self.open();

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
        println!("EXPR");
        self.expr_recursive(0);
    }

    /// Parses an expression using the Pratt parsing algorithm.
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_recursive(&mut self, min_bp: u8) {
        let mut lhs = self.expr_delimited();
        self.consume_trivia();

        // NOTE(pavyamsiri): Technically we should break if the min_bp is >= the binding power
        // if the operator is valid but as there are no overlaps this works as well.
        loop {
            let right = self.peek(0);

            // Call operator
            if self.at(TokenKind::LeftParenthesis)
                && min_bp
                    < postfix_binding_power(right)
                        .expect("left parenthesis is the postfix call operator.")
            {
                let mark = self.open_before(lhs);
                self.arg_list();
                lhs = self.close(mark, TreeKind::ExprCall);
                self.consume_trivia();
                continue;
            }

            // Field accessors
            if self.at(TokenKind::Dot)
                && min_bp
                    < postfix_binding_power(right).expect("dot is the postfix get/set operator.")
            {
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

                lhs = self.close(mark, TreeKind::ExprSet);
                self.consume_trivia();
                continue;
            }

            // Assignment infix
            if let Some((l_bp, r_bp)) = assignment_infix_binding_power(right)
                && min_bp < l_bp
            {
                let mark = self.open_before(lhs);
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);
                lhs = self.close(mark, TreeKind::ExprInfixAssignment);
                self.consume_trivia();
                continue;
            }

            // Short circuit infix
            if let Some((l_bp, r_bp)) = short_circuit_infix_binding_power(right)
                && min_bp < l_bp
            {
                let mark = self.open_before(lhs);
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);
                lhs = self.close(mark, TreeKind::ExprInfixShortCircuit);
                self.consume_trivia();
                continue;
            }

            // Normal infix
            if let Some((l_bp, r_bp)) = infix_binding_power(right)
                && min_bp < l_bp
            {
                let mark = self.open_before(lhs);
                self.advance();
                self.consume_trivia();

                self.expr_recursive(r_bp);
                lhs = self.close(mark, TreeKind::ExprInfix);
                self.consume_trivia();
                continue;
            }

            break;
        }
    }

    /// Parses a delimited expression meaning any literals or groupings.
    /// Returns a mark of where to patch up.
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn expr_delimited(&mut self) -> MarkClosed {
        let mark = self.open();
        match self.peek(0) {
            // Atoms
            TokenKind::NumericLiteral
            | TokenKind::KeywordTrue
            | TokenKind::KeywordFalse
            | TokenKind::KeywordNil
            | TokenKind::Ident
            | TokenKind::StringLiteral
            | TokenKind::KeywordThis
            | TokenKind::KeywordSuper => {
                self.advance();
                self.close(mark, TreeKind::ExprAtom)
            }
            TokenKind::LeftParenthesis => {
                self.expect(TokenKind::LeftParenthesis);
                self.consume_trivia();
                self.expr();
                self.consume_trivia();
                self.expect(TokenKind::RightParenthesis);
                self.close(mark, TreeKind::ExprGroup)
            }
            op @ (TokenKind::Bang | TokenKind::Minus) => {
                let r_bp = prefix_binding_power(op).expect("! and - are prefix operators");
                self.advance();
                self.consume_trivia();

                let rhs = self.expr_recursive(r_bp);
                self.close(mark, TreeKind::ExprPrefix)
            }
            _ => {
                if !self.is_eof() {
                    self.advance();
                }
                self.close(mark, TreeKind::Error)
            }
        }
    }

    /// Parses an argument list.
    /// ```grammar
    /// arg_list -> "(" arg? ") ;
    /// ```
    /// Assumes leading trivia has been consumed and does not consume trailing trivia.
    fn arg_list(&mut self) {
        println!("ARG LIST");
        assert!(
            self.at(TokenKind::LeftParenthesis),
            "argument lists always begin with `(`."
        );
        let mark = self.open();
        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        while !self.at(TokenKind::RightParenthesis) && !self.is_eof() {
            self.arg();
            self.consume_trivia();
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
        println!("ARG");
        let mark = self.open();

        self.expr();
        self.consume_trivia();

        if !self.at(TokenKind::RightParenthesis) {
            self.expect(TokenKind::Comma);
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
        println!("IF");
        assert!(
            self.at(TokenKind::KeywordIf),
            "if statements always begin with `if`."
        );
        let mark = self.open();

        self.expect(TokenKind::KeywordIf);
        self.consume_trivia();

        self.expect(TokenKind::LeftParenthesis);
        self.consume_trivia();

        self.expr();
        self.consume_trivia();

        self.expect(TokenKind::RightParenthesis);
        self.consume_trivia();

        self.stmt();
        self.consume_trivia();

        if self.at(TokenKind::KeywordElse) {
            self.expect(TokenKind::KeywordElse);
            self.consume_trivia();
            self.stmt();
            self.consume_trivia();
        }

        self.expect(TokenKind::Semicolon);
        self.close(mark, TreeKind::StmtIf);
    }
}

#[cfg(test)]
mod test {
    use prox_lexer::SourceLookup;

    use crate::cst::parser::Parser;

    #[test]
    fn smoke() {
        let text = r#"// Lox interpreter written in ... Lox!
// Scanner: converts Lox source code input into tokens

// One-character tokens (values are the ASCII codes)
var foo = !true or false;
var foo = -2;
var LEFT_PAREN = 40;"#;
        let lookup = SourceLookup::new(text);
        let parser = Parser::new(text);
        let res = parser.parse();
        let mut buffer = String::new();
        res.dump(&lookup, &mut buffer, 0, true)
            .expect("can't handle formatting errors.");
        println!("{buffer}");
    }
}
