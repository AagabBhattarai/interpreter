use crate::error::ParseError;
use crate::expression::{Expr, ExprBuilder, ExprKind};
use crate::scanner::{Literal, Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum Leaf {
    Text(String),
    Number(f64),
    Boolean(bool),
    Identifier(String),
    Nil,
}
impl std::fmt::Display for Leaf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Leaf::Boolean(b) => write!(f, "{b}"),
            Leaf::Nil => write!(f, "nil"),
            Leaf::Text(t) => write!(f, "{t}"),
            Leaf::Number(n) => {
                if n.fract().abs() < f64::EPSILON {
                    write!(f, "{:.1}", n)
                } else {
                    write!(f, "{n}")
                }
            }
            Leaf::Identifier(i) => write!(f, "{i}"),
        }
    }
}

impl From<Literal> for Leaf {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Text(s) => Leaf::Text(s),
            Literal::Number(n) => Leaf::Number(n),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Negation,
}
impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Negation => write!(f, "-"),
        }
    }
}
impl From<TokenType> for UnaryOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::BANG => UnaryOp::Not,
            TokenType::MINUS => UnaryOp::Negation,
            k => panic!("Token {:?} isn't a unary operator", k),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Divide,
    Multiply,
    Add,
    Subtract,
    Equal,
    NotEqual,
    Lesser,
    Greater,
    GreaterEqual,
    LesserEqual,
}
impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::Lesser => write!(f, "<"),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::LesserEqual => write!(f, "<="),
        }
    }
}

impl From<TokenType> for BinaryOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::SLASH => BinaryOp::Divide,
            TokenType::STAR => BinaryOp::Multiply,
            TokenType::PLUS => BinaryOp::Add,
            TokenType::MINUS => BinaryOp::Subtract,
            TokenType::EQUAL_EQUAL => BinaryOp::Equal,
            TokenType::BANG_EQUAL => BinaryOp::NotEqual,
            TokenType::LESS => BinaryOp::Lesser,
            TokenType::GREATER => BinaryOp::Greater,
            TokenType::GREATER_EQUAL => BinaryOp::GreaterEqual,
            TokenType::LESS_EQUAL => BinaryOp::LesserEqual,
            k => panic!("Token {:?} isn't a binary operator", k),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}
impl From<TokenType> for LogicalOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::AND => LogicalOp::And,
            TokenType::OR => LogicalOp::Or,
            k => panic!("Token {:?} isn't a logical operator", k),
        }
    }
}

impl std::fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOp::And => write!(f, "and"),
            LogicalOp::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Var {
        name: String,
        expr: Expr,
        line: usize,
    },
    Statement(Statement),
    Func {
        name: Leaf,            //For classes probably need to change this to other type
        parameters: Vec<Leaf>, // Need to have identifier class instead of packing it in a Leaf??
        block: Statement,
    },
}
#[derive(Debug, Clone)]
pub enum Statement {
    Expression {
        expr: Expr,
        line: usize,
    },
    Print {
        expr: Expr,
        line: usize,
    },
    Block(Vec<Declaration>),
    Conditional {
        expr: Expr,
        then_block: Box<Statement>,
        else_block: Option<Box<Statement>>,
        line: usize,
    },
    While {
        expr: Expr,
        block: Box<Statement>,
        line: usize,
    },
    Return {
        expr: Expr,
        line: usize,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    expr_builder: ExprBuilder,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            expr_builder: ExprBuilder::new(),
        }
    }
    pub fn parse_program(&mut self) -> Result<Vec<Declaration>, ParseError> {
        let mut statements = Vec::new();
        while !self.at_end_of_stream() {
            statements.push(self.declarations()?);
        }
        Ok(statements)
    }
    pub fn declarations(&mut self) -> Result<Declaration, ParseError> {
        let line = self.get_line_number();

        if self.is_expected_token(TokenType::VAR) {
            return self.var_declaration(line);
        }
        if self.is_expected_token(TokenType::FUN) {
            return self.function_declaration(line);
        }

        self.statement().map(Declaration::Statement)
    }

    fn function_declaration(&mut self, line: usize) -> Result<Declaration, ParseError> {
        self.function(line)
    }

    fn function(&mut self, line: usize) -> Result<Declaration, ParseError> {
        let name = self.consume(TokenType::IDENTIFIER, "Expected function identifier")?;
        let name = Leaf::Identifier(name);

        let mut parameters = Vec::new();
        let msg = format!("Expected '(' in the function {} signature", &name);
        self.consume(TokenType::LEFT_PAREN, &msg)?;

        if !self.check(TokenType::RIGHT_PAREN) {
            loop {
                let msg = format!("Expected parameter in function {} signature", &name);
                let param = self.consume(TokenType::IDENTIFIER, &msg)?;
                parameters.push(Leaf::Identifier(param));
                if !self.is_expected_token(TokenType::COMMA) {
                    break;
                }
            }
        }

        let msg = format!("Expected closing ')' in the function {} signature", &name);
        self.consume(TokenType::RIGHT_PAREN, &msg)?;

        let msg = format!("Expected '{{' in the function {} body", &name);
        self.consume(TokenType::LEFT_BRACE, &msg)?;

        let block = self.block_statement(line)?;

        Ok(Declaration::Func {
            name,
            parameters,
            block,
        })
    }

    fn var_declaration(&mut self, line: usize) -> Result<Declaration, ParseError> {
        let name = self.consume(TokenType::IDENTIFIER, "Expected variable name")?;

        let expr = if self.is_expected_token(TokenType::EQUAL) {
            self.expression()?
        } else {
            self.expr_builder.leaf(Leaf::Nil)
        };
        self.check_end_of_statement(line)?;
        Ok(Declaration::Var { name, expr, line })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        let line = self.get_line_number();

        if self.is_expected_token(TokenType::PRINT) {
            return self.print_statement(line);
        }
        if self.is_expected_token(TokenType::LEFT_BRACE) {
            return self.block_statement(line);
        }
        if self.is_expected_token(TokenType::IF) {
            return self.conditional_statement(line);
        }
        if self.is_expected_token(TokenType::WHILE) {
            return self.while_statement(line);
        }
        if self.is_expected_token(TokenType::FOR) {
            return self.for_statement(line);
        }
        if self.is_expected_token(TokenType::RETURN) {
            return self.return_statement(line);
        }

        self.expression_statement(line)
    }

    fn return_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let expr = if !self.check(TokenType::SEMICOLON) {
            self.expression()?
        } else {
            self.expr_builder.leaf(Leaf::Nil)
        };
        self.consume(
            TokenType::SEMICOLON,
            "Expected ';' at the end of the statement",
        )?;
        Ok(Statement::Return { expr, line })
    }
    fn for_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let error_msg = |c: char| format!("Expected '{c}' in the 'for'");
        self.consume(TokenType::LEFT_PAREN, &error_msg('('))?;
        let initializer = if self.is_expected_token(TokenType::SEMICOLON) {
            None
        } else if self.is_expected_token(TokenType::VAR) {
            Some(self.var_declaration(line)?)
        } else {
            Some(
                self.expression_statement(line)
                    .map(Declaration::Statement)?,
            )
        };

        let condition = if self.check(TokenType::SEMICOLON) {
            self.expr_builder.leaf(Leaf::Boolean(true))
        } else {
            self.expression()?
        };
        self.consume(TokenType::SEMICOLON, &error_msg(';'))?;

        let increment = if self.check(TokenType::RIGHT_PAREN) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::RIGHT_PAREN, &error_msg(')'))?;

        let body = self.statement()?;

        let body = if let Some(increment) = increment {
            let increment = Declaration::Statement(Statement::Expression {
                expr: increment,
                line,
            });
            let body = Declaration::Statement(body);
            Statement::Block(vec![body, increment])
        } else {
            body
        };

        let body = Statement::While {
            expr: condition,
            block: Box::new(body),
            line,
        };

        let body = if let Some(initializer) = initializer {
            let body = Declaration::Statement(body);
            Statement::Block(vec![initializer, body])
        } else {
            body
        };

        Ok(body)
    }

    fn while_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let error_msg = |c: char| format!("Expected '{c}' in the 'while'");

        self.consume(TokenType::LEFT_PAREN, &error_msg('('))?;
        let expr = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, &error_msg(')'))?;

        let block = Box::new(self.statement()?);

        Ok(Statement::While { expr, block, line })
    }

    fn conditional_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let error_msg = |c: char| format!("Expected '{c}' in the 'if'");

        self.consume(TokenType::LEFT_PAREN, &error_msg('('))?;
        let expr = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, &error_msg(')'))?;

        let then_block = Box::new(self.statement()?);

        let else_block = if self.is_expected_token(TokenType::ELSE) {
            let block = self.statement()?;
            Some(Box::new(block))
        } else {
            None
        };

        Ok(Statement::Conditional {
            expr,
            then_block,
            else_block,
            line,
        })
    }

    fn block_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let mut statements = Vec::new();
        while !self.check(TokenType::RIGHT_BRACE) && !self.at_end_of_stream() {
            statements.push(self.declarations()?);
        }

        // let line = self.get_line_number(); // idk which line should be reported
        let error_msg = format!("[Line {}]: Expected '}}' for the block", line);
        self.consume(TokenType::RIGHT_BRACE, &error_msg)?;
        Ok(Statement::Block(statements))
    }
    fn print_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.check_end_of_statement(line)?;
        Ok(Statement::Print { expr, line })
    }

    fn expression_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.check_end_of_statement(line)?;
        Ok(Statement::Expression { expr, line })
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;

        if self.is_expected_token(TokenType::EQUAL) {
            let value = self.assignment()?;

            // I can't evaluate it yet, but i have a inkling that placing identifier at leaf will require refactor # FOR NOW LET'S move forward.
            // Need to have assignment for field acess too.
            if let ExprKind::Leaf(Leaf::Identifier(lvalue)) = expr.data {
                return Ok(self.expr_builder.assignment(lvalue, value));
            } else {
                // Again error reporting mechanism isn't standard, that is why i am having a hard time getting the line number value
                let line = self.get_line_number();
                let msg = format!("[Line {}]: Invalid rvalue | Invalid assignment token", line);
                return Err(ParseError::invalid_token(msg));
            }
        }
        return Ok(expr);
    }
    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.logical_and()?;
        while self.is_expected_token(TokenType::OR) {
            let op = LogicalOp::from(self.previous());
            let right = self.logical_and()?;
            left = self.expr_builder.logical(left, op, right);
        }
        Ok(left)
    }
    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.equality()?;

        while self.is_expected_token(TokenType::AND) {
            let op = LogicalOp::from(self.previous());
            let right = self.equality()?;
            left = self.expr_builder.logical(left, op, right);
        }
        Ok(left)
    }
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.is_expected_token(TokenType::BANG_EQUAL)
            || self.is_expected_token(TokenType::EQUAL_EQUAL)
        {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.comparison()?;
            expr = self.expr_builder.binary(expr, operator, right_operand);
        }
        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        while self.is_expected_token(TokenType::GREATER)
            || self.is_expected_token(TokenType::GREATER_EQUAL)
            || self.is_expected_token(TokenType::LESS)
            || self.is_expected_token(TokenType::LESS_EQUAL)
        {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.term()?;
            expr = self.expr_builder.binary(expr, operator, right_operand);
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while self.is_expected_token(TokenType::PLUS) || self.is_expected_token(TokenType::MINUS) {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.factor()?;
            expr = self.expr_builder.binary(expr, operator, right_operand);
        }
        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while self.is_expected_token(TokenType::SLASH) || self.is_expected_token(TokenType::STAR) {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.unary()?;
            expr = self.expr_builder.binary(expr, operator, right_operand);
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.is_expected_token(TokenType::BANG) || self.is_expected_token(TokenType::MINUS) {
            // Ok(Expr::Unary(
            let expr = self.unary()?;
            Ok(self
                .expr_builder
                .unary(UnaryOp::from(self.previous()), expr))
        } else {
            self.call()
        }
    }
    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.is_expected_token(TokenType::LEFT_PAREN) {
                expr = self.process_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn process_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::new();
        if !self.check(TokenType::RIGHT_PAREN) {
            loop {
                args.push(self.expression()?);

                if !self.is_expected_token(TokenType::COMMA) {
                    break;
                }
            }
        }

        self.consume(TokenType::RIGHT_PAREN, "Expected a closing ')' in the call")?;
        Ok(self.expr_builder.call(callee, args))
    }
    //This is such a bad design
    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.is_expected_token(TokenType::TRUE) {
            return Ok(self.expr_builder.leaf(Leaf::Boolean(true)));
        } else if self.is_expected_token(TokenType::FALSE) {
            return Ok(self.expr_builder.leaf(Leaf::Boolean(false)));
        } else if self.is_expected_token(TokenType::NIL) {
            return Ok(self.expr_builder.leaf(Leaf::Nil));
        } else if self.is_expected_token(TokenType::NUMBER)
            || self.is_expected_token(TokenType::STRING)
        {
            let leaf_literal = self.get_token_literal();
            return Ok(self.expr_builder.leaf(Leaf::from(leaf_literal)));
        } else if self.is_expected_token(TokenType::LEFT_PAREN) {
            let expr = self.expression()?;
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression")?;
            return Ok(self.expr_builder.grouping(expr));
        } else if self.is_expected_token(TokenType::IDENTIFIER) {
            return Ok(self.expr_builder.leaf(Leaf::Identifier(self.get_lexeme())));
        } else {
            if self.at_end_of_stream() {
                let error_msg = format!("Error: Reached the end of the file.");
                return Err(ParseError::invalid_token(error_msg));
            }
            let current: Token = self.get_token_info();
            let line = current.line;
            let lexeme = current.lexeme;
            let error_msg = format!("[line {}]: Error Encountered at '{}'", line, lexeme);
            return Err(ParseError::invalid_token(error_msg));
        }
    }

    fn peek(&self) -> TokenType {
        self.tokens[self.current].token_type
    }
    fn previous(&self) -> TokenType {
        self.tokens[self.current - 1].token_type
    }
    fn check(&self, expectation: TokenType) -> bool {
        self.peek() == expectation
    }
    fn at_end_of_stream(&self) -> bool {
        self.peek() == TokenType::EOF
    }

    fn is_expected_token(&mut self, expectation: TokenType) -> bool {
        let reality = self.peek();
        if expectation == reality {
            self.current += 1;
            return true;
        }
        false
    }
    fn get_token_info(&self) -> Token {
        self.tokens[self.current].clone()
    }
    // Token at current - 1 // Refactor to just return token and get info from that; it has become very inefficient
    fn get_token_literal(&mut self) -> Literal {
        let index = self.current - 1;
        self.tokens[index]
            .literal
            .take()
            .expect("Literals exist only for Number and String") //idk but wrapping literal inside option and taking the value is better than cloning ?
    }
    fn get_line_number(&self) -> usize {
        self.tokens[self.current].line
    }
    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }
    fn check_end_of_statement(&mut self, line: usize) -> Result<(), ParseError> {
        if !self.is_expected_token(TokenType::SEMICOLON) {
            let error_msg = format!("[line {}]: Expected ';' at end of the statement", line);
            return Err(ParseError::expected_token(error_msg));
        }
        let _ = self.is_expected_token(TokenType::SEMICOLON);
        Ok(())
    }
    /// Get lexeme for the token that was consumed.
    fn get_lexeme(&self) -> String {
        let index = self.current - 1;
        self.tokens[index].lexeme.clone()
    }

    fn consume(&mut self, required_token: TokenType, msg: &str) -> Result<String, ParseError> {
        let token = self.get_token_info();
        let line = token.line;
        let value = token.lexeme;
        if self.is_expected_token(required_token) {
            Ok(value)
        } else {
            let error_msg = format!("Line [{}]: {} got {}", line, msg, value);
            Err(ParseError::invalid_token(error_msg))
        }
    }
}
