use crate::error::ParseError;
use crate::scanner::{Literal, Token, TokenType};

#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr {
    Leaf(Leaf), // literal values
    Grouping(Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Leaf(l) => write!(f, "{}", l),
            Expr::Grouping(e) => write!(f, "(group {})", *e),
            Expr::Unary(op, right) => write!(f, "({} {})", op, *right),
            Expr::Binary(left, op, right) => write!(f, "({1} {0} {2})", left, op, right),
            // _ => todo!(),
        }
    }
}

pub enum Statement {
    Expression {
        expr: Expr,
        line: usize,
    },
    Print {
        expr: Expr,
        line: usize,
    },
    Var {
        name: String,
        expr: Expr,
        line: usize,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub fn parse_program(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();
        while !self.at_end_of_stream() {
            statements.push(self.declarations()?);
        }
        Ok(statements)
    }
    pub fn declarations(&mut self) -> Result<Statement, ParseError> {
        let line = self.get_line_number();
        if self.is_expected_token(TokenType::VAR) {
            return Ok(self.var_statement(line)?);
        }

        Ok(self.statement(line)?)
    }
    fn var_statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        let name = self.consume(TokenType::IDENTIFIER, "Expected variable name")?;

        let expr = if self.is_expected_token(TokenType::EQUAL) {
            self.expression()?
        } else {
            Expr::Leaf(Leaf::Nil)
        };
        self.check_end_of_statement(line)?;
        return Ok(Statement::Var { name, expr, line });
    }

    fn statement(&mut self, line: usize) -> Result<Statement, ParseError> {
        if self.is_expected_token(TokenType::PRINT) {
            return Ok(self.print_statement(line)?);
        }
        Ok(self.expression_statement(line)?)
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
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.is_expected_token(TokenType::BANG_EQUAL)
            || self.is_expected_token(TokenType::EQUAL_EQUAL)
        {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right_operand));
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
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right_operand));
        }
        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while self.is_expected_token(TokenType::PLUS) || self.is_expected_token(TokenType::MINUS) {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right_operand));
        }
        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while self.is_expected_token(TokenType::SLASH) || self.is_expected_token(TokenType::STAR) {
            let operator = BinaryOp::from(self.previous());
            let right_operand = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right_operand));
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.is_expected_token(TokenType::BANG) || self.is_expected_token(TokenType::MINUS) {
            Ok(Expr::Unary(
                UnaryOp::from(self.previous()),
                Box::new(self.unary()?),
            ))
        } else {
            self.primary()
        }
    }
    //This is such a bad design
    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.is_expected_token(TokenType::TRUE) {
            return Ok(Expr::Leaf(Leaf::Boolean(true)));
        } else if self.is_expected_token(TokenType::FALSE) {
            return Ok(Expr::Leaf(Leaf::Boolean(false)));
        } else if self.is_expected_token(TokenType::NIL) {
            return Ok(Expr::Leaf(Leaf::Nil));
        } else if self.is_expected_token(TokenType::NUMBER)
            || self.is_expected_token(TokenType::STRING)
        {
            let leaf_literal = self.get_token_literal();
            return Ok(Expr::Leaf(Leaf::from(leaf_literal)));
        } else if self.is_expected_token(TokenType::LEFT_PAREN) {
            let expr = Box::new(self.expression()?);
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression")?;
            return Ok(Expr::Grouping(expr));
        } else if self.is_expected_token(TokenType::IDENTIFIER) {
            return Ok(Expr::Leaf(Leaf::Identifier(self.get_lexeme())));
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
    // Token at current - 1
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
            let error_msg = format!("Line [{}]: {}", line, msg);
            Err(ParseError::invalid_token(error_msg))
        }
    }
}
