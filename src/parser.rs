use crate::scanner::{Token, TokenType, Literal};

enum Leaf {
    Text(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

pub enum Expr {
    Leaf(Leaf), // literal values
    Grouping(Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Leaf(l) => match l {
                Leaf::Boolean(b) => write!(f, "{b}"),
                Leaf::Nil => write!(f, "nil"),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    pub fn parse(&mut self) -> Expr {
        self.expression()
    }
    fn expression(&mut self) -> Expr {
        self.primary()
    }
    fn primary(&mut self) -> Expr {
        if self.is_expected_token(TokenType::TRUE) {
            return Expr::Leaf(Leaf::Boolean(true));
        } else if self.is_expected_token(TokenType::FALSE) {
            return Expr::Leaf(Leaf::Boolean(false));
        } else if self.is_expected_token(TokenType::NIL) {
            return Expr::Leaf(Leaf::Nil);
        } else if self.is_expected_token(TokenType::NUMBER) {
            todo!()
            // return Expr::Leaf(Leaf::Number(self.previous.))
        }
        else {
            todo!()
        }
    }


    fn peek(&self) -> TokenType {
        self.tokens[self.current].token_type
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
    // Token at current - 1 
    // fn get_token_literal(&self) -> Literal {
    //     let index = self.current - 1;
    //     self.tokens[index].literal.clone() //idk but wrapping literal inside option and taking the value is better than cloning ? 
    // }
}
