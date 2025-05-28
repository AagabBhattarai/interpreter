pub enum Errors {
    LexicalError(String, u8),
}

pub enum ParseError {
    InvalidToken(String, u8),
    ExpectedToken(String, u8),
}
impl ParseError {
    pub fn invalid_token(msg: String) -> ParseError {
        let exit_code = 65;
        ParseError::InvalidToken(msg, exit_code)
    }
    pub fn expected_token(msg: String) -> ParseError {
        let exit_code = 65;
        ParseError::ExpectedToken(msg, exit_code)
    }
}

#[derive(Debug)]
pub enum EvalError {
    OperandError(String, u8),
    UndefinedVariable(String, u8),
}
impl EvalError {
    pub fn operand_error(msg: String) -> EvalError {
        let exit_code: u8 = 70;
        EvalError::OperandError(msg, exit_code)
    }
    pub fn undefined_variable(msg: String) -> EvalError {
        let exit_code: u8 = 70;
        EvalError::UndefinedVariable(msg, exit_code)
    }
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::OperandError(msg, _) => write!(f, "{}", msg),
            EvalError::UndefinedVariable(msg, _) => write!(f, "{}", msg),
        }
    }
}

// pub fn add_line(line: usize) -> String {
//     let msg = format!("[Line: {}]", line);
//     msg
// }
