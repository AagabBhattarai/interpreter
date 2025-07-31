use crate::evaluate::Referenceable;

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
pub enum ResolutionError{
    ReDeclaration(String, u8),
}
impl ResolutionError {
    pub fn re_declaration(msg: String) -> ResolutionError {
        let exit_code = 67;
        ResolutionError::ReDeclaration(msg, exit_code) 
    }
}

#[derive(Debug)]
pub enum EvalError {
    OperandError(String, u8),
    UndefinedVariable(String, u8),
    NotCallable(String, u8),
    ArityError(String, u8),
    ReturnValue(Referenceable),
    InvalidReturn(String, u8),
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
    pub fn not_callable(msg: String) -> EvalError {
        let exit_code: u8 = 70;
        EvalError::NotCallable(msg, exit_code)
    }
    pub fn arity_error(msg: String) -> EvalError {
        let exit_code: u8 = 70;
        EvalError::ArityError(msg, exit_code)
    }

    pub fn invalid_return(msg: String) -> EvalError {
        let exit_code: u8 = 70;
        EvalError::UndefinedVariable(msg, exit_code)
    }
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::OperandError(msg, _) => write!(f, "{}", msg),
            EvalError::UndefinedVariable(msg, _) => write!(f, "{}", msg),
            EvalError::NotCallable(msg, _) => write!(f, "{}", msg),
            EvalError::ArityError(msg, _) => write!(f, "{}", msg),
            _ => panic!("Only return value case is not covered"),
        }
    }
}

// pub fn add_line(line: usize) -> String {
//     let msg = format!("[Line: {}]", line);
//     msg
// }
