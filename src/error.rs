pub enum Errors {
    LexicalError(String, u8),
}

pub enum ParseError {
    InvalidToken(String, u8),
}
impl ParseError {
    pub fn invalid_token(msg: String) -> ParseError {
        let exit_code = 65;
        ParseError::InvalidToken(msg, exit_code)
    }
}

#[derive(Debug)]
pub enum EvalError {
    CastError(String, u8),
}
impl EvalError {
    pub fn cast_error(msg: String) -> EvalError {
        let exit_code: u8 = 70;
        EvalError::CastError(msg, exit_code)
    }
}
