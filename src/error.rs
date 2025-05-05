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
