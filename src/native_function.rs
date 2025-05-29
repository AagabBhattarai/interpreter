use crate::{
    error::EvalError,
    evaluate::{Referenceable, Value},
};
use std::time::{SystemTime, UNIX_EPOCH};

pub fn clock_native(args: Vec<Referenceable>) -> Result<Referenceable, EvalError> {
    if args.len() != 0 {
        let msg = format!("Expected 0 arguments but got {}", args.len());
        return Err(EvalError::arity_error(msg));
    }
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as f64;

    Ok(Referenceable::Value(Value::Num(now)))
}
