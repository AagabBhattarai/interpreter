use crate::error::EvalError;
use crate::parser::BinaryOp;
use crate::parser::Expr;
use crate::parser::Leaf;
use crate::parser::UnaryOp;

// BOTH Value and leaf enum are same don't repeat things??? Refactor fast but later
#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}
impl Value {
    fn as_f64(&self) -> Result<f64, EvalError> {
        match self {
            Value::Num(n) => Ok(*n),
            _ => {
                let msg = "Cannot cast to a number".to_string();
                Err(EvalError::cast_error(msg))
            }
        }
    }
}

impl From<&Leaf> for Value {
    fn from(leaf: &Leaf) -> Self {
        match leaf {
            Leaf::Text(s) => Value::String(s.clone()),
            Leaf::Number(n) => Value::Num(*n),
            Leaf::Boolean(b) => Value::Boolean(*b),
            Leaf::Nil => Value::Nil,
        }
    }
}

impl From<Value> for Leaf {
    fn from(value: Value) -> Self {
        match value {
            Value::Num(n) => Leaf::Number(n),
            Value::String(s) => Leaf::Text(s),
            Value::Boolean(b) => Leaf::Boolean(b),
            Value::Nil => Leaf::Nil,
        }
    }
}

// impl std::fmt::Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", Leaf::from(self.clone()))
//     }
// }

//I am not sure but Leaf Display of numbers and Value display are different
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
            Value::String(t) => write!(f, "{t}"),
            Value::Num(n) => {
                write!(f, "{n}")
            }
        }
    }
}
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate(&self, ast: &Expr) -> Result<Value, EvalError> {
        // println!("{:?}", ast);
        match ast {
            Expr::Leaf(l) => return Ok(Value::from(l)),
            Expr::Grouping(b) => return self.evaluate(b.as_ref()),
            Expr::Unary(operator, operand) => {
                let sub_expr_value = self.evaluate(operand.as_ref())?;
                match operator {
                    UnaryOp::Negation => return Ok(Value::Num(-sub_expr_value.as_f64().unwrap())),
                    UnaryOp::Not => return Ok(Value::Boolean(!self.is_truthy(&sub_expr_value))),
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate(left.as_ref())?;
                let right = self.evaluate(right.as_ref())?;

                match operator {
                    BinaryOp::Add => match (left, right) {
                        (Value::String(l), Value::String(r)) => {
                            return Ok(Value::String(format!("{}{}", l, r)))
                        }
                        (Value::Num(l), Value::Num(r)) => return Ok(Value::Num(l + r)),
                        _ => panic!("Invalid operands for addition"),
                    },
                    BinaryOp::Multiply => return Ok(Value::Num(left.as_f64()? * right.as_f64()?)),
                    BinaryOp::Divide => return Ok(Value::Num(left.as_f64()? / right.as_f64()?)),
                    BinaryOp::Subtract => return Ok(Value::Num(left.as_f64()? - right.as_f64()?)),
                    BinaryOp::Lesser => {
                        return Ok(Value::Boolean(left.as_f64()? < right.as_f64()?))
                    }
                    BinaryOp::Greater => {
                        return Ok(Value::Boolean(left.as_f64()? > right.as_f64()?))
                    }
                    BinaryOp::GreaterEqual => {
                        return Ok(Value::Boolean(left.as_f64()? >= right.as_f64()?))
                    }
                    BinaryOp::LesserEqual => {
                        return Ok(Value::Boolean(left.as_f64()? <= right.as_f64()?))
                    }
                    BinaryOp::Equal => {
                        return Ok(Value::Boolean(left == right));
                    }
                    BinaryOp::NotEqual => {
                        return Ok(Value::Boolean(left != right));
                    }
                }
            }
        }
    }
    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }
}
