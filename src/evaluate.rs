use crate::error::add_line;
use crate::error::EvalError;
use crate::parser::BinaryOp;
use crate::parser::Expr;
use crate::parser::Leaf;
use crate::parser::Statement;
use crate::parser::UnaryOp;
use std::collections::HashMap;

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
                let msg = "Operands must be a number\n".to_string();
                Err(EvalError::operand_error(msg))
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
            Leaf::Identifier(s) => {
                panic!("Identifier condition should be dealth prior to this")
            }
        }
    }
}

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
pub struct Evaluator {
    symbol_table: HashMap<String, Value>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
        }
    }

    pub fn evaluate(&mut self, statements: Vec<Statement>) -> Result<(), EvalError> {
        for statement in statements {
            match statement {
                Statement::Expression { expr, line } => {
                    self.evaluate_expr(&expr)?;
                }
                Statement::Print { expr, line } => {
                    let value: Value = self.evaluate_expr(&expr)?;
                    println!("{}", value);
                }
                Statement::Var { name, expr, line } => {
                    let value = self.evaluate_expr(&expr).map_err(|e| {
                        EvalError::undefined_variable(format!("{} {}", add_line(line), e))
                    })?; // So, I immediately evaluate the expression and assign it to the symbol table
                    self.symbol_table.insert(name, value);
                }
                _ => todo!(),
            }
        }
        Ok(())
    }
    pub fn evaluate_expr(&self, expr: &Expr) -> Result<Value, EvalError> {
        // println!("{:?}", expr);
        match expr {
            Expr::Leaf(Leaf::Identifier(s)) => {
                let value = self.symbol_table.get(s);
                match value {
                    Some(v) => return Ok(v.clone()),
                    None => {
                        let msg = format!("Undefined variable {}", s);
                        return Err(EvalError::undefined_variable(msg));
                    }
                }
            }
            Expr::Leaf(l) => return Ok(Value::from(l)),
            Expr::Grouping(b) => return self.evaluate_expr(b.as_ref()),
            Expr::Unary(operator, operand) => {
                let sub_expr_value = self.evaluate_expr(operand.as_ref())?;
                match operator {
                    UnaryOp::Negation => return Ok(Value::Num(-sub_expr_value.as_f64()?)),
                    UnaryOp::Not => return Ok(Value::Boolean(!self.is_truthy(&sub_expr_value))),
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate_expr(left.as_ref())?;
                let right = self.evaluate_expr(right.as_ref())?;

                match operator {
                    BinaryOp::Add => match (left, right) {
                        (Value::String(l), Value::String(r)) => {
                            return Ok(Value::String(format!("{}{}", l, r)))
                        }
                        (Value::Num(l), Value::Num(r)) => return Ok(Value::Num(l + r)),
                        _ => {
                            return Err(EvalError::operand_error(
                                "Operands must be a number\n".to_string(),
                            ))
                        }
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
