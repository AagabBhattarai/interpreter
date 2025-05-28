use crate::error::EvalError;
use crate::parser::BinaryOp;
use crate::parser::Declaration;
use crate::parser::Expr;
use crate::parser::Leaf;
use crate::parser::LogicalOp;
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
            Leaf::Identifier(_) => {
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
    environment: Vec<HashMap<String, Value>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: vec![HashMap::new()],
        }
    }

    fn current_environment(&mut self) -> &mut HashMap<String, Value> {
        self.environment.last_mut().unwrap()
    }

    fn insert_to_env(&mut self, name: &String, value: Value) {
        self.current_environment().insert(name.to_string(), value);
    }

    fn get_env(&self, name: &str) -> Option<&Value> {
        for env in self.environment.iter().rev() {
            if let Some(value) = env.get(name) {
                return Some(value);
            }
        }
        None
    }
    fn add_env(&mut self, new_env: HashMap<String, Value>) {
        self.environment.push(new_env);
    }
    fn remove_env(&mut self) {
        self.environment.pop();
    }
    pub fn evaluate(&mut self, declarations: &Vec<Declaration>) -> Result<(), EvalError> {
        for declaration in declarations {
            match declaration {
                Declaration::Var { name, expr, line } => {
                    let value = self.evaluate_expr(&expr, *line)?; // So, I immediately evaluate the expression and assign it to the symbol table
                    self.insert_to_env(name, value);
                }
                Declaration::Statement(statement) => self.evaluate_statement(statement)?,
            }
        }
        Ok(())
    }
    fn evaluate_statement(&mut self, statement: &Statement) -> Result<(), EvalError> {
        match statement {
            Statement::Expression { expr, line } => {
                self.evaluate_expr(&expr, *line)?;
            }
            Statement::Print { expr, line } => {
                let value: Value = self.evaluate_expr(&expr, *line)?;
                println!("{}", value);
            }
            Statement::Block(statements) => {
                let new_environment: HashMap<String, Value> = HashMap::new();
                self.add_env(new_environment);
                self.evaluate(statements)?;
                self.remove_env();
            }
            Statement::Conditional {
                expr,
                then_block,
                else_block,
                line,
            } => {
                let value = &self.evaluate_expr(&expr, *line)?;
                if self.is_truthy(value) {
                    self.evaluate_statement(then_block.as_ref())?;
                } else if let Some(else_block) = else_block {
                    self.evaluate_statement(else_block.as_ref())?;
                }
            }
            Statement::While { expr, block, line } => {
                while {
                    let result = self.evaluate_expr(expr, *line)?;
                    self.is_truthy(&result)
                } {
                    self.evaluate_statement(block.as_ref())?;
                }
            }
        }
        Ok(())
    }

    pub fn evaluate_expr(&mut self, expr: &Expr, line: usize) -> Result<Value, EvalError> {
        // println!("{:?}", expr);
        match expr {
            Expr::Assignment(identifier, value) => {
                let value = self.evaluate_expr(value.as_ref(), line)?;
                self.assign(identifier, value, line)
            }
            Expr::Leaf(Leaf::Identifier(s)) => {
                let value = self.get_env(s);
                match value {
                    Some(v) => return Ok(v.clone()),
                    None => {
                        let msg = format!("[Line {}]: Undefined variable {}", line, s);
                        return Err(EvalError::undefined_variable(msg));
                    }
                }
            }
            Expr::Leaf(l) => return Ok(Value::from(l)),
            Expr::Grouping(b) => return self.evaluate_expr(b.as_ref(), line),
            Expr::Unary(operator, operand) => {
                let sub_expr_value = self.evaluate_expr(operand.as_ref(), line)?;
                match operator {
                    UnaryOp::Negation => return Ok(Value::Num(-sub_expr_value.as_f64()?)),
                    UnaryOp::Not => return Ok(Value::Boolean(!self.is_truthy(&sub_expr_value))),
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate_expr(left.as_ref(), line)?;
                let right = self.evaluate_expr(right.as_ref(), line)?;

                match operator {
                    BinaryOp::Add => match (left, right) {
                        (Value::String(l), Value::String(r)) => {
                            return Ok(Value::String(format!("{}{}", l, r)))
                        }
                        (Value::Num(l), Value::Num(r)) => return Ok(Value::Num(l + r)),
                        _ => {
                            let msg =
                                format!("[Line {}]: Operands must be a number or strings\n", line);
                            return Err(EvalError::operand_error(msg));
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
            Expr::Logical(left, op, right) => {
                let left = self.evaluate_expr(left.as_ref(), line)?;

                match op {
                    LogicalOp::Or => {
                        if self.is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                    LogicalOp::And => {
                        if !self.is_truthy(&left) {
                            return Ok(left);
                        }
                    }
                }
                self.evaluate_expr(right.as_ref(), line)
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
    fn get_env_mut(&mut self, name: &str) -> Option<&mut Value> {
        for env in self.environment.iter_mut().rev() {
            if let Some(value) = env.get_mut(name) {
                return Some(value);
            }
        }
        None
    }
    fn assign(&mut self, key: &str, value: Value, line: usize) -> Result<Value, EvalError> {
        let Some(k) = self.get_env_mut(key) else {
            let msg = format!("[Line {}]: Undefined variable '{}' used", line, key);
            return Err(EvalError::undefined_variable(msg));
        };

        *k = value.clone();
        Ok(value)
    }
}
