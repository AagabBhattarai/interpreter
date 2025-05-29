use crate::error::EvalError;
use crate::native_function::clock_native;
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

// trait Callable {
//     fn call(evaluator: &mut Evaluator, )
// }

#[derive(Clone)]
pub struct Callable {
    params: Vec<String>,
    block: Statement,
    closure: ReferenceTable,
}
impl Callable {
    fn new(params: Vec<String>, block: Statement, closure: ReferenceTable) -> Self {
        Self {
            params,
            block,
            closure,
        }
    }
    fn call(
        &mut self,
        interpreter: &mut Evaluator,
        arguments: Vec<Value>,
    ) -> Result<Referenceable, EvalError> {
        if arguments.len() != self.params.len() {
            let msg = format!(
                "Expected {} arguments but got {}",
                self.params.len(),
                arguments.len()
            );
            return Err(EvalError::arity_error(msg));
        }
        let mut global_reference_table = interpreter.global_environment();
        let mut function_scope = ReferenceTable::new();
        for (param, arg) in self.params.iter().zip(arguments.iter()) {
            let value = Referenceable::Value(arg.clone());
            function_scope.insert(param.clone(), value);
        }
        let block = match &self.block {
            Statement::Block(b) => b,
            _ => panic!("Unreachable state of function call reached"),
        };
        interpreter.evaluate(block)?;
        Ok(Referenceable::Value(Value::Nil))
    }
}

#[derive(Clone)]
pub enum Referenceable {
    Value(Value),
    Statement(Callable),
    Native(fn(Vec<Referenceable>) -> Result<Referenceable, EvalError>),
}

type ReferenceTable = HashMap<String, Referenceable>;

pub struct Evaluator {
    pub environment: Vec<ReferenceTable>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: vec![HashMap::new()],
        }
    }
    pub fn register_native_functions(&mut self) {
        const NATIVE_FUNCTIONS: &[(
            &str,
            fn(Vec<Referenceable>) -> Result<Referenceable, EvalError>,
        )] = &[("clock", clock_native)];
        let global_env = self.global_environment();
        for (name, func) in NATIVE_FUNCTIONS.iter() {
            global_env.insert(name.to_string(), Referenceable::Native(*func));
        }
    }
    pub fn global_environment(&mut self) -> &mut ReferenceTable {
        self.environment.first_mut().unwrap()
    }
    fn current_environment(&mut self) -> &mut ReferenceTable {
        self.environment.last_mut().unwrap()
    }

    fn insert_to_env(&mut self, name: &String, value: Referenceable) {
        self.current_environment().insert(name.to_string(), value);
    }

    fn get_env(&self, name: &str) -> Option<&Referenceable> {
        for env in self.environment.iter().rev() {
            if let Some(value) = env.get(name) {
                return Some(value);
            }
        }
        None
    }
    fn add_env(&mut self, new_env: ReferenceTable) {
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
                Declaration::Func {
                    name,
                    parameters,
                    block,
                } => {
                    let name = match name {
                        Leaf::Identifier(s) => s,
                        _ => {
                            return Err(EvalError::not_callable(format!(
                                "{} is not callable",
                                name
                            )))
                        }
                    };
                    let params = parameters
                        .iter()
                        .map(|p| match p {
                            Leaf::Identifier(s) => Ok(s.clone()),
                            _ => Err(EvalError::not_callable(format!("{} is not callable", p))),
                        })
                        .collect::<Result<Vec<String>, EvalError>>()?;

                    let info =
                        Callable::new(params, block.clone(), self.current_environment().clone());

                    self.insert_to_env(name, Referenceable::Statement(info));
                }
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
                let value = self.evaluate_expr(&expr, *line)?;
                if let Referenceable::Value(v) = value {
                    println!("{}", v);
                } else {
                    let msg = format!("[Line {}]: Expected value for print statement", line);
                    return Err(EvalError::operand_error(msg));
                }
            }
            Statement::Block(statements) => {
                let new_environment: ReferenceTable = HashMap::new();
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
                let value = self.evaluate_expr(&expr, *line)?;
                if let Referenceable::Value(v) = value {
                    if self.is_truthy(&v) {
                        self.evaluate_statement(then_block.as_ref())?;
                    } else if let Some(else_block) = else_block {
                        self.evaluate_statement(else_block.as_ref())?;
                    }
                } else {
                    let msg = format!("[Line {}]: Expected value for conditional", line);
                    return Err(EvalError::operand_error(msg));
                }
            }
            Statement::While { expr, block, line } => {
                while {
                    let result = self.evaluate_expr(expr, *line)?;
                    if let Referenceable::Value(v) = result {
                        self.is_truthy(&v)
                    } else {
                        let msg = format!("[Line {}]: Expected value for while condition", line);
                        return Err(EvalError::operand_error(msg));
                    }
                } {
                    self.evaluate_statement(block.as_ref())?;
                }
            }
            _ => todo!(),
        }
        Ok(())
    }

    pub fn evaluate_expr(&mut self, expr: &Expr, line: usize) -> Result<Referenceable, EvalError> {
        // println!("{:?}", expr);
        match expr {
            Expr::Assignment(identifier, value) => {
                let value = self.evaluate_expr(value.as_ref(), line)?;
                let result = self.assign(identifier, value, line)?;
                match result {
                    Referenceable::Value(v) => Ok(Referenceable::Value(v)),
                    _ => {
                        let msg = format!("[Line {}]: Expected value for assignment", line);
                        return Err(EvalError::operand_error(msg));
                    }
                }
            }
            Expr::Leaf(Leaf::Identifier(s)) => {
                let value = self.get_env(s);
                match value {
                    Some(v) => return Ok(v.clone()),
                    None => {
                        let msg = format!("[Line {}]: Undefined variable {}", line, s);
                        return Err(EvalError::undefined_variable(msg));
                    }
                    _ => {
                        let msg = format!("[Line {}]: Expected value for identifier", line);
                        return Err(EvalError::operand_error(msg));
                    }
                }
            }
            Expr::Leaf(l) => return Ok(Referenceable::Value(Value::from(l))),
            Expr::Grouping(b) => return self.evaluate_expr(b.as_ref(), line),
            Expr::Unary(operator, operand) => {
                let sub_expr_value = self.evaluate_expr(operand.as_ref(), line)?;
                match operator {
                    UnaryOp::Negation => {
                        if let Referenceable::Value(v) = sub_expr_value {
                            return Ok(Referenceable::Value(Value::Num(-v.as_f64()?)));
                        }
                        let msg = format!("[Line {}]: Expected number for negation", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    UnaryOp::Not => {
                        if let Referenceable::Value(v) = sub_expr_value {
                            return Ok(Referenceable::Value(Value::Boolean(!self.is_truthy(&v))));
                        }
                        let msg = format!("[Line {}]: Expected boolean for not operation", line);
                        return Err(EvalError::operand_error(msg));
                    }
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate_expr(left.as_ref(), line)?;
                let right = self.evaluate_expr(right.as_ref(), line)?;

                match operator {
                    BinaryOp::Add => match (left, right) {
                        (
                            Referenceable::Value(Value::String(l)),
                            Referenceable::Value(Value::String(r)),
                        ) => return Ok(Referenceable::Value(Value::String(format!("{}{}", l, r)))),
                        (
                            Referenceable::Value(Value::Num(l)),
                            Referenceable::Value(Value::Num(r)),
                        ) => return Ok(Referenceable::Value(Value::Num(l + r))),
                        _ => {
                            let msg =
                                format!("[Line {}]: Operands must be a number or strings\n", line);
                            return Err(EvalError::operand_error(msg));
                        }
                    },
                    BinaryOp::Multiply => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Num(l.as_f64()? * r.as_f64()?)));
                        }
                        let msg = format!("[Line {}]: Expected numbers for multiplication", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::Divide => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Num(l.as_f64()? / r.as_f64()?)));
                        }
                        let msg = format!("[Line {}]: Expected numbers for division", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::Subtract => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Num(l.as_f64()? - r.as_f64()?)));
                        }
                        let msg = format!("[Line {}]: Expected numbers for subtraction", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::Lesser => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Boolean(
                                l.as_f64()? < r.as_f64()?,
                            )));
                        }
                        let msg = format!("[Line {}]: Expected numbers for comparison", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::Greater => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Boolean(
                                l.as_f64()? > r.as_f64()?,
                            )));
                        }
                        let msg = format!("[Line {}]: Expected numbers for comparison", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::GreaterEqual => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Boolean(
                                l.as_f64()? >= r.as_f64()?,
                            )));
                        }
                        let msg = format!("[Line {}]: Expected numbers for comparison", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::LesserEqual => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Boolean(
                                l.as_f64()? <= r.as_f64()?,
                            )));
                        }
                        let msg = format!("[Line {}]: Expected numbers for comparison", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::Equal => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Boolean(l == r)));
                        }
                        let msg = format!("[Line {}]: Expected values for equality check", line);
                        return Err(EvalError::operand_error(msg));
                    }
                    BinaryOp::NotEqual => {
                        if let (Referenceable::Value(l), Referenceable::Value(r)) = (left, right) {
                            return Ok(Referenceable::Value(Value::Boolean(l != r)));
                        }
                        let msg = format!("[Line {}]: Expected values for inequality check", line);
                        return Err(EvalError::operand_error(msg));
                    }
                }
            }
            Expr::Logical(left, op, right) => {
                let left = self.evaluate_expr(left.as_ref(), line)?;

                match op {
                    LogicalOp::Or => {
                        if let Referenceable::Value(v) = &left {
                            if self.is_truthy(v) {
                                return Ok(left);
                            }
                        }
                    }
                    LogicalOp::And => {
                        if let Referenceable::Value(v) = &left {
                            if !self.is_truthy(v) {
                                return Ok(left);
                            }
                        }
                    }
                }
                self.evaluate_expr(right.as_ref(), line)
            }
            Expr::Call(callee, arguments) => {
                let callee = self.evaluate_expr(callee.as_ref(), line)?;

                let callee = match callee {
                    Referenceable::Value(Value::String(s)) => {
                        let msg = format!("{} is not callable", s);
                        return Err(EvalError::not_callable(msg));
                    }
                    _ => callee,
                };

                let evaluated_args: Result<Vec<_>, _> = arguments
                    .iter()
                    .map(|e| self.evaluate_expr(e, line))
                    .collect();
                let evaluated_args = evaluated_args?;

                match callee {
                    Referenceable::Native(func) => return func(evaluated_args),
                    _ => todo!(),
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
    fn get_env_mut(&mut self, name: &str) -> Option<&mut Referenceable> {
        for env in self.environment.iter_mut().rev() {
            if let Some(value) = env.get_mut(name) {
                return Some(value);
            }
        }
        None
    }
    fn assign(
        &mut self,
        key: &str,
        value: Referenceable,
        line: usize,
    ) -> Result<Referenceable, EvalError> {
        let Some(k) = self.get_env_mut(key) else {
            let msg = format!("[Line {}]: Undefined variable '{}' used", line, key);
            return Err(EvalError::undefined_variable(msg));
        };

        *k = value.clone();
        Ok(value)
    }
}
