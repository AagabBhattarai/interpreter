use std::cell::RefCell;
use std::mem::swap;
use std::rc::Rc;

use crate::error::EvalError;
use crate::expression::{Expr, ExprId, ExprKind};
use crate::parser::{BinaryOp, Declaration, Leaf, LogicalOp, Statement, UnaryOp};
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

#[derive(Debug, Clone)]
pub struct Callable {
    params: Vec<String>,
    block: Statement,
    // #[debug(with = "debug_closure")]
    closure: Pointer,
}

impl Callable {
    fn new(params: Vec<String>, block: Statement, closure: Pointer) -> Self {
        Self {
            params,
            block,
            closure,
        }
    }
    fn call(
        &mut self,
        interpreter: &mut Evaluator,
        arguments: Vec<Referenceable>,
    ) -> Result<Referenceable, EvalError> {
        if arguments.len() != self.params.len() {
            let msg = format!(
                "Expected {} arguments but got {}",
                self.params.len(),
                arguments.len()
            );
            return Err(EvalError::arity_error(msg));
        }
        // let mut global_reference_table = interpreter.global_environment();
        let mut function_scope = Rc::new(RefCell::new(Environment::create_child(Rc::clone(
            &self.closure,
        ))));
        swap(&mut function_scope, &mut interpreter.current_environment);

        for (param, arg) in self.params.iter().zip(arguments.iter()) {
            interpreter.define(&param, arg.clone());
        }

        let result = interpreter.evaluate_statement(&self.block);
        swap(&mut function_scope, &mut interpreter.current_environment);
        match result {
            Ok(_) => return Ok(Referenceable::Value(Value::Nil)),
            Err(EvalError::ReturnValue(value)) => return Ok(value),
            Err(e) => return Err(e),
        }
        // interpreter.evaluate(block)?;
    }
}

#[derive(Debug, Clone)]
pub enum Referenceable {
    Value(Value),
    Statement(Callable, String),
    Native(
        fn(Vec<Referenceable>) -> Result<Referenceable, EvalError>,
        String,
    ),
}

// pub struct Evaluator {
//     pub environment: Vec<ReferenceTable>,
// }
use crate::environment::*;
pub struct Evaluator {
    pub current_environment: Pointer,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut environment = Environment::new();
        environment.register_native_functions();
        Self {
            current_environment: Rc::new(RefCell::new(environment)),
        }
    }

    fn define(&mut self, name: &str, value: Referenceable) {
        self.current_environment.borrow_mut().define(name, value);
    }
    fn update(
        &mut self,
        name: &str,
        value: Referenceable,
        kind: &str,
        line: usize,
    ) -> Result<(), EvalError> {
        let mut current_env = self.current_environment.borrow_mut();
        match current_env.set(name, value, kind) {
            Ok(_) => Ok(()),
            Err(msg) => {
                let msg = format!("[Line {}]: {}", line, msg);
                Err(EvalError::undefined_variable(msg))
            }
        }
    }
    fn lookup(&self, name: &str, kind: &str, line: usize) -> Result<Referenceable, EvalError> {
        match self.current_environment.borrow().lookup(name) {
            Some(v) => Ok(v),
            None => {
                let msg = format!("[Line {}]: Undefined {}", line, kind);
                Err(EvalError::undefined_variable(msg)) //Resolution Error
            }
        }
    }

    fn new_scope(&mut self) {
        let scope = Environment::create_child(Rc::clone(&self.current_environment));
        let scope = Rc::new(RefCell::new(scope));
        self.current_environment = scope
    }
    fn end_scope(&mut self) {
        let parent = match &self.current_environment.borrow().parent_environment {
            Some(parent) => Rc::clone(parent),
            None => return,
        };
        self.current_environment = parent;
    }
    fn global_scope(&self) -> bool {
        let Some(_parent) = &self.current_environment.borrow().parent_environment else {
            return true;
        };
        return false;
    }
    pub fn evaluate(&mut self, declarations: &Vec<Declaration>) -> Result<(), EvalError> {
        for declaration in declarations {
            match declaration {
                Declaration::Var { name, expr, line } => {
                    let value = self.evaluate_expr(&expr, *line)?; // So, I immediately evaluate the expression and assign it to the symbol table
                    self.define(name, value);
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
                            _ => Err(EvalError::not_callable(format!(
                                "{} is not a valid parameter to fun {}",
                                p, name
                            ))),
                        })
                        .collect::<Result<Vec<String>, EvalError>>()?;

                    let current_env = Rc::clone(&self.current_environment);
                    let info = Callable::new(params, block.clone(), current_env);

                    self.define(name, Referenceable::Statement(info, name.to_string()));
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
                match value {
                    Referenceable::Value(v) => println!("{}", v),
                    Referenceable::Statement(_, s) => println!("<fn {}>", s),
                    Referenceable::Native(_, s) => println!("<native fn {}>", s),
                    _ => {
                        println!("{:?}", value);
                        let msg = format!("[Line {}]: Expected value for print statement", line);
                        return Err(EvalError::operand_error(msg));
                    }
                }
            }
            Statement::Block(statements) => {
                self.new_scope();
                self.evaluate(statements)?;
                self.end_scope();
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
            Statement::Return { expr, line } => {
                let result = self.evaluate_expr(expr, *line)?;
                if !self.global_scope() {
                    return Err(EvalError::ReturnValue(result));
                }
                let msg = format!("[Line {}] Can't return from global scope", line);
                return Err(EvalError::invalid_return(msg));
            }
            _ => todo!(),
        }
        Ok(())
    }

    pub fn evaluate_expr(&mut self, expr: &Expr, line: usize) -> Result<Referenceable, EvalError> {
        // println!("{:?}", expr);
        match &expr.data {
            ExprKind::Assignment(identifier, value) => {
                let value = self.evaluate_expr(value.as_ref(), line)?;
                let kind = &"variable";
                self.update(identifier, value, kind, line)?;
                self.lookup(&identifier, kind, line) // print a=10; prints 10 because assignment returns value
            }
            ExprKind::Leaf(Leaf::Identifier(s)) => {
                let kind = &"Identifier";
                self.lookup(s, kind, line)
            }
            ExprKind::Leaf(l) => return Ok(Referenceable::Value(Value::from(l))),
            ExprKind::Grouping(b) => return self.evaluate_expr(b.as_ref(), line),
            ExprKind::Unary(operator, operand) => {
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
            ExprKind::Binary(left, operator, right) => {
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
            ExprKind::Logical(left, op, right) => {
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
            ExprKind::Call(callee, arguments) => {
                let callee = self.evaluate_expr(callee.as_ref(), line)?;

                // redundant? but for specific test case this check is present
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
                    Referenceable::Native(func, _) => return func(evaluated_args),
                    Referenceable::Statement(mut callable, _) => {
                        return callable.call(self, evaluated_args)
                    }
                    Referenceable::Value(v) => {
                        return Err(EvalError::not_callable(format!(
                            "[Line {}] Can't call {} as a function",
                            line, v,
                        )))
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
