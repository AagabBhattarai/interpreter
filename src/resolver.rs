use crate::{
    error::ResolutionError,
    expression::Expr,
    parser::{Declaration, Leaf, Statement},
};
use std::collections::HashMap;

struct Scopes {
    stack: Vec<HashMap<String, bool>>,
}
impl Scopes {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }
    fn new_scope(&mut self) {
        self.stack.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.stack.pop();
    }
    fn declare(&mut self, value: &str) {
        if let Some(scope) = self.stack.last_mut() {
            scope.insert(value.to_string(), false);
        }
    }
    fn define(&mut self, value: &str) {
        if let Some(scope) = self.stack.last_mut() {
            scope.insert(value.to_string(), true);
        }
    }
    //check if identifier itself is uninitialized
    fn is_ready(&self, key: &str) -> bool {
        self.stack
            .last()
            .and_then(|scope| scope.get(key).copied())
            .unwrap_or(true)
    }

    fn lookup(&self, key: &str) -> Option<usize> {
        /*         let mut depth = None;
        for (i, scope) in self.stack.iter().enumerate().rev() {
            if scope.contains_key(key) {
                depth = Some(self.stack.len() - 1 - i);
            }
        }
        return depth */
        self.stack.iter().enumerate().rev().find_map(|(i, scope)| {
            if scope.contains_key(key) {
                Some(self.stack.len() - 1 - i)
            } else {
                None
            }
        })
    }
}
pub struct Resolver {
    scopes: Scopes,
    depth: HashMap<Expr, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            depth: HashMap::new(),
        }
    }

    pub fn resolve_declarations(
        &mut self,
        declarations: &Vec<Declaration>,
    ) -> Result<(), ResolutionError> {
        for declaration in declarations {
            match declaration {
                Declaration::Var { name, expr, line } => self.resolve_var(name, expr, *line)?,
                Declaration::Statement(statement) => self.resolve_statement(statement)?,
                Declaration::Func {
                    name,
                    parameters,
                    block,
                } => self.resolve_func(name, parameters, block)?,
            }
        }

        Ok(())
    }

    fn resolve_var(&mut self, name: &str, expr: &Expr, line: usize) -> Result<(), ResolutionError> {
        self.scopes.declare(name);
        self.resolve_expression(expr, line)?;
        self.scopes.define(name);
        Ok(())
    }
    fn resolve_expression(&mut self, expr: &Expr, line: usize) -> Result<(), ResolutionError> {
        match expr {
            // Expr::Leaf(Leaf::Identifier(l)) => {
            //     // let expr = self.resolve_expression(expr, line);
            //     if !self.scopes.is_ready(l) {
            //         let msg =
            //             format!("[Line {line}] Can't use identifier {l} in its own initializer");
            //         return Err(ResolutionError::re_declaration(msg));
            //     }
            //     if let Some(depth) = self.scopes.lookup(l) {
            //         // self.depth.insert()
            //     }
            // }
            // Expr::Binary(left, _, right) | Expr::Logical(left, _, right) => {
            //     self.resolve_expression(left, line)?;
            //     self.resolve_expression(right, line)?;
            // }

            // Expr::Leaf(l) => {}
            _ => todo!(),
        }
        Ok(())
    }
    fn resolve_statement(&mut self, statement: &Statement) -> Result<(), ResolutionError> {
        match statement {
            Statement::Block(declaration) => {
                self.scopes.new_scope();
                self.resolve_declarations(declaration)?;
                self.scopes.end_scope();
            }
            _ => todo!(),
        }
        todo!()
    }

    fn resolve_func(
        &self,
        name: &Leaf,
        parameters: &[Leaf],
        block: &Statement,
    ) -> Result<(), ResolutionError> {
        todo!()
    }
}
