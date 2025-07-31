use crate::parser::{BinaryOp, Leaf, LogicalOp, UnaryOp};
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Leaf(Leaf), // literal values
    Grouping(Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    Logical(Box<Expr>, LogicalOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub data: ExprKind,
    pub id: ExprId,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            ExprKind::Leaf(l) => write!(f, "{}", l),
            ExprKind::Grouping(e) => write!(f, "(group {})", *e),
            ExprKind::Unary(op, right) => write!(f, "({} {})", op, *right),
            ExprKind::Binary(left, op, right) => write!(f, "({1} {0} {2})", left, op, right),
            ExprKind::Assignment(l, r) => write!(f, "(= {} {})", l, r), // _ => todo!(),
            ExprKind::Logical(left, op, right) => write!(f, "({1} {0} {2})", left, op, right),
            ExprKind::Call(func, args) => {
                let mut argument_string = String::new();
                let add_args = |e: &Expr| {
                    argument_string.push_str(format!("{},", e).as_str());
                };
                args.iter().for_each(add_args);
                argument_string.pop();

                write!(f, "({}({}))", func, argument_string)
            }
        }
    }
}

pub struct ExprBuilder {
    next_id: usize,
}
impl ExprBuilder {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }
    fn get_id(&mut self) -> ExprId {
        let id = ExprId(self.next_id);
        self.next_id += 1;
        return id;
    }
    pub fn leaf(&mut self, leaf: Leaf) -> Expr {
        Expr {
            data: ExprKind::Leaf(leaf),
            id: self.get_id(),
        }
    }

    pub fn grouping(&mut self, expr: Expr) -> Expr {
        Expr {
            data: ExprKind::Grouping(Box::new(expr)),
            id: self.get_id(),
        }
    }

    pub fn unary(&mut self, op: UnaryOp, right: Expr) -> Expr {
        Expr {
            data: ExprKind::Unary(op, Box::new(right)),
            id: self.get_id(),
        }
    }

    pub fn binary(&mut self, left: Expr, op: BinaryOp, right: Expr) -> Expr {
        Expr {
            data: ExprKind::Binary(Box::new(left), op, Box::new(right)),
            id: self.get_id(),
        }
    }

    pub fn assignment(&mut self, name: Expr, expr: Expr) -> Expr {
        Expr {
            data: ExprKind::Assignment(Box::new(name), Box::new(expr)),
            id: self.get_id(),
        }
    }

    pub fn logical(&mut self, left: Expr, op: LogicalOp, right: Expr) -> Expr {
        Expr {
            data: ExprKind::Logical(Box::new(left), op, Box::new(right)),
            id: self.get_id(),
        }
    }

    pub fn call(&mut self, func: Expr, args: Vec<Expr>) -> Expr {
        Expr {
            data: ExprKind::Call(Box::new(func), args),
            id: self.get_id(),
        }
    }
}
