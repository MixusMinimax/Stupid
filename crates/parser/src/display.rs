use std::fmt::{Display, Formatter};

mod ast {
    pub use crate::syntax::ast::*;
}

impl Display for ast::BinOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ast::BinOperator::*;
        f.write_str(match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Equals => "==",
            NotEquals => "!=",
            Greater => ">",
            GreaterEq => ">=",
            Less => "<",
            LessEq => "<=",
            And => "&&",
            Or => "||",
            BitAnd => "&",
            BitOr => "|",
            BitXor => "^",
        })
    }
}

impl Display for ast::UnOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ast::UnOperator::*;
        f.write_str(match self {
            Negate => "-",
            Not => "!",
            BitNot => "~",
            Deref => "*",
        })
    }
}
