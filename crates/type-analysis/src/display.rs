use std::fmt::{Display, Write};
use util::MyInto;

mod analyzed {
    pub use crate::program::analyzed::*;
}

impl Display for analyzed::Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for constant in self.constants.values() {
            if !first {
                f.write_str("\n")?;
            }
            first = false;
            let c = &*(**constant).borrow();
            let (name, type_, value) = match c {
                analyzed::Declaration::Typed { .. } => return Err(std::fmt::Error),
                analyzed::Declaration::UnTyped { name, type_, value } => {
                    (name, display_type(MyInto::into(type_)), value)
                }
            };
            f.write_fmt(format_args!("const {}: {} = {};", name, type_, &**value))?;
        }

        for procedure in self.procedures.values() {
            if !first {
                f.write_str("\n\n")?;
            }
            first = false;
            let p = &*(**procedure).borrow();
            write!(f, "proc {}(", p.name)?;
            let mut first = true;
            for arg in p.arguments.values() {
                if !first {
                    f.write_str(", ")?;
                }
                first = false;
                let a = &*(**arg).borrow();
                let (name, t, value) = a.get();
                let type_ = display_type(t);
                match value {
                    Some(v) => write!(f, "{}: {} := {}", name, type_, &**v),
                    None => write!(f, "{}: {}", name, type_),
                }?;
            }
            write!(
                f,
                ") -> {} {}",
                display_type(MyInto::into(&p.return_type)),
                *p.return_value
            )?;
        }
        Ok(())
    }
}

impl Display for analyzed::Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use analyzed::ExpressionValue::*;
        let indent = f.width().unwrap_or(0);
        match &self.value {
            Integer(value) => write!(f, "{}", value),
            Long(value) => write!(f, "{}l", value),
            Float(value) => write!(f, "{}f", value),
            Double(value) => write!(f, "{}d", value),
            Boolean(value) => write!(f, "{}", value),
            Variable(var) => f.write_str((*(**var).borrow()).get_name()),
            BinOp(left, op, right) => {
                write!(
                    f,
                    "({:>indent$} {} {:>indent$})",
                    **left,
                    op,
                    **right,
                    indent = indent
                )
            }
            UnOp(op, right) => {
                write!(f, "({}{:>indent$})", op, **right, indent = indent)
            }
            Block { statements, last } => {
                let padding = format!("{:>indent$}", "", indent = indent);
                f.write_str("{")?;
                for statement in statements.iter() {
                    write!(
                        f,
                        "\n{}    {:>indent$}",
                        padding,
                        statement,
                        indent = indent + 4
                    )?;
                }
                if let Some(l) = last {
                    write!(f, "\n{}    {:>indent$}", padding, l, indent = indent + 4)?;
                }
                if statements.len() > 0 || last.is_some() {
                    write!(f, "\n{}", padding)?
                };
                f.write_char('}')?;
                Ok(())
            }
            FunctionCall {
                procedure,
                arguments,
            } => {
                let name = &(**procedure).borrow().name;
                write!(f, "{}(", name)?;
                let mut is_first = true;
                for arg in arguments.iter() {
                    if !is_first {
                        f.write_str(", ")?;
                    }
                    is_first = false;
                    write!(f, "{:>indent$}", arg, indent = indent + 4)?;
                }
                f.write_char(')')?;
                Ok(())
            }
            Assignment(variable, value) => {
                write!(
                    f,
                    "({} = {:>indent$})",
                    (**variable).borrow().get_name(),
                    &**value,
                    indent = indent
                )
            }
            IfElse {
                condition,
                then,
                else_,
            } => {
                write!(
                    f,
                    "(if {:>indent$} {:>indent$} else {:>indent$})",
                    &**condition,
                    &**then,
                    &**else_,
                    indent = indent
                )
            }
        }?;
        Ok(())
    }
}

impl Display for analyzed::Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use analyzed::Statement::*;
        let indent = f.width().unwrap_or(0);
        match self {
            ExpressionStatement(expr) => write!(f, "{:>indent$};", expr, indent = indent),
            VariableDeclaration(decl) => {
                let d = &*(**decl).borrow();
                let (name, type_, value) = d.get();
                match value {
                    Some(v) => write!(
                        f,
                        "let {}: {} = {:>indent$};",
                        name,
                        display_type(type_),
                        &**v,
                        indent = indent
                    ),
                    None => write!(f, "let {}: {};", name, display_type(type_)),
                }
            }
            If { condition, then } => {
                write!(
                    f,
                    "if {:>indent$} {:>indent$};",
                    condition,
                    then,
                    indent = indent
                )
            }
        }
    }
}

fn display_type<'a>(type_: Option<&'a String>) -> &'a str {
    match type_ {
        Some(name) => name.as_str(),
        None => "unknown",
    }
}
