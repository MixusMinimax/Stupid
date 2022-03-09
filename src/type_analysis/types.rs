use std::{cell::RefCell, fmt::Display, rc::Rc};

mod analyzed {
    pub use crate::type_analysis::program::analyzed::*;
}

pub struct TypeAnalysisError {
    message: String,
}

pub fn analyze_program(program: &mut analyzed::Program) -> Result<(), TypeAnalysisError> {
    let mut analyzed_count: i32;
    let mut failed_count: i32;

    // Firstly, analyze types of procedures and constants,
    // which means that expressions not required for
    // the return type for functions are ignored.
    loop {
        analyzed_count = 0;
        failed_count = 0;

        for (_, constant) in &program.constants {
            if let analyzed::Declaration::UnTyped { type_: Some(_), .. } = &*(*constant).borrow() {
                continue;
            }
            match analyze_decl(constant.clone(), false) {
                Ok(_) => analyzed_count = analyzed_count + 1,
                Err(_) => failed_count = failed_count + 1,
            };
        }

        for (_, procedure) in &program.procedures {
            if let Some(_) = (*procedure).borrow().return_type {
                continue;
            }
            match analyze_proc(procedure.clone(), false) {
                Ok(_) => analyzed_count = analyzed_count + 1,
                Err(_) => failed_count = failed_count + 1,
            };
        }

        if analyzed_count == 0 && failed_count == 0 {
            break Ok(());
        }
        if analyzed_count == 0 {
            break Err(TypeAnalysisError::new("Some types could not be deduced"));
        }
    }?;
    // Now, we can fully analyze constants and procedures, including irrelevant variables.
    for (_, constant) in &program.constants {
        analyze_decl(constant.clone(), true)
            .map_err(|()| TypeAnalysisError::new("Could not fully analyze constant"))?;
    }

    for (_, procedure) in &program.procedures {
        analyze_proc(procedure.clone(), true)
            .map_err(|()| TypeAnalysisError::new("Could not fully analyze procedure"))?;
    }
    Ok(())
}

fn analyze_decl(
    declaration: Rc<RefCell<analyzed::Declaration>>,
    analyze_all: bool,
) -> Result<String, ()> {
    match &mut *(*declaration).borrow_mut() {
        analyzed::Declaration::UnTyped { type_, value, .. } => match type_ {
            Some(name) if !analyze_all => Ok(name.clone()),
            _ => {
                let t = analyze_expr(&mut **value, analyze_all)?;
                *type_ = Some(t.clone());
                Ok(t)
            }
        },
        analyzed::Declaration::Typed { type_, value, .. } => {
            if analyze_all {
                if let Some(v) = value {
                    analyze_expr(&mut **v, analyze_all)?;
                }
            }
            Ok(type_.clone())
        }
    }
}

fn analyze_proc(procedure: Rc<RefCell<analyzed::Procedure>>, analyze_all: bool) -> Result<(), ()> {
    if analyze_all {
        // Extract argument rcs so that proc is not borrowed while analyzing arguments.
        // In theory, a function can be used as a default argument for an argument of itself.
        let arguments: Vec<_> = (*procedure)
            .borrow()
            .arguments
            .values()
            .map(|rc| rc.clone())
            .collect();
        for argument in arguments {
            analyze_decl(argument, analyze_all)?;
        }
    }
    let return_type = Some(analyze_expr(
        &mut *(*procedure).borrow_mut().return_value,
        analyze_all,
    )?);
    (*procedure).borrow_mut().return_type = return_type;
    Ok(())
}

fn analyze_expr(expression: &mut analyzed::Expression, analyze_all: bool) -> Result<String, ()> {
    use analyzed::ExpressionValue::*;

    if !analyze_all {
        if let Some(ref name) = expression.type_ {
            return Ok(name.clone());
        }
    }
    let type_ = match &mut expression.value {
        Integer(_) | Long(_) | Float(_) | Double(_) => return expression.type_.clone().ok_or(()),
        BinOp(left, _, right) => common_type(
            analyze_expr(&mut *left, analyze_all)?,
            analyze_expr(&mut *right, analyze_all)?,
        ),
        UnOp(_, expr) => Some(analyze_expr(&mut *expr, analyze_all)?),
        Variable(decl) => Some(analyze_decl(decl.clone(), analyze_all)?),
        Block { statements, last } => {
            if analyze_all {
                for statement in statements.iter_mut() {
                    analyze_statement(statement)?;
                }
            }
            let _asd = 1;
            match last {
                Some(expr) => Some(analyze_expr(&mut **expr, analyze_all)?),
                None => Some("void".to_string()),
            }
        }
    };
    expression.type_ = type_.clone();
    type_.ok_or(())
}

fn analyze_statement(statement: &mut analyzed::Statement) -> Result<(), ()> {
    use analyzed::Statement::*;
    match statement {
        ExpressionStatement(expr) => analyze_expr(expr, true)?,
        VariableDeclaration(decl) => analyze_decl(decl.clone(), true)?,
    };
    Ok(())
}

fn common_type(left: String, right: String) -> Option<String> {
    match (left.as_str(), right.as_str()) {
        (t_left, t_right) if t_left == t_right => Some(t_left.to_string()),
        ("int", "long") | ("long", "int") => Some("long".to_string()),
        ("float", "int") | ("int", "float") => Some("float".to_string()),
        ("double", "int" | "long" | "float") | ("int" | "long" | "float", "double") => {
            Some("double".to_string())
        }
        _ => None,
    }
}

// Errors

impl TypeAnalysisError {
    pub fn new<S: AsRef<str>>(message: S) -> Self {
        Self {
            message: message.as_ref().to_string(),
        }
    }
}

impl Display for TypeAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("AstConversionError: {}", self.message))?;
        Ok(())
    }
}
