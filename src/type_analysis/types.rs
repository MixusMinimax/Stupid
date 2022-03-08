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
            match analyze_decl(constant.clone()) {
                Ok(_) => analyzed_count = analyzed_count + 1,
                Err(_) => failed_count = failed_count + 1,
            };
        }

        for (_, procedure) in &program.procedures {
            if let Some(_) = (*procedure).borrow().return_type {
                continue;
            }
            match analyze_proc(procedure.clone()) {
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
    // TODO: analyze not needed thingies
    Ok(())
}

fn analyze_decl(declaration: Rc<RefCell<analyzed::Declaration>>) -> Result<String, ()> {
    match &mut *(*declaration).borrow_mut() {
        analyzed::Declaration::UnTyped {
            ref mut type_,
            ref mut value,
            ..
        } => match type_ {
            Some(name) => Ok(name.clone()),
            None => {
                let t = analyze_expr(&mut **value)?;
                *type_ = Some(t.clone());
                Ok(t)
            }
        },
        analyzed::Declaration::Typed { ref type_, .. } => Ok(type_.clone()),
    }
}

fn analyze_proc(procedure: Rc<RefCell<analyzed::Procedure>>) -> Result<(), ()> {
    let return_type = Some(analyze_expr(&mut *(*procedure).borrow_mut().return_value)?);
    (*procedure).borrow_mut().return_type = return_type;
    Ok(())
}

fn analyze_expr(expression: &mut analyzed::Expression) -> Result<String, ()> {
    use analyzed::ExpressionValue::*;

    if let Some(ref name) = expression.type_ {
        return Ok(name.clone());
    }
    let type_ = match &mut expression.value {
        BinOp(left, _, right) => common_type(analyze_expr(&mut *left)?, analyze_expr(&mut *right)?),
        UnOp(_, expr) => Some(analyze_expr(&mut *expr)?),
        Variable(decl) => Some(analyze_decl(decl.clone())?),
        Block { last, .. } => match last {
            Some(expr) => Some(analyze_expr(&mut **expr)?),
            None => Some("void".to_string()),
        },
        _ => None,
    };
    expression.type_ = type_.clone();
    type_.ok_or(())
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
