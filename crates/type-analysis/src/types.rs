use std::{cell::RefCell, fmt::Display, rc::Rc};

mod analyzed {
    pub use crate::program::analyzed::*;
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

        for constant in program.constants.values() {
            if let analyzed::Declaration::UnTyped { type_: Some(_), .. } = &*(**constant).borrow() {
                continue;
            }
            match analyze_decl(constant.clone(), false) {
                Ok(_) => analyzed_count = analyzed_count + 1,
                Err(_) => failed_count = failed_count + 1,
            };
        }

        for procedure in program.procedures.values() {
            if let Some(_) = (**procedure).borrow().return_type {
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
    // Now, we can fully analyze constants and procedures, including irrelevant variables and arguments.
    for constant in program.constants.values() {
        analyze_decl(constant.clone(), true)?;
    }

    for procedure in program.procedures.values() {
        analyze_proc(procedure.clone(), true)?;
    }
    Ok(())
}

fn analyze_decl(
    declaration: Rc<RefCell<analyzed::Declaration>>,
    analyze_all: bool,
) -> Result<String, TypeAnalysisError> {
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
                    let actual_type = analyze_expr(&mut **v, analyze_all)?;
                    check_assignable(&actual_type, type_)?;
                }
            }
            Ok(type_.clone())
        }
    }
}

fn analyze_proc(
    procedure: Rc<RefCell<analyzed::Procedure>>,
    analyze_all: bool,
) -> Result<(), TypeAnalysisError> {
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

fn analyze_expr(
    expression: &mut analyzed::Expression,
    analyze_all: bool,
) -> Result<String, TypeAnalysisError> {
    use analyzed::ExpressionValue::*;

    if !analyze_all {
        if let Some(ref name) = expression.type_ {
            return Ok(name.clone());
        }
    }
    let type_ = match &mut expression.value {
        Integer(_) | Long(_) | Float(_) | Double(_) | Boolean(_) => {
            return expression.type_.clone().ok_or(TypeAnalysisError::new(
                "Literal was missing type. This is not possible",
            ))
        }
        BinOp(left, op, right) => {
            use parser::syntax::ast::BinOperator::*;
            match op {
                Add | Subtract | Multiply | Divide | BitAnd | BitOr | BitEor => common_type(
                    &analyze_expr(&mut *left, analyze_all)?,
                    &analyze_expr(&mut *right, analyze_all)?,
                ),
                Equals | NotEquals | Greater | GreaterEq | Less | LessEq | And | Or => {
                    if analyze_all {
                        analyze_expr(&mut *left, analyze_all)?;
                        analyze_expr(&mut *right, analyze_all)?;
                    }
                    Some("bool".to_string())
                }
            }
        }
        UnOp(op, expr) => {
            use parser::syntax::ast::UnOperator::*;
            match op {
                Negate | BitNot => Some(analyze_expr(&mut *expr, analyze_all)?),
                Not => {
                    if analyze_all {
                        analyze_expr(&mut *expr, analyze_all)?;
                    }
                    Some("bool".to_string())
                }
                Deref => todo!(),
            }
        }
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
        FunctionCall {
            procedure,
            arguments,
        } => {
            if analyze_all {
                for (expr, decl) in arguments
                    .iter_mut()
                    .zip((*procedure.clone()).borrow().arguments.values())
                {
                    let actual_type = analyze_expr(expr, analyze_all)?;
                    let expected_type = analyze_decl(decl.clone(), analyze_all)?;
                    check_assignable(&actual_type, &expected_type)?;
                }
            };
            let return_type = (**procedure).borrow().return_type.clone();
            return_type
        }
        Assignment(decl, value) => {
            let expected_type = analyze_decl(decl.clone(), analyze_all)?;
            if analyze_all {
                let actual_type = analyze_expr(value, analyze_all)?;
                check_assignable(&actual_type, &expected_type)?;
            };
            Some(expected_type)
        }
        IfElse {
            condition,
            then,
            else_,
        } => {
            if analyze_all {
                let cond_t = analyze_expr(condition, analyze_all)?;
                check_assignable(&cond_t, &"bool".to_string())?;
            };
            let then_t = analyze_expr(then, analyze_all)?;
            let else_t = analyze_expr(else_, analyze_all)?;
            common_type(&then_t, &else_t)
        }
    };
    expression.type_ = type_.clone();
    type_.ok_or(TypeAnalysisError::new("Failed to analyze expression"))
}

fn analyze_statement(statement: &mut analyzed::Statement) -> Result<(), TypeAnalysisError> {
    use analyzed::Statement::*;
    match statement {
        ExpressionStatement(expr) => analyze_expr(expr, true)?,
        VariableDeclaration(decl) => analyze_decl(decl.clone(), true)?,
        If { condition, then } => {
            analyze_expr(condition, true)?;
            analyze_expr(then, true)?
        }
    };
    Ok(())
}

fn common_type(left: &String, right: &String) -> Option<String> {
    match (left.as_str(), right.as_str()) {
        (t_left, t_right) if t_left == t_right => Some(t_left.to_string()),
        ("int", "long") | ("long", "int") => Some("long".to_string()),
        ("float", "int") | ("int", "float") => Some("float".to_string()),
        ("double", "int" | "long" | "float") | ("int" | "long" | "float", "double") => {
            Some("double".to_string())
        }
        ("bool", "int" | "long") | ("int" | "long", "bool") => Some("bool".to_string()),
        _ => None,
    }
}

fn check_assignable(actual_type: &String, expected_type: &String) -> Result<(), TypeAnalysisError> {
    if !(match common_type(actual_type, expected_type) {
        Some(common) => common == *expected_type,
        _ => false,
    }) {
        Err(TypeAnalysisError::new(format!(
            "Expected {}, but got {}",
            expected_type, actual_type
        )))
    } else {
        Ok(())
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
