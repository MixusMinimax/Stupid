use std::{borrow::BorrowMut, cell::RefMut, fmt::Display};

mod analyzed {
    pub use crate::type_analysis::program::analyzed::*;
}

pub struct TypeAnalysisError {
    message: String,
}

pub fn analyze_program(program: &mut analyzed::Program) -> Result<(), TypeAnalysisError> {
    let count = program.constants.len() + program.procedures.len();
    let mut analyzed_count: i32;
    let mut failed_count: i32;

    loop {
        analyzed_count = 0;
        failed_count = 0;

        for (_, constant) in &program.constants {
            if let Some(_) = constant.borrow_mut().type_ {
                continue;
            }
            match analyze_const(constant.borrow_mut()) {
                Ok(_) => analyzed_count = analyzed_count + 1,
                Err(_) => failed_count = failed_count + 1,
            };
        }

        for (_, procedure) in &program.procedures {
            if let Some(_) = procedure.borrow_mut().return_type {
                continue;
            }
            match analyze_proc(procedure.borrow_mut()) {
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
    }
}

fn analyze_const(mut constant: RefMut<analyzed::Constant>) -> Result<(), ()> {
    match &(&*constant.value).type_ {
        Some(ref name) => Ok({
            constant.type_ = Some(name.clone());
        }),
        _ => Err(()),
    }
}

fn analyze_proc(mut procedure: RefMut<analyzed::Procedure>) -> Result<(), ()> {
    match &(&*procedure.return_value).type_ {
        Some(ref name) => Ok({
            procedure.return_type = Some(name.clone());
        }),
        _ => Err(()),
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
