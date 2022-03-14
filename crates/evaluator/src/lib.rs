use std::fmt::Display;
use type_analysis::TypeResult;

pub mod analyzed {
    pub use type_analysis::program::analyzed::*;
}

#[derive(Debug)]
pub struct Evaluator {
    program: analyzed::Program,
}

#[derive(Debug)]
pub struct EvaluateResult {
    pub program: analyzed::Program,
}

impl Evaluator {
    pub fn new(typed_ast: TypeResult) -> Self {
        Evaluator {
            program: typed_ast.program,
        }
    }

    pub fn evaluate(self) -> Result<EvaluateResult, EvaluateError> {
        drop(&self.program);
        todo!();
    }
}

// Errors

#[derive(Debug)]
pub struct EvaluateError {
    message: String,
}

impl EvaluateError {
    pub fn new<S: AsRef<str>>(message: S) -> Self {
        Self {
            message: message.as_ref().to_string(),
        }
    }
}

impl Display for EvaluateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("EvaluateError: {}", self.message))?;
        Ok(())
    }
}
