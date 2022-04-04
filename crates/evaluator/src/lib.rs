pub mod analyzed {
    pub use type_analysis::program::analyzed::*;
}
mod call_context;
mod emulator;
mod simplifier;

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
        let constants: Vec<_> = self.program.constants.values().map(|e| e.clone()).collect();
        for constant in constants {
            simplifier::simplify_declaration(constant)?;
        }
        Ok(EvaluateResult {
            program: self.program,
        })
    }
}

// Errors

use std::fmt::Display;

use type_analysis::TypeResult;

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
