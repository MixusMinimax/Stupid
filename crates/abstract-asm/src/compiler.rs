use crate::intermediary_language::IntermediaryLanguage;
use evaluator::EvaluateResult;
use std::fmt::Display;
mod analyzed {
    pub use evaluator::analyzed::*;
}

#[derive(Debug)]
pub struct Compiler {
    typed_ast: analyzed::Program,
}

impl Compiler {
    pub fn new(typed_ast: EvaluateResult) -> Self {
        Compiler {
            typed_ast: typed_ast.program,
        }
    }

    pub fn compile(&self) -> Result<IntermediaryLanguage, CompileError> {
        drop(&self.typed_ast);
        todo!();
    }
}

// Errors

#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
}

impl CompileError {
    pub fn new<S: AsRef<str>>(message: S) -> Self {
        Self {
            message: message.as_ref().to_string(),
        }
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("CompileError: {}", self.message))
    }
}
