use crate::intermediary_language::{Constant, Function, Instruction, IntermediaryLanguage};
use evaluator::{
    analyzed::{Declaration, Procedure},
    EvaluateResult,
};
use indexmap::IndexMap;
use std::{borrow::Borrow, cell::RefCell, fmt::Display, rc::Rc};
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
        let mut result = IntermediaryLanguage {
            constants: IndexMap::new(),
            functions: IndexMap::new(),
        };
        for constant in self.typed_ast.constants.values() {
            let compiled = self.compile_constant(constant)?;
            result.constants.insert(compiled.name.clone(), compiled);
        }
        for procedure in self.typed_ast.procedures.values() {
            let compiled = self.compile_function(procedure)?;
            result.functions.insert(compiled.name.clone(), compiled);
        }
        Ok(result)
    }

    fn compile_constant(&self, c: &Rc<RefCell<Declaration>>) -> Result<Constant, CompileError> {
        let ast_constant = (**c).borrow();
        let compiled = Constant {
            name: ast_constant.get_name().to_string(),
            init: vec![],
        };
        Ok(compiled)
    }

    fn compile_function(&self, p: &Rc<RefCell<Procedure>>) -> Result<Function, CompileError> {
        let ast_procedure = (**p).borrow();
        let mut compiled = Function {
            name: ast_procedure.name.clone(),
            instructions: vec![Instruction::Label(ast_procedure.name.clone())],
        };
        compiled.instructions.push(Instruction::Return);
        Ok(compiled)
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
