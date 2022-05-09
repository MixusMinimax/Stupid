use std::{cell::RefCell, rc::Rc};

use evaluator::analyzed::Procedure;

use crate::{
    compiler::CompileError,
    expression_compiler::compile_expression,
    intermediary_language::{Function, Instruction, Variable, VariableSize},
};

#[derive(Debug)]
pub struct FunctionCompiler<'a> {
    procedure: &'a Procedure,
    next_var_id: u64,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(procedure: &'a Procedure) -> Self {
        FunctionCompiler {
            procedure,
            next_var_id: 0,
        }
    }

    pub fn compile(mut self) -> Result<Function, CompileError> {
        let mut compiled = Function::new(&self.procedure.name);
        compiled
            .instructions
            .push(Instruction::Label(self.procedure.name.clone()));

        // NOTE: I think default arguments will be compiled into the call site.
        //   ALternatively, there could be initialization blocks inside the function,
        //   Which are executed when needed.

        let return_variable = compile_expression(
            &self.procedure.return_value,
            &mut compiled.instructions,
            &mut |size| self.new_variable(size),
        )?;

        compiled
            .instructions
            .push(Instruction::Return(return_variable));
        Ok(compiled)
    }

    fn new_variable(&mut self, size: VariableSize) -> Variable {
        let ret = Variable {
            id: self.next_var_id,
            size,
        };
        self.next_var_id += 1;
        ret
    }
}
