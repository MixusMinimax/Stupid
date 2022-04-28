use std::env::var;

use crate::{
    compiler::CompileError,
    intermediary_language::{Immediate, Instruction, Variable, VariableSize},
};
use evaluator::analyzed::{Expression, ExpressionValue};

pub fn compile_expression<F: FnMut(VariableSize) -> Variable>(
    expression: &Expression,
    instructions: &mut Vec<Instruction>,
    mut variable_generator: F,
) -> Result<Variable, CompileError> {
    use ExpressionValue::*;
    Ok(match &expression.value {
        Integer(value) => {
            let var = variable_generator(VariableSize::DoubleWord);
            instructions.push(Instruction::AssignImmediate(
                var.clone(),
                Immediate::Integer {
                    value: *value as u64,
                    size: VariableSize::DoubleWord,
                },
            ));
            var
        }
        Long(_) => todo!(),
        Float(_) => todo!(),
        Double(_) => todo!(),
        Boolean(_) => todo!(),
        Variable(_) => todo!(),
        BinOp(_, _, _) => todo!(),
        UnOp(_, _) => todo!(),
        Block {
            statements: _,
            last: _,
        } => todo!(),
        FunctionCall {
            procedure: _,
            arguments: _,
        } => todo!(),
        Assignment(_, _) => todo!(),
        IfElse {
            condition: _,
            then: _,
            else_: _,
        } => todo!(),
    })
}
