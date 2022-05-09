use crate::{
    compiler::CompileError,
    intermediary_language::{Immediate, Instruction, Variable, VariableSize},
};
use evaluator::analyzed::{Expression, ExpressionValue, Statement};

pub fn compile_expression<F: FnMut(VariableSize) -> Variable>(
    expression: &Expression,
    instructions: &mut Vec<Instruction>,
    variable_generator: &mut F,
) -> Result<Variable, CompileError> {
    use ExpressionValue::*;
    Ok(match &expression.value {
        Integer(value) => compile_immediate(
            *value as u64,
            VariableSize::DoubleWord,
            instructions,
            variable_generator,
        )?,
        Long(value) => compile_immediate(
            *value as u64,
            VariableSize::Long,
            instructions,
            variable_generator,
        )?,
        Float(_) => todo!(),
        Double(_) => todo!(),
        Boolean(_) => todo!(),
        Variable(_) => todo!(),
        BinOp(_, _, _) => todo!(),
        UnOp(_, _) => todo!(),
        Block { statements, last } => {
            for statement in statements.iter() {
                compile_statement(statement, instructions, variable_generator)?;
            }
            if let Some(value) = last {
                compile_expression(&**value, instructions, variable_generator)?
            } else {
                todo!()
            }
        }
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

fn compile_immediate<F: FnMut(VariableSize) -> Variable>(
    value: u64,
    size: VariableSize,
    instructions: &mut Vec<Instruction>,
    variable_generator: &mut F,
) -> Result<Variable, CompileError> {
    let var = variable_generator(VariableSize::DoubleWord);
    instructions.push(Instruction::AssignImmediate(
        var.clone(),
        Immediate::Integer { value, size },
    ));
    Ok(var)
}

fn compile_statement<F: FnMut(VariableSize) -> Variable>(
    statement: &Statement,
    instructions: &mut Vec<Instruction>,
    variable_generator: &mut F,
) -> Result<(), CompileError> {
    match statement {
        Statement::ExpressionStatement(expression) => {
            compile_expression(expression, instructions, variable_generator)?;
            Ok(())
        }
        Statement::VariableDeclaration(d) => {
            let declaration = &*(**d).borrow();
            let (_, _, v) = declaration.get();
            if let Some(value) = v {
                let _variable = compile_expression(&**value, instructions, variable_generator)?;
                // TODO: store variable in map from decl to variable names
            }
            Ok(())
        }
        Statement::If {
            condition: _,
            then: _,
        } => todo!(),
        Statement::SemiColon => todo!(),
    }
}
