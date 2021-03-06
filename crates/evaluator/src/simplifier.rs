mod analyzed {
    pub use super::super::analyzed::*;
}
use super::EvaluateError;
use analyzed::LiteralType;
use std::{
    cell::RefCell,
    mem::take,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Sub},
    rc::Rc,
};
use type_analysis::program::analyzed::Statement;
use util::MyInto;

pub fn simplify_declaration(decl: Rc<RefCell<analyzed::Declaration>>) -> Result<(), EvaluateError> {
    if let Some(value) = (*decl).borrow_mut().get_mut().2 {
        let x = &mut **value;
        simplify_expression(x)?;
    }
    Ok(())
}

pub fn simplify_procedure(proc: Rc<RefCell<analyzed::Procedure>>) -> Result<(), EvaluateError> {
    for arg in (*proc).borrow_mut().arguments.values() {
        simplify_declaration(arg.clone())?;
    }
    simplify_expression(&mut (*proc).borrow_mut().return_value)?;
    Ok(())
}

pub fn simplify_expression(expr: &mut analyzed::Expression) -> Result<(), EvaluateError> {
    use analyzed::ExpressionValue::*;
    let simplified = match &mut expr.value {
        BinOp(left, op, right) => {
            simplify_expression(&mut **left)?;
            simplify_expression(&mut **right)?;
            execute_binop(&left.value, op, &right.value)
        }
        UnOp(op, expr) => {
            simplify_expression(&mut **expr)?;
            execute_unop(op, &expr.value)
        }
        Block { statements, last } => {
            for statement in statements.iter_mut() {
                simplify_statement(statement)?;
            }
            // Remove all semicolons.
            statements.retain(|x| {
                if let Statement::SemiColon = x {
                    false
                } else {
                    true
                }
            });

            if let Some(last) = last {
                simplify_expression(last)?;
                if statements.is_empty() {
                    Some(take(&mut last.value))
                } else {
                    None
                }
            } else {
                None
            }
        }
        FunctionCall {
            procedure: _,
            arguments,
        } => {
            for arg in arguments.iter_mut() {
                simplify_expression(arg)?;
            }
            None
        }
        Assignment(_, value) => {
            simplify_expression(&mut **value)?;
            None
        }
        IfElse {
            condition,
            then,
            else_,
        } => {
            use analyzed::ExpressionValue::*;
            simplify_expression(&mut **condition)?;
            simplify_expression(&mut **then)?;
            simplify_expression(&mut **else_)?;
            if let analyzed::Expression {
                value: Boolean(b), ..
            } = **condition
            {
                if b {
                    Some(take(&mut then.value))
                } else {
                    Some(take(&mut else_.value))
                }
            } else {
                None
            }
        }
        Integer(_) | Long(_) | Float(_) | Double(_) | Boolean(_) | Variable(_) => None,
    };
    if let Some(s) = simplified {
        expr.value = s;
    }
    Ok(())
}

pub fn simplify_statement(statement: &mut Statement) -> Result<(), EvaluateError> {
    use Statement::*;
    match statement {
        VariableDeclaration(decl) => simplify_declaration(decl.clone()),
        ExpressionStatement(expr) => simplify_expression(expr),
        If { condition, then } => {
            use analyzed::ExpressionValue::*;
            simplify_expression(condition)?;
            simplify_expression(then)?;
            if let analyzed::Expression {
                value: Boolean(b), ..
            } = **condition
            {
                if b {
                    *statement = ExpressionStatement(take(&mut **then));
                } else {
                    *statement = SemiColon;
                }
            }
            Ok(())
        }
        SemiColon => Ok(()),
    }
}

pub fn execute_unop(
    op: &analyzed::UnOperator,
    value: &analyzed::ExpressionValue,
) -> Option<analyzed::ExpressionValue> {
    use analyzed::ExpressionValue::*;
    use analyzed::UnOperator::*;
    return match op {
        Negate => match value {
            Integer(a) => Some(Integer(-a)),
            Long(a) => Some(Long(-a)),
            Float(a) => Some(Float(-a)),
            Double(a) => Some(Double(-a)),
            _ => None,
        },
        Not => match value {
            Boolean(a) => Some(Boolean(!a)),
            Integer(a) => Some(Boolean(*a == 0)),
            Long(a) => Some(Boolean(*a == 0)),
            _ => None,
        },
        BitNot => match value {
            Boolean(a) => Some(Boolean(!a)),
            Integer(a) => Some(Integer(!a)),
            Long(a) => Some(Long(!a)),
            _ => None,
        },
        Deref => None,
    };
}

pub fn execute_binop(
    left: &analyzed::ExpressionValue,
    op: &analyzed::BinOperator,
    right: &analyzed::ExpressionValue,
) -> Option<analyzed::ExpressionValue> {
    use analyzed::ExpressionValue::*;

    if let Some((a, b)) = match (left, right) {
        (Boolean(a), Boolean(b)) => Some((*a, *b)),
        (Boolean(a), Integer(b)) => Some((*a, MyInto::<bool>::into(*b))),
        (Boolean(a), Long(b)) => Some((*a, MyInto::<bool>::into(*b))),
        (Integer(a), Boolean(b)) => Some((MyInto::<bool>::into(*a), *b)),
        (Long(a), Boolean(b)) => Some((MyInto::<bool>::into(*a), *b)),
        _ => None,
    } {
        return execute_logic(a, op, b);
    };
    if let Some((a, b)) = match (left, right) {
        (Integer(a), Integer(b)) => Some((*a, *b)),
        _ => None,
    } {
        return execute_arithmetic(a, op, b).or_else(|| execute_logic(a, op, b));
    };
    if let Some((a, b)) = match (left, right) {
        (Long(a), Long(b)) => Some((*a, *b)),
        (Integer(a), Long(b)) => Some((*a as i64, *b)),
        (Long(a), Integer(b)) => Some((*a, *b as i64)),
        _ => None,
    } {
        return execute_arithmetic(a, op, b).or_else(|| execute_logic(a, op, b));
    };
    if let Some((a, b)) = match (left, right) {
        (Float(a), Float(b)) => Some((*a, *b)),
        (Integer(a), Float(b)) => Some((*a as f32, *b)),
        (Float(a), Integer(b)) => Some((*a, *b as f32)),
        _ => None,
    } {
        return execute_arithmetic(a, op, b);
    };
    if let Some((a, b)) = match (left, right) {
        (Double(a), Double(b)) => Some((*a, *b)),
        (Integer(a), Double(b)) => Some((*a as f64, *b)),
        (Long(a), Double(b)) => Some((*a as f64, *b)),
        (Float(a), Double(b)) => Some((*a as f64, *b)),
        (Double(a), Integer(b)) => Some((*a, *b as f64)),
        (Double(a), Long(b)) => Some((*a, *b as f64)),
        (Double(a), Float(b)) => Some((*a, *b as f64)),
        _ => None,
    } {
        return execute_arithmetic(a, op, b);
    };
    None
}

pub fn execute_arithmetic<T: Add + Sub + Mul + Div + PartialEq + PartialOrd>(
    left: T,
    op: &analyzed::BinOperator,
    right: T,
) -> Option<analyzed::ExpressionValue>
where
    <T as Add>::Output: Into<LiteralType>,
    <T as Sub>::Output: Into<LiteralType>,
    <T as Mul>::Output: Into<LiteralType>,
    <T as Div>::Output: Into<LiteralType>,
{
    use analyzed::BinOperator::*;
    match op {
        Add => Some(analyzed::Expression::literal(left + right).value),
        Subtract => Some(analyzed::Expression::literal(left - right).value),
        Multiply => Some(analyzed::Expression::literal(left * right).value),
        Divide => Some(analyzed::Expression::literal(left / right).value),
        Equals => Some(analyzed::Expression::literal(left == right).value),
        NotEquals => Some(analyzed::Expression::literal(left != right).value),
        Greater => Some(analyzed::Expression::literal(left > right).value),
        GreaterEq => Some(analyzed::Expression::literal(left >= right).value),
        Less => Some(analyzed::Expression::literal(left < right).value),
        LessEq => Some(analyzed::Expression::literal(left <= right).value),
        And | Or | BitAnd | BitOr | BitXor => None,
    }
}

pub fn execute_logic<T: BitAnd + BitOr + BitXor + MyInto<bool>>(
    left: T,
    op: &analyzed::BinOperator,
    right: T,
) -> Option<analyzed::ExpressionValue>
where
    <T as BitAnd>::Output: Into<LiteralType>,
    <T as BitOr>::Output: Into<LiteralType>,
    <T as BitXor>::Output: Into<LiteralType>,
{
    use analyzed::BinOperator::*;
    match op {
        BitAnd => Some(analyzed::Expression::literal(left & right).value),
        BitOr => Some(analyzed::Expression::literal(left | right).value),
        BitXor => Some(analyzed::Expression::literal(left ^ right).value),
        And => Some(analyzed::ExpressionValue::Boolean(
            left.into() && right.into(),
        )),
        Or => Some(analyzed::ExpressionValue::Boolean(
            left.into() || right.into(),
        )),
        _ => None,
    }
}
