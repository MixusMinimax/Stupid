use std::{
    cell::RefCell,
    fmt::Display,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Sub},
    rc::Rc,
};
use type_analysis::{program::analyzed::LiteralType, TypeResult};
use util::MyInto;

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

    pub fn evaluate(mut self) -> Result<EvaluateResult, EvaluateError> {
        let constants: Vec<_> = self.program.constants.values().map(|e| e.clone()).collect();
        for constant in constants {
            self.simplify_constant(constant)?;
        }
        Ok(EvaluateResult {
            program: self.program,
        })
    }

    fn simplify_constant(
        &mut self,
        constant: Rc<RefCell<analyzed::Declaration>>,
    ) -> Result<(), EvaluateError> {
        if let Some(value) = (*constant).borrow_mut().get_mut().2 {
            let x = &mut **value;
            self.simplify_expression(x)?;
        }
        Ok(())
    }

    fn simplify_expression(
        &mut self,
        expr: &mut analyzed::Expression,
    ) -> Result<(), EvaluateError> {
        use analyzed::ExpressionValue::*;
        let simplified = match &mut expr.value {
            BinOp(left, op, right) => {
                self.simplify_expression(&mut **left)?;
                self.simplify_expression(&mut **right)?;
                Self::execute_binop(&left.value, op, &right.value)
            }
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
            _ => None,
        };
        if let Some(s) = simplified {
            expr.value = s;
        }
        Ok(())
    }

    fn execute_binop(
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
            return Self::execute_logic(a, op, b);
        };
        if let Some((a, b)) = match (left, right) {
            (Integer(a), Integer(b)) => Some((*a, *b)),
            _ => None,
        } {
            return Self::execute_arithmetic(a, op, b).or_else(|| Self::execute_logic(a, op, b));
        };
        if let Some((a, b)) = match (left, right) {
            (Long(a), Long(b)) => Some((*a, *b)),
            (Integer(a), Long(b)) => Some((*a as i64, *b)),
            (Long(a), Integer(b)) => Some((*a, *b as i64)),
            _ => None,
        } {
            return Self::execute_arithmetic(a, op, b).or_else(|| Self::execute_logic(a, op, b));
        };
        if let Some((a, b)) = match (left, right) {
            (Float(a), Float(b)) => Some((*a, *b)),
            (Integer(a), Float(b)) => Some((*a as f32, *b)),
            (Float(a), Integer(b)) => Some((*a, *b as f32)),
            _ => None,
        } {
            return Self::execute_arithmetic(a, op, b);
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
            return Self::execute_arithmetic(a, op, b);
        };
        None
    }

    fn execute_arithmetic<T: Add + Sub + Mul + Div + PartialEq + PartialOrd>(
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

    fn execute_logic<T: BitAnd + BitOr + BitXor + MyInto<bool>>(
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
