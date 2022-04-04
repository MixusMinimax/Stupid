//! Recursively evaluates constants.
//! Evaluates function calls in addition to other expressions.

use std::{cell::RefCell, rc::Rc};
use type_analysis::program::analyzed::{Declaration, Expression, ExpressionValue, Program};

#[derive(Debug)]
pub struct Emulator<'a> {
    program: &'a Program,
}

impl<'a> Emulator<'a> {
    pub fn new(program: &'a Program) -> Self {
        Emulator { program }
    }

    pub fn evaluate(&self) {}

    fn evaluate_constant(&self, constant: Rc<RefCell<Declaration>>) {
        if let Some(value) = (*constant).borrow_mut().get_mut().2 {
            let x = &mut **value;
            self.evaluate_expression(x);
        }
    }

    fn evaluate_expression(&self, expr: &mut Expression) {
        use ExpressionValue::*;
    }
}
