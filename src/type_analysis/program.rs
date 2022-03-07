use crate::parser::ParseResult;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

mod ast {
    pub use crate::parser::syntax::ast::*;
}

pub mod analyzed {
    use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

    #[derive(Debug, Clone)]
    pub struct Type {
        pub name: String,
        pub size: u64,
    }

    #[derive(Debug)]
    pub struct Program {
        pub types: HashMap<String, Type>,
        pub constants: HashMap<String, RefCell<Constant>>,
        pub procedures: HashMap<String, RefCell<Procedure>>,
    }

    #[derive(Debug)]
    pub struct Constant {
        pub name: String,
        pub type_: Option<String>,
        pub value: Box<Expression>,
    }

    #[derive(Debug)]
    pub struct Procedure {
        pub name: String,
        pub return_type: Option<String>,
        pub arguments: HashMap<String, Rc<Argument>>,
        pub return_value: Box<Expression>,
    }

    #[derive(Debug)]
    pub struct Argument {
        pub name: String,
        pub type_: String,
        pub default: Option<i32>,
    }

    #[derive(Debug)]
    pub struct Expression {
        pub value: ExpressionValue,
        pub type_: Option<String>,
    }

    impl Expression {
        pub fn int(value: i32) -> Self {
            Expression {
                value: ExpressionValue::Integer(value),
                type_: Some("int".to_string()),
            }
        }

        pub fn long(value: i64) -> Self {
            Expression {
                value: ExpressionValue::Long(value),
                type_: Some("long".to_string()),
            }
        }

        pub fn float(value: f32) -> Self {
            Expression {
                value: ExpressionValue::Float(value),
                type_: Some("float".to_string()),
            }
        }

        pub fn double(value: f64) -> Self {
            Expression {
                value: ExpressionValue::Double(value),
                type_: Some("double".to_string()),
            }
        }
    }

    #[derive(Debug)]
    pub enum ExpressionValue {
        Integer(i32),
        Long(i64),
        Float(f32),
        Double(f64),
    }
}

#[derive(Debug, Clone)]
pub struct AstConversionError {
    message: String,
}

pub fn convert(parsed: &ParseResult) -> Result<analyzed::Program, AstConversionError> {
    let types = scan_types(&parsed.root)?;
    let mut constants = HashMap::new();
    let mut procedures = HashMap::new();

    for ref constant in parsed.root.constants.iter() {
        constants.insert(
            constant.name.clone(),
            RefCell::new(convert_constant(constant)?),
        );
    }

    for ref proc in parsed.root.functions.iter() {
        procedures.insert(proc.name.clone(), RefCell::new(convert_procedure(proc)?));
    }

    Ok(analyzed::Program {
        types,
        constants,
        procedures,
    })
}

// conversion

fn scan_types(root: &ast::Root) -> Result<HashMap<String, analyzed::Type>, AstConversionError> {
    let mut types = HashMap::new();
    types.insert(
        "int".to_string(),
        analyzed::Type {
            name: "int".to_string(),
            size: 4,
        },
    );
    types.insert(
        "long".to_string(),
        analyzed::Type {
            name: "long".to_string(),
            size: 8,
        },
    );
    types.insert(
        "float".to_string(),
        analyzed::Type {
            name: "float".to_string(),
            size: 4,
        },
    );
    types.insert(
        "double".to_string(),
        analyzed::Type {
            name: "double".to_string(),
            size: 8,
        },
    );
    // TODO: allow custom types to exist. Size dependency needs to be resolved.
    Ok(types)
}

fn convert_constant(constant: &ast::Const) -> Result<analyzed::Constant, AstConversionError> {
    Ok(analyzed::Constant {
        name: constant.name.clone(),
        type_: None,
        value: Box::new(convert_expression(&constant.value)?),
    })
}

fn convert_procedure(procedure: &ast::Proc) -> Result<analyzed::Procedure, AstConversionError> {
    Ok(analyzed::Procedure {
        name: procedure.name.clone(),
        arguments: HashMap::new(),
        return_type: match &procedure.return_type {
            ast::Type::Named(ref name) => Some(name.clone()),
            ast::Type::Auto => None,
        },
        return_value: Box::new(convert_expression(&procedure.value)?),
    })
}

fn convert_expression(expr: &ast::Expression) -> Result<analyzed::Expression, AstConversionError> {
    Ok(match &expr {
        ast::Expression::Integer(value) => analyzed::Expression::int(*value),
        ast::Expression::Long(value) => analyzed::Expression::long(*value),
        ast::Expression::Float(value) => analyzed::Expression::float(*value),
        ast::Expression::Double(value) => analyzed::Expression::double(*value),
        _ => return Err(AstConversionError::new("Not implemented!")),
    })
}

// Errors

impl AstConversionError {
    pub fn new<S: AsRef<str>>(message: S) -> Self {
        Self {
            message: message.as_ref().to_string(),
        }
    }
}

impl Display for AstConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("AstConversionError: {}", self.message))?;
        Ok(())
    }
}
