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
        pub constants: HashMap<String, Rc<RefCell<Declaration>>>,
        pub procedures: HashMap<String, Rc<RefCell<Procedure>>>,
    }

    #[derive(Debug)]
    pub struct Procedure {
        pub name: String,
        pub return_type: Option<String>,
        pub arguments: HashMap<String, Rc<RefCell<Declaration>>>,
        pub return_value: Box<Expression>,
    }

    #[derive(Debug)]
    pub enum Declaration {
        Typed {
            name: String,
            type_: String,
            value: Option<Box<Expression>>,
        },
        UnTyped {
            name: String,
            type_: Option<String>,
            value: Box<Expression>,
        },
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

        BinOp(Box<Expression>, super::ast::BinOperator, Box<Expression>),
        UnOp(super::ast::UnOperator, Box<Expression>),
    }
}

// conversion

#[derive(Debug, Clone)]
pub struct AstConversionError {
    message: String,
}

type Scope = HashMap<String, Rc<RefCell<analyzed::Declaration>>>;

pub fn convert(parsed: &ParseResult) -> Result<analyzed::Program, AstConversionError> {
    let types = scan_types(&parsed.root)?;
    let mut constants = HashMap::new();
    let mut procedures = HashMap::new();

    // Create placeholders to populate root scope.
    for ref constant in parsed.root.constants.iter() {
        constants.insert(
            constant.name.clone(),
            Rc::new(RefCell::new(analyzed::Declaration::UnTyped {
                name: constant.name.clone(),
                type_: None,
                value: Box::new(analyzed::Expression::int(0)),
            })),
        );
    }

    for ref proc in parsed.root.functions.iter() {
        procedures.insert(
            proc.name.clone(),
            Rc::new(RefCell::new(analyzed::Procedure {
                name: proc.name.clone(),
                return_type: None,
                arguments: HashMap::new(),
                return_value: Box::new(analyzed::Expression::int(0)),
            })),
        );
    }

    let mut scopes = vec![constants.clone()];

    for ref constant in parsed.root.constants.iter() {
        constants.insert(
            constant.name.clone(),
            Rc::new(RefCell::new(convert_constant(constant, &mut scopes)?)),
        );
    }

    for ref proc in parsed.root.functions.iter() {
        procedures.insert(
            proc.name.clone(),
            Rc::new(RefCell::new(convert_procedure(proc, &mut scopes)?)),
        );
    }

    Ok(analyzed::Program {
        types,
        constants,
        procedures,
    })
}

fn scan_types(_root: &ast::Root) -> Result<HashMap<String, analyzed::Type>, AstConversionError> {
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

fn convert_constant(
    constant: &ast::Const,
    scopes: &mut Vec<Scope>,
) -> Result<analyzed::Declaration, AstConversionError> {
    Ok(analyzed::Declaration::UnTyped {
        name: constant.name.clone(),
        type_: None,
        value: Box::new(convert_expression(&constant.value, scopes)?),
    })
}

fn convert_procedure(
    procedure: &ast::Proc,
    scopes: &mut Vec<Scope>,
) -> Result<analyzed::Procedure, AstConversionError> {
    let mut arguments = HashMap::new();
    for ref argument in &procedure.arguments {
        arguments.insert(
            argument.name.clone(),
            match &argument.arg_type {
                ast::Type::Named(type_) => Rc::new(RefCell::new(analyzed::Declaration::Typed {
                    name: argument.name.clone(),
                    type_: type_.clone(),
                    value: match &argument.default {
                        Some(val) => Some(Box::new(convert_expression(&*val, scopes)?)),
                        _ => None,
                    },
                })),
                _ => {
                    return Err(AstConversionError::new(
                        "Non-specific argument types not supported.",
                    ))
                }
            },
        );
    }

    Ok(analyzed::Procedure {
        name: procedure.name.clone(),
        arguments,
        return_type: match &procedure.return_type {
            ast::Type::Named(ref name) => Some(name.clone()),
            ast::Type::Auto => None,
        },
        return_value: Box::new(convert_expression(&procedure.value, scopes)?),
    })
}

fn convert_expression(
    expr: &ast::Expression,
    scopes: &mut Vec<Scope>,
) -> Result<analyzed::Expression, AstConversionError> {
    use ast::Expression::*;
    Ok(match &expr {
        Integer(value) => analyzed::Expression::int(*value),
        Long(value) => analyzed::Expression::long(*value),
        Float(value) => analyzed::Expression::float(*value),
        Double(value) => analyzed::Expression::double(*value),

        BinOp(left, op, right) => analyzed::Expression {
            type_: None,
            value: analyzed::ExpressionValue::BinOp(
                Box::new(convert_expression(left, scopes)?),
                op.clone(),
                Box::new(convert_expression(right, scopes)?),
            ),
        },
        UnOp(op, right) => analyzed::Expression {
            type_: None,
            value: analyzed::ExpressionValue::UnOp(
                op.clone(),
                Box::new(convert_expression(right, scopes)?),
            ),
        },

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
