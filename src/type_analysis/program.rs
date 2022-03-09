use crate::parser::ParseResult;
use indexmap::IndexMap;
use std::{cell::RefCell, fmt::Display, rc::Rc};

mod ast {
    pub use crate::parser::syntax::ast::*;
}

pub mod analyzed {
    use crate::util::MyInto;
    use indexmap::IndexMap;
    use std::{cell::RefCell, rc::Rc};

    #[derive(Debug, Clone)]
    pub struct Type {
        pub name: String,
        pub size: u64,
    }

    #[derive(Debug)]
    pub struct Program {
        pub types: IndexMap<String, Type>,
        pub constants: IndexMap<String, Rc<RefCell<Declaration>>>,
        pub procedures: IndexMap<String, Rc<RefCell<Procedure>>>,
    }

    #[derive(Debug)]
    pub struct Procedure {
        pub name: String,
        pub return_type: Option<String>,
        pub arguments: IndexMap<String, Rc<RefCell<Declaration>>>,
        pub return_value: Box<Expression>,
        pub arg_count: i32,
        pub arg_count_required: i32,
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

    impl Declaration {
        pub fn get_name(&self) -> &str {
            match self {
                Self::Typed { name, .. } | Self::UnTyped { name, .. } => name.as_str(),
            }
        }

        pub fn get(&self) -> (&String, Option<&String>, Option<&Box<Expression>>) {
            match self {
                Self::Typed { name, type_, value } => (name, Some(type_), MyInto::into(value)),
                Self::UnTyped { name, type_, value } => (name, MyInto::into(type_), Some(value)),
            }
        }
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
        Variable(Rc<RefCell<Declaration>>),

        BinOp(Box<Expression>, super::ast::BinOperator, Box<Expression>),
        UnOp(super::ast::UnOperator, Box<Expression>),

        Block {
            statements: Vec<Statement>,
            last: Option<Box<Expression>>,
        },
        /*
        FunctionCall {
            procedure: Rc<RefCell<Procedure>>,
            arguments: Vec<Expression>,
        },
        */
    }

    #[derive(Debug)]
    pub enum Statement {
        ExpressionStatement(Expression),
        VariableDeclaration(Rc<RefCell<Declaration>>),
    }
}

// conversion

#[derive(Debug, Clone)]
pub struct AstConversionError {
    message: String,
}

type Scope = IndexMap<String, Rc<RefCell<analyzed::Declaration>>>;

pub fn convert(parsed: &ParseResult) -> Result<analyzed::Program, AstConversionError> {
    let types = scan_types(&parsed.root)?;
    let mut constants = IndexMap::new();
    let mut procedures = IndexMap::new();

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
                arguments: IndexMap::new(),
                return_value: Box::new(analyzed::Expression::int(0)),
                arg_count: 0,
                arg_count_required: 0,
            })),
        );
    }

    let mut scopes = vec![constants.clone()];

    for ref const_ in parsed.root.constants.iter() {
        let constant = convert_constant(const_, &mut scopes)?;
        *(*constants[const_.name.as_str()]).borrow_mut() = constant;
    }

    for ref proc in parsed.root.functions.iter() {
        let procedure = convert_procedure(proc, &mut scopes)?;
        *(*procedures[proc.name.as_str()]).borrow_mut() = procedure;
    }

    Ok(analyzed::Program {
        types,
        constants,
        procedures,
    })
}

fn scan_types(_root: &ast::Root) -> Result<IndexMap<String, analyzed::Type>, AstConversionError> {
    let mut types = IndexMap::new();
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
    types.insert(
        "void".to_string(),
        analyzed::Type {
            name: "void".to_string(),
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
    let mut arguments = IndexMap::new();
    let mut found_default_argument = false;
    let mut arg_count = 0;
    let mut arg_count_required = 0;

    for ref argument in &procedure.arguments {
        arg_count += 1;
        if found_default_argument && argument.default.is_none() {
            return Err(AstConversionError::new(
                "Argument: Missing default value after encountering an argument that has one",
            ));
        }
        if argument.default.is_some() {
            found_default_argument = true;
        } else {
            arg_count_required += 1;
        }
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
                ast::Type::Auto => Rc::new(RefCell::new(analyzed::Declaration::UnTyped {
                    name: argument.name.clone(),
                    type_: None,
                    value: match &argument.default {
                        Some(val) => Box::new(convert_expression(&*val, scopes)?),
                        _ => {
                            return Err(AstConversionError::new(
                                "Untyped Arguments require default value.",
                            ))
                        }
                    },
                })),
            },
        );
    }
    scopes.push(arguments.clone());

    let ret = analyzed::Procedure {
        name: procedure.name.clone(),
        arguments,
        return_type: match &procedure.return_type {
            ast::Type::Named(ref name) => Some(name.clone()),
            ast::Type::Auto => None,
        },
        return_value: Box::new(convert_expression(&procedure.value, scopes)?),
        arg_count,
        arg_count_required,
    };
    scopes.pop();
    Ok(ret)
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

        Bracketed(expr) => convert_expression(expr, scopes)?,

        Variable(name) => {
            let mut scope_iter = scopes.iter().rev();
            match {
                loop {
                    match scope_iter.next() {
                        Some(scope) => {
                            if let Some(decl) = scope.get(name.as_str()) {
                                break Some(decl.clone());
                            }
                        }
                        None => break None,
                    }
                }
            } {
                Some(decl) => analyzed::Expression {
                    type_: None,
                    value: analyzed::ExpressionValue::Variable(decl),
                },
                None => {
                    return Err(AstConversionError::new(format!(
                        "Variable \"{}\" not found!",
                        name.as_str()
                    )))
                }
            }
        }

        Block { statements, last } => {
            scopes.push(IndexMap::new());
            let mut statements_converted = vec![];
            for s in statements.iter() {
                if let Some(statement) = convert_statement(s, scopes)? {
                    if let analyzed::Statement::VariableDeclaration(decl) = &statement {
                        let scope = scopes.last_mut().unwrap();
                        let d = &*(**decl).borrow();
                        scope.insert(d.get_name().to_string(), decl.clone());
                    }
                    statements_converted.push(statement);
                }
            }
            let l = match last {
                Some(expr) => Some(Box::new(convert_expression(expr, scopes)?)),
                None => None,
            };
            scopes.pop();
            analyzed::Expression {
                type_: None,
                value: analyzed::ExpressionValue::Block {
                    statements: statements_converted,
                    last: l,
                },
            }
        }

        _ => todo!(),
    })
}

fn convert_statement(
    statement: &ast::Statement,
    scopes: &mut Vec<Scope>,
) -> Result<Option<analyzed::Statement>, AstConversionError> {
    use ast::Statement::*;
    Ok(match statement {
        SemiColon => None,
        VariableDeclaration {
            name,
            var_type,
            value,
        } => Some(analyzed::Statement::VariableDeclaration(Rc::new(
            RefCell::new(match var_type {
                ast::Type::Named(type_) => analyzed::Declaration::Typed {
                    name: name.clone(),
                    type_: type_.clone(),
                    value: match value {
                        Some(v) => Some(Box::new(convert_expression(&**v, scopes)?)),
                        None => None,
                    },
                },
                ast::Type::Auto => analyzed::Declaration::UnTyped {
                    name: name.clone(),
                    type_: None,
                    value: match value {
                        Some(v) => Box::new(convert_expression(&**v, scopes)?),
                        None => {
                            return Err(AstConversionError::new(
                                "Untyped variables require initialization value.",
                            ))
                        }
                    },
                },
            }),
        ))),
        ExpressionStatement(expr) => Some(analyzed::Statement::ExpressionStatement(
            convert_expression(expr, scopes)?,
        )),
        If { .. } => todo!(),
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
