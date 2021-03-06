use indexmap::IndexMap;
use parser::ParseResult;
use std::{cell::RefCell, fmt::Display, rc::Rc};

mod ast {
    pub use parser::syntax::ast::*;
}

pub mod analyzed {
    pub use super::ast::{BinOperator, UnOperator};
    use indexmap::IndexMap;
    use std::{cell::RefCell, default, rc::Rc};
    use util::MyInto;

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

        pub fn get_mut(
            &mut self,
        ) -> (
            &mut String,
            Option<&mut String>,
            Option<&mut Box<Expression>>,
        ) {
            match self {
                Self::Typed { name, type_, value } => (name, Some(type_), MyInto::into(value)),
                Self::UnTyped { name, type_, value } => (name, MyInto::into(type_), Some(value)),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Expression {
        pub value: ExpressionValue,
        pub type_: Option<String>,
    }

    pub enum LiteralType {
        I32(i32),
        I64(i64),
        F32(f32),
        F64(f64),
        Bool(bool),
    }

    impl From<i32> for LiteralType {
        fn from(a: i32) -> Self {
            LiteralType::I32(a)
        }
    }

    impl From<i64> for LiteralType {
        fn from(a: i64) -> Self {
            LiteralType::I64(a)
        }
    }

    impl From<f32> for LiteralType {
        fn from(a: f32) -> Self {
            LiteralType::F32(a)
        }
    }

    impl From<f64> for LiteralType {
        fn from(a: f64) -> Self {
            LiteralType::F64(a)
        }
    }

    impl From<bool> for LiteralType {
        fn from(a: bool) -> Self {
            LiteralType::Bool(a)
        }
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

        pub fn boolean(value: bool) -> Self {
            Expression {
                value: ExpressionValue::Boolean(value),
                type_: Some("bool".to_string()),
            }
        }

        pub fn literal<T: Into<LiteralType>>(value: T) -> Self {
            match value.into() {
                LiteralType::I32(a) => Self::int(a),
                LiteralType::I64(a) => Self::long(a),
                LiteralType::F32(a) => Self::float(a),
                LiteralType::F64(a) => Self::double(a),
                LiteralType::Bool(a) => Self::boolean(a),
            }
        }
    }

    impl Default for Expression {
        fn default() -> Self {
            Expression {
                value: ExpressionValue::default(),
                type_: Some("void".to_string()),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ExpressionValue {
        Integer(i32),
        Long(i64),
        Float(f32),
        Double(f64),
        Boolean(bool),
        Variable(Rc<RefCell<Declaration>>),

        BinOp(Box<Expression>, super::ast::BinOperator, Box<Expression>),
        UnOp(super::ast::UnOperator, Box<Expression>),

        Block {
            statements: Vec<Statement>,
            last: Option<Box<Expression>>,
        },
        FunctionCall {
            procedure: Rc<RefCell<Procedure>>,
            arguments: Vec<Expression>,
        },

        Assignment(Rc<RefCell<Declaration>>, Box<Expression>),

        IfElse {
            condition: Box<Expression>,
            then: Box<Expression>,
            else_: Box<Expression>,
        },
    }

    impl Default for ExpressionValue {
        fn default() -> Self {
            ExpressionValue::Block {
                statements: Vec::new(),
                last: None,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Statement {
        ExpressionStatement(Expression),
        VariableDeclaration(Rc<RefCell<Declaration>>),
        If {
            condition: Box<Expression>,
            then: Box<Expression>,
        },
        SemiColon,
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
    for constant in parsed.root.constants.iter() {
        constants.insert(
            constant.name.clone(),
            Rc::new(RefCell::new(analyzed::Declaration::UnTyped {
                name: constant.name.clone(),
                type_: None,
                value: Box::new(analyzed::Expression::int(0)),
            })),
        );
    }

    for proc in parsed.root.functions.iter() {
        procedures.insert(
            proc.name.clone(),
            Rc::new(RefCell::new(convert_procedure_declaration(proc)?)),
        );
    }

    let mut scopes = vec![constants.clone()];

    for const_ in parsed.root.constants.iter() {
        let constant = convert_constant(const_, &mut scopes, &procedures)?;
        *(*constants[const_.name.as_str()]).borrow_mut() = constant;
    }

    for proc in parsed.root.functions.iter() {
        convert_procedure(
            proc,
            procedures[proc.name.as_str()].clone(),
            &mut scopes,
            &procedures,
        )?;
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
    types.insert(
        "bool".to_string(),
        analyzed::Type {
            name: "bool".to_string(),
            size: 4,
        },
    );
    // TODO: allow custom types to exist. Size dependency needs to be resolved.
    Ok(types)
}

fn convert_constant(
    constant: &ast::Const,
    scopes: &mut Vec<Scope>,
    procedures: &IndexMap<String, Rc<RefCell<analyzed::Procedure>>>,
) -> Result<analyzed::Declaration, AstConversionError> {
    Ok(analyzed::Declaration::UnTyped {
        name: constant.name.clone(),
        type_: None,
        value: Box::new(convert_expression(&constant.value, scopes, procedures)?),
    })
}

fn convert_procedure_declaration(
    procedure: &ast::Proc,
) -> Result<analyzed::Procedure, AstConversionError> {
    let mut arg_count = 0;
    let mut arg_count_required = 0;
    let mut found_default_argument = false;
    for argument in procedure.arguments.iter() {
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
    }
    Ok(analyzed::Procedure {
        name: procedure.name.clone(),
        return_type: None,
        arguments: IndexMap::new(),
        return_value: Box::new(analyzed::Expression::int(0)),
        arg_count,
        arg_count_required,
    })
}

fn convert_procedure(
    procedure: &ast::Proc,
    procedure_converted: Rc<RefCell<analyzed::Procedure>>,
    scopes: &mut Vec<Scope>,
    procedures: &IndexMap<String, Rc<RefCell<analyzed::Procedure>>>,
) -> Result<(), AstConversionError> {
    let mut arguments = IndexMap::new();

    for argument in procedure.arguments.iter() {
        arguments.insert(
            argument.name.clone(),
            match &argument.arg_type {
                ast::Type::Named(type_) => Rc::new(RefCell::new(analyzed::Declaration::Typed {
                    name: argument.name.clone(),
                    type_: type_.clone(),
                    value: match &argument.default {
                        Some(val) => Some(Box::new(convert_expression(&*val, scopes, procedures)?)),
                        _ => None,
                    },
                })),
                ast::Type::Auto => Rc::new(RefCell::new(analyzed::Declaration::UnTyped {
                    name: argument.name.clone(),
                    type_: None,
                    value: match &argument.default {
                        Some(val) => Box::new(convert_expression(&*val, scopes, procedures)?),
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
    let return_value = Box::new(convert_expression(&procedure.value, scopes, procedures)?);
    scopes.pop();

    let proc = &mut *(*procedure_converted).borrow_mut();
    proc.name = procedure.name.clone();
    proc.arguments = arguments;
    proc.return_type = match &procedure.return_type {
        ast::Type::Named(name) => Some(name.clone()),
        ast::Type::Auto => None,
    };
    proc.return_value = return_value;

    Ok(())
}

fn convert_expression(
    expr: &ast::Expression,
    scopes: &mut Vec<Scope>,
    procedures: &IndexMap<String, Rc<RefCell<analyzed::Procedure>>>,
) -> Result<analyzed::Expression, AstConversionError> {
    use ast::Expression::*;
    Ok(analyzed::Expression {
        type_: None,
        value: match expr {
            Integer(value) => return Ok(analyzed::Expression::int(*value)),
            Long(value) => return Ok(analyzed::Expression::long(*value)),
            Float(value) => return Ok(analyzed::Expression::float(*value)),
            Double(value) => return Ok(analyzed::Expression::double(*value)),
            Boolean(value) => return Ok(analyzed::Expression::boolean(*value)),

            BinOp(left, op, right) => analyzed::ExpressionValue::BinOp(
                Box::new(convert_expression(left, scopes, procedures)?),
                op.clone(),
                Box::new(convert_expression(right, scopes, procedures)?),
            ),

            UnOp(op, right) => analyzed::ExpressionValue::UnOp(
                op.clone(),
                Box::new(convert_expression(right, scopes, procedures)?),
            ),

            Bracketed(expr) => return convert_expression(expr, scopes, procedures),

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
                    Some(decl) => analyzed::ExpressionValue::Variable(decl),
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
                    if let Some(statement) = convert_statement(s, scopes, procedures)? {
                        if let analyzed::Statement::VariableDeclaration(decl) = &statement {
                            let scope = scopes.last_mut().unwrap();
                            let d = &*(**decl).borrow();
                            scope.insert(d.get_name().to_string(), decl.clone());
                        }
                        statements_converted.push(statement);
                    }
                }
                let l = match last {
                    Some(expr) => Some(Box::new(convert_expression(expr, scopes, procedures)?)),
                    None => None,
                };
                scopes.pop();
                analyzed::ExpressionValue::Block {
                    statements: statements_converted,
                    last: l,
                }
            }

            FunctionCall {
                function,
                arguments,
            } => {
                let procedure = match &**function {
                    Variable(name) => procedures
                        .get(name)
                        .ok_or(AstConversionError::new(format!(
                            "Procedure {} not found!",
                            name
                        )))?
                        .clone(),
                    _ => {
                        return Err(AstConversionError::new(
                            "dynamic function calls are not supported!",
                        ))
                    }
                };
                let args = arguments
                    .iter()
                    .map(|arg| convert_expression(arg, scopes, procedures))
                    .collect::<Result<Vec<_>, _>>()?;
                {
                    let analyzed::Procedure {
                        ref name,
                        arg_count,
                        arg_count_required,
                        ..
                    } = *(*procedure).borrow();
                    if args.len() < (arg_count_required as usize)
                        || args.len() > (arg_count as usize)
                    {
                        return Err(AstConversionError::new(format!(
                            "Wrong amount of arguments supplied! {} requires {}..={} arguments.",
                            name, arg_count_required, arg_count
                        )));
                    }
                }
                analyzed::ExpressionValue::FunctionCall {
                    procedure,
                    arguments: args,
                }
            }

            Assignment(v, val) => match &**v {
                var @ Variable(_) => {
                    let variable = convert_expression(var, scopes, procedures)?;
                    let value = convert_expression(val, scopes, procedures)?;
                    if let analyzed::ExpressionValue::Variable(decl) = variable.value {
                        analyzed::ExpressionValue::Assignment(decl, Box::new(value))
                    } else {
                        panic!("variable was not converted to variable!");
                    }
                }
                _ => todo!(),
            },

            IfElse {
                condition,
                then,
                else_,
            } => analyzed::ExpressionValue::IfElse {
                condition: Box::new(convert_expression(condition, scopes, procedures)?),
                then: Box::new(convert_expression(then, scopes, procedures)?),
                else_: Box::new(convert_expression(else_, scopes, procedures)?),
            },

            Member(_, _) => todo!(),
        },
    })
}

fn convert_statement(
    statement: &ast::Statement,
    scopes: &mut Vec<Scope>,
    procedures: &IndexMap<String, Rc<RefCell<analyzed::Procedure>>>,
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
                        Some(v) => Some(Box::new(convert_expression(&**v, scopes, procedures)?)),
                        None => None,
                    },
                },
                ast::Type::Auto => analyzed::Declaration::UnTyped {
                    name: name.clone(),
                    type_: None,
                    value: match value {
                        Some(v) => Box::new(convert_expression(&**v, scopes, procedures)?),
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
            convert_expression(expr, scopes, procedures)?,
        )),
        If { condition, then } => Some(analyzed::Statement::If {
            condition: Box::new(convert_expression(condition, scopes, procedures)?),
            then: Box::new(convert_expression(then, scopes, procedures)?),
        }),
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
