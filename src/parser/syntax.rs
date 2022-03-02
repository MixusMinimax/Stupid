use super::lexer::{Token, TokenEntry, TokenList};
use duplicate::duplicate_item;
use parsegen::parser;
use std::{fmt::Display, str::FromStr};

type TokenAndEntry = (Token, TokenEntry);

parser! {
    enum TokenAndEntry {
        "+" => (Token::Plus, <TokenEntry>),
        "-" => (Token::Minus, <TokenEntry>),
        "*" => (Token::Star, <TokenEntry>),
        "/" => (Token::Slash, <TokenEntry>),
        "%" => (Token::Percent, <TokenEntry>),
        "^" => (Token::Caret, <TokenEntry>),
        "#" => (Token::Hash, <TokenEntry>),
        "==" => (Token::EqEq, <TokenEntry>),
        "~=" => (Token::TildeEq, <TokenEntry>),
        "<=" => (Token::LtEq, <TokenEntry>),
        ">=" => (Token::GtEq, <TokenEntry>),
        "<" => (Token::Lt, <TokenEntry>),
        ">" => (Token::Gt, <TokenEntry>),
        "=" => (Token::Eq, <TokenEntry>),
        "(" => (Token::LParen, <TokenEntry>),
        ")" => (Token::RParen, <TokenEntry>),
        "{" => (Token::LBrace, <TokenEntry>),
        "}" => (Token::RBrace, <TokenEntry>),
        "]" => (Token::RBracket, <TokenEntry>),
        ";" => (Token::Semicolon, <TokenEntry>),
        ":" => (Token::Colon, <TokenEntry>),
        "," => (Token::Comma, <TokenEntry>),
        "." => (Token::Dot, <TokenEntry>),
        ".." => (Token::DotDot, <TokenEntry>),
        "..." => (Token::DotDotDot, <TokenEntry>),
        "->" => (Token::RArrow, <TokenEntry>),
        "proc" => (Token::Proc, <TokenEntry>),
        "return" => (Token::Return, <TokenEntry>),
        "const" => (Token::Const, <TokenEntry>),
        "let" => (Token::Let, <TokenEntry>),
        "var" => (Token::Var(<String>), <TokenEntry>),
        "int" => (Token::Integer(<String>), <TokenEntry>),
        "long" => (Token::Long(<String>), <TokenEntry>),
        "float" => (Token::Float(<String>), <TokenEntry>),
        "double" => (Token::Double(<String>), <TokenEntry>),
        "str" => (Token::String(<String>), <TokenEntry>),
    }

    pub Root: Result<ast::Root, ParseError> = {
        => Ok(ast::Root { constants: vec![], functions: vec![] }),

        <mut r:Root> <c:Const> => (||{
            match r {
                Ok(mut root) => {
                    root.constants.push(c?);
                    Ok(root)
                },
                e => e
            }
        })(),

        <mut r:Root> <p:Proc> => (||{
            match r {
                Ok(mut root) => {
                    root.functions.push(p?);
                    Ok(root)
                },
                e => e
            }
        })(),
    };

    Const: Result<ast::Const, ParseError> = {
        "const" <name:"var"> "=" <e:Expr> ";" => (||{
            Ok(ast::Const { name: name.0, value: e?, var_type: ast::Type::Auto })
        })(),
        "const" <name:"var"> ":" <t:"var"> "=" <e:Expr> ";" => (||{
            Ok(ast::Const { name: name.0, value: e?, var_type: ast::Type::Named(t.0) })
        })(),
    };

    Proc: Result<ast::Proc, ParseError> = {
        "proc" <name:"var"> "(" <args:Args> ")" <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Auto, arguments: args?, value: e? })
        })(),

        "proc" <name:"var"> "(" <args:Args> ")" "->" <t:"var"> <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Named(t.0), arguments: args?, value: e? })
        })(),

        "proc" <name:"var"> <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Auto, arguments: vec![], value: e? })
        })(),

        "proc" <name:"var"> "->" <t:"var"> <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Named(t.0), arguments: vec![], value: e? })
        })(),
    };

    Args: Result<Vec<ast::Arg>, ParseError> = {
        => Ok(vec![]),

        <arg:Arg> => (||{
            Ok(vec![arg?])
        })(),

        <mut args:Args> "," <arg:Arg> => (||{
            match args {
                Ok(mut args) => {
                    args.push(arg?);
                    Ok(args)
                },
                e => e
            }
        })()
    };

    Arg: Result<ast::Arg, ParseError> = {
        <name:"var"> ":" <t:"var"> => (||{
            Ok(ast::Arg { name: name.0, arg_type: ast::Type::Named(t.0), default: None })
        })(),

        <name:"var"> ":" <t:"var"> "=" <e:Expr> => (||{
            Ok(ast::Arg { name: name.0, arg_type: ast::Type::Named(t.0), default: Some(Box::new(e?)) })
        })(),

        <name:"var"> "=" <e:Expr> => (||{
            Ok(ast::Arg { name: name.0, arg_type: ast::Type::Auto, default: Some(Box::new(e?)) })
        })(),
    };

    Expr: Result<ast::Expression, ParseError> = {
        <l:Expr> "+" <r:Term> => (||{
            Ok(ast::Expression::Sum(Box::new(l?), Box::new(r?)))
        })(),

        <l:Expr> "-" <r:Term> => (||{
            Ok(ast::Expression::Difference(Box::new(l?), Box::new(r?)))
        })(),

        <t:Term> => t,
    };

    Term: Result<ast::Expression, ParseError> = {
        <l:Term> "*" <r:Fac> => (||{
            Ok(ast::Expression::Product(Box::new(l?), Box::new(r?)))
        })(),

        <l:Term> "/" <r:Fac> => (||{
            Ok(ast::Expression::Quotient(Box::new(l?), Box::new(r?)))
        })(),

        <f:Fac> => f,
    };

    Fac: Result<ast::Expression, ParseError> = {
        <n:"int"> => (||{
            Ok(ast::Expression::Integer(Parsable::parse(&n.0)?))
        })(),

        <n:"long"> => (||{
            Ok(ast::Expression::Long(Parsable::parse(&n.0)?))
        })(),

        <n:"float"> => (||{
            Ok(ast::Expression::Float(Parsable::parse(&n.0)?))
        })(),

        <n:"double"> => (||{
            Ok(ast::Expression::Double(Parsable::parse(&n.0)?))
        })(),

        <var:"var"> => (||{
            Ok(ast::Expression::Variable(var.0))
        })(),

        "(" <e:Expr> ")" => e,

        "-" <f:Fac> => (||{
            Ok(ast::Expression::Negate(Box::new(f?)))
        })(),

        "{" <body:BlockBody> "}" => body,
    };

    BlockBody: Result<ast::Expression, ParseError> = {
        => Ok(ast::Expression::Block { statements: vec![], last: None }),

        <mut block:BlockBody> <s:Statement> => (||{
            match block {
                Ok(ast::Expression::Block {ref mut statements, last: _}) => {
                    statements.push(s?);
                    block
                },
                e => e
            }
        })(),

        <mut block:BlockBody> <e:Expr2> => (||{
            match block {
                Ok(ast::Expression::Block {statements: _, ref mut last}) => {
                    *last = Some(Box::new(e?));
                    block
                },
                e => e
            }
        })(),
    };

    Expr2: Result<ast::Expression, ParseError> = {
        <l:Expr2> "+" <r:Term> => (||{
            Ok(ast::Expression::Sum(Box::new(l?), Box::new(r?)))
        })(),

        <l:Expr2> "-" <r:Term> => (||{
            Ok(ast::Expression::Difference(Box::new(l?), Box::new(r?)))
        })(),

        <t:Term> => t,
    };

    Statement: Result<ast::Statement, ParseError> = {
        <e:Expr2> ";" => (||{
            Ok(ast::Statement::ExpressionStatement(e?))
        })(),

        <name:"var"> "=" <e:Expr2> ";" => (||{
            Ok(ast::Statement::Assignment { name: name.0, value: Box::new(e?) })
        })(),

        "let" <name:"var"> ":" <t:"var"> ";" => (||{
            Ok(ast::Statement::VariableDeclaration {
                name: name.0,
                var_type: ast::Type::Named(t.0),
                value: None,
            })
        })(),

        "let" <name:"var"> ":" <t:"var"> "=" <e:Expr3> ";" => (||{
            Ok(ast::Statement::VariableDeclaration {
                name: name.0,
                var_type: ast::Type::Named(t.0),
                value: Some(Box::new(e?)),
            })
        })(),

        "let" <name:"var"> "=" <e:Expr3> ";" => (||{
            Ok(ast::Statement::VariableDeclaration {
                name: name.0,
                var_type: ast::Type::Auto,
                value: Some(Box::new(e?)),
            })
        })(),
    };

    Expr3: Result<ast::Expression, ParseError> = {
        <l:Expr3> "+" <r:Term> => (||{
            Ok(ast::Expression::Sum(Box::new(l?), Box::new(r?)))
        })(),

        <l:Expr3> "-" <r:Term> => (||{
            Ok(ast::Expression::Difference(Box::new(l?), Box::new(r?)))
        })(),

        <t:Term> => t,
    };
}

#[derive(Debug)]
pub struct ParseResult {
    root: ast::Root,
}

pub struct SyntaxParser {}

impl SyntaxParser {
    pub fn new() -> Self {
        SyntaxParser {}
    }

    pub fn parse(&mut self, tokens: TokenList) -> Result<ParseResult, ParseError> {
        let root = Root::parse(tokens.iter().map(|entry @ TokenEntry(_, token, _)| {
            Ok::<TokenAndEntry, ParseError>((token.clone(), entry.clone()))
        }))
        .map_err(|err| ParseError::new(format!("{:?}", err), None))?;
        Ok(ParseResult { root: root? })
    }
}

enum State {
    Root,
}

mod ast {

    #[derive(Debug, Clone)]
    pub struct Root {
        pub constants: Vec<Const>,
        pub functions: Vec<Proc>,
    }

    #[derive(Debug, Clone)]
    pub struct Const {
        pub var_type: Type,
        pub name: String,
        pub value: Expression,
    }

    #[derive(Debug, Clone)]
    pub struct Proc {
        pub name: String,
        pub arguments: Vec<Arg>,
        pub return_type: Type,
        pub value: Expression,
    }

    #[derive(Debug, Clone)]
    pub enum Type {
        Auto,
        Named(String),
    }

    #[derive(Debug, Clone)]
    pub struct Arg {
        pub arg_type: Type,
        pub name: String,
        pub default: Option<Box<Expression>>,
    }

    #[derive(Debug, Clone)]
    pub enum Expression {
        Block {
            statements: Vec<Statement>,
            last: Option<Box<Expression>>,
        },
        Integer(i32),
        Long(i64),
        Float(f32),
        Double(f64),
        Variable(String),
        Sum(Box<Expression>, Box<Expression>),
        Difference(Box<Expression>, Box<Expression>),
        Product(Box<Expression>, Box<Expression>),
        Quotient(Box<Expression>, Box<Expression>),
        Negate(Box<Expression>),
    }

    #[derive(Debug, Clone)]
    pub enum Statement {
        VariableDeclaration {
            name: String,
            var_type: Type,
            value: Option<Box<Expression>>,
        },
        ExpressionStatement(Expression),
        // I would like for assignments to be expressions, but that isn't possible with a CLR parser.
        // This is because we need to see the assignment operator before knowing what's on the left of it.
        // In other words, assignments are right-associative: a = b = c <=> a = (b = c)
        // Once ParseGen has support for that, this will be changed.
        Assignment {
            name: String,
            value: Box<Expression>,
        },
    }
}

trait Parsable {
    fn parse(literal: &String) -> Result<Self, ParseError>
    where
        Self: Sized;
}

#[duplicate_item(T; [f32]; [f64])]
impl Parsable for T {
    fn parse(literal: &String) -> Result<Self, ParseError> {
        literal
            .parse()
            .map_err(|err| ParseError::new(format!("{}", err), None))
    }
}

#[duplicate_item(T; [i32]; [i64])]
impl Parsable for T {
    fn parse(literal: &String) -> Result<Self, ParseError> {
        if literal.starts_with("0x") {
            T::from_str_radix(&literal.as_str()[2..], 16)
                .map_err(|err| ParseError::new(format!("{}", err), None))
        } else if literal.starts_with("0b") {
            T::from_str_radix(&literal.as_str()[2..], 2)
                .map_err(|err| ParseError::new(format!("{}", err), None))
        } else {
            literal
                .parse()
                .map_err(|err| ParseError::new(format!("{}", err), None))
        }
    }
}

// Errors

#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    token: Option<TokenEntry>,
}

impl ParseError {
    pub fn new<S: AsRef<str>>(message: S, token: Option<TokenEntry>) -> Self {
        ParseError {
            message: message.as_ref().to_string(),
            token,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("ParseError: {}", self.message))?;
        if let Some(token) = self.token.clone() {
            f.write_fmt(format_args!("at {}", token))?
        }
        Ok(())
    }
}
