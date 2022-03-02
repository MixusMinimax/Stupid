use super::lexer::{Token, TokenEntry, TokenList};
use parsegen::parser;
use std::fmt::Display;

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
        <n:"int"> => (||{
            Ok(ast::Expression::Integer(n.0.parse().map_err(|err| ParseError::new(format!("{}", err), Some(n.1)))?))
        })(),
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

    #[derive(Debug)]
    pub struct Root {
        pub constants: Vec<Const>,
        pub functions: Vec<Proc>,
    }

    #[derive(Debug)]
    pub struct Const {
        pub var_type: Type,
        pub name: String,
        pub value: Expression,
    }

    #[derive(Debug)]
    pub struct Proc {
        pub name: String,
        pub arguments: Vec<Arg>,
        pub return_type: Type,
        pub value: Expression,
    }

    #[derive(Debug)]
    pub enum Type {
        Auto,
        Named(String),
    }

    #[derive(Debug)]
    pub struct Arg {
        pub arg_type: Type,
        pub name: String,
        pub default: Option<Box<Expression>>,
    }

    #[derive(Debug)]
    pub enum Expression {
        Block {
            statements: Vec<Statement>,
            last: Option<Box<Expression>>,
        },
        Assignment {
            variable_name: String,
            value: Box<Expression>,
        },
        Integer(i32),
    }

    #[derive(Debug)]
    enum Statement {
        VariableDeclaration {
            name: String,
            var_type: Type,
            value: Option<Box<Expression>>,
        },
        ExpressionStatement(Expression),
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
