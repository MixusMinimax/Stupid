use super::lexer::{Token, TokenEntry, TokenList};
use duplicate::duplicate_item;
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
        "~" => (Token::Tilde, <TokenEntry>),
        "#" => (Token::Hash, <TokenEntry>),
        "&" => (Token::Amp, <TokenEntry>),
        "&&" => (Token::AmpAmp, <TokenEntry>),
        "|" => (Token::Pipe, <TokenEntry>),
        "||" => (Token::PipePipe, <TokenEntry>),
        "==" => (Token::EqEq, <TokenEntry>),
        "!=" => (Token::NotEq, <TokenEntry>),
        "~=" => (Token::TildeEq, <TokenEntry>),
        ":=" => (Token::ColonEq, <TokenEntry>),
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
        "!" => (Token::Bang, <TokenEntry>),
        "." => (Token::Dot, <TokenEntry>),
        ".." => (Token::DotDot, <TokenEntry>),
        "..." => (Token::DotDotDot, <TokenEntry>),
        "->" => (Token::RArrow, <TokenEntry>),
        "proc" => (Token::Proc, <TokenEntry>),
        "return" => (Token::Return, <TokenEntry>),
        "const" => (Token::Const, <TokenEntry>),
        "let" => (Token::Let, <TokenEntry>),
        "if" => (Token::If, <TokenEntry>),
        "else" => (Token::Else, <TokenEntry>),
        "var" => (Token::Var(<String>), <TokenEntry>),
        "int" => (Token::Integer(<String>), <TokenEntry>),
        "long" => (Token::Long(<String>), <TokenEntry>),
        "float" => (Token::Float(<String>), <TokenEntry>),
        "double" => (Token::Double(<String>), <TokenEntry>),
        "str" => (Token::String(<String>), <TokenEntry>),
        "bool" => (Token::Boolean(<bool>), <TokenEntry>),
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
        "proc" <name:"var"> "(" <args:ArgsDecl> ")" <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Auto, arguments: args?, value: e? })
        })(),

        "proc" <name:"var"> "(" <args:ArgsDecl> ")" "->" <t:"var"> <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Named(t.0), arguments: args?, value: e? })
        })(),

        "proc" <name:"var"> <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Auto, arguments: vec![], value: e? })
        })(),

        "proc" <name:"var"> "->" <t:"var"> <e:Expr> => (||{
            Ok(ast::Proc { name: name.0, return_type: ast::Type::Named(t.0), arguments: vec![], value: e? })
        })(),
    };

    ArgsDecl: Result<Vec<ast::Arg>, ParseError> = {
        => Ok(vec![]),

        <arg:ArgDecl> => (||{
            Ok(vec![arg?])
        })(),

        <mut args:ArgsDecl> "," <arg:ArgDecl> => (||{
            match args {
                Ok(mut args) => {
                    args.push(arg?);
                    Ok(args)
                },
                e => e
            }
        })()
    };

    ArgDecl: Result<ast::Arg, ParseError> = {
        <name:"var"> ":" <t:"var"> => (||{
            Ok(ast::Arg { name: name.0, arg_type: ast::Type::Named(t.0), default: None })
        })(),

        <name:"var"> ":" <t:"var"> ":=" <e:Expr> => (||{
            Ok(ast::Arg { name: name.0, arg_type: ast::Type::Named(t.0), default: Some(Box::new(e?)) })
        })(),

        <name:"var"> ":=" <e:Expr> => (||{
            Ok(ast::Arg { name: name.0, arg_type: ast::Type::Auto, default: Some(Box::new(e?)) })
        })(),
    };

    ExprList: Result<Vec<ast::Expression>, ParseError> = {
        => Ok(vec![]),

        <e:Expr> => (||{
            Ok(vec![e?])
        })(),

        <mut exprs:ExprList> "," <e:Expr> => (||{
            match exprs {
                Ok(mut l) => {
                    l.push(e?);
                    Ok(l)
                },
                e => e
            }
        })(),
    };

    Expr: Result<ast::Expression, ParseError> = {
        "if" <a:Assignment> <b:Assignment> "else" <c:Assignment> => (||{
            Ok(ast::Expression::IfElse{
                condition: Box::new(a?),
                then: Box::new(b?),
                else_: Box::new(c?),
            })
        })(),

        <a:Assignment> => a,
    };

    Assignment: Result<ast::Expression, ParseError> = {
        <l:Assignment> "=" <r:LogicOr> => (||{
            Ok(swap_assignment(l?, r?))
        })(),

        <c:LogicOr> => c,
    };

    LogicOr: Result<ast::Expression, ParseError> = {
        <l:LogicOr> "||" <r:LogicAnd> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Or, Box::new(r?)))
        })(),

        <c:LogicAnd> => c,
    };

    LogicAnd: Result<ast::Expression, ParseError> = {
        <l:LogicAnd> "&&" <r:BitOr> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::And, Box::new(r?)))
        })(),

        <c:BitOr> => c,
    };

    BitOr: Result<ast::Expression, ParseError> = {
        <l:BitOr> "|" <r:BitXor> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::BitOr, Box::new(r?)))
        })(),

        <c:BitXor> => c,
    };

    BitXor: Result<ast::Expression, ParseError> = {
        <l:BitXor> "^" <r:BitAnd> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::BitXor, Box::new(r?)))
        })(),

        <c:BitAnd> => c,
    };

    BitAnd: Result<ast::Expression, ParseError> = {
        <l:BitAnd> "&" <r:Equality> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::BitAnd, Box::new(r?)))
        })(),

        <c:Equality> => c,
    };

    Equality: Result<ast::Expression, ParseError> = {
        <l:Equality> "==" <r:Order> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Equals, Box::new(r?)))
        })(),

        <l:Equality> "!=" <r:Order> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::NotEquals, Box::new(r?)))
        })(),

        <c:Order> => c,
    };

    Order: Result<ast::Expression, ParseError> = {
        <l:Order> ">" <r:Sum> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Greater, Box::new(r?)))
        })(),

        <l:Order> ">=" <r:Sum> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::GreaterEq, Box::new(r?)))
        })(),

        <l:Order> "<" <r:Sum> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Less, Box::new(r?)))
        })(),

        <l:Order> "<=" <r:Sum> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::LessEq, Box::new(r?)))
        })(),

        <c:Sum> => c,
    };

    Sum: Result<ast::Expression, ParseError> = {
        <l:Sum> "+" <r:Product> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Add, Box::new(r?)))
        })(),

        <l:Sum> "-" <r:Product> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Subtract, Box::new(r?)))
        })(),

        <t:Product> => t,
    };

    Product: Result<ast::Expression, ParseError> = {
        <l:Product> "*" <r:Prefix> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Multiply, Box::new(r?)))
        })(),

        <l:Product> "/" <r:Prefix> => (||{
            Ok(ast::Expression::BinOp(Box::new(l?), ast::BinOperator::Divide, Box::new(r?)))
        })(),

        // TODO: Modulo

        <f:Prefix> => f,
    };

    Prefix: Result<ast::Expression, ParseError> = {
        "-" <f:Prefix> => (||{
            Ok(ast::Expression::UnOp(ast::UnOperator::Negate, Box::new(f?)))
        })(),

        "!" <f:Prefix> => (||{
            Ok(ast::Expression::UnOp(ast::UnOperator::Not, Box::new(f?)))
        })(),

        "~" <f:Prefix> => (||{
            Ok(ast::Expression::UnOp(ast::UnOperator::BitNot, Box::new(f?)))
        })(),

        "*" <f:Prefix> => (||{
            Ok(ast::Expression::UnOp(ast::UnOperator::Deref, Box::new(f?)))
        })(),

        <c:Suffix> => c,
    };

    Suffix: Result<ast::Expression, ParseError> = {
        <m:Suffix> "." <name:"var"> => (||{
            Ok(ast::Expression::Member(Box::new(m?), name.0))
        })(),

        <f:Suffix> "(" <args:ExprList> ")" => (||{
            Ok(ast::Expression::FunctionCall{
                function: Box::new(f?),
                arguments: args?,
            })
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

        <var:"var"> => Ok(ast::Expression::Variable(var.0)),

        <b:"bool"> => Ok(ast::Expression::Boolean(b.0)),

        "(" <e:Expr> ")" => (||{
            // This is needed later when flipping assignments around
            Ok(ast::Expression::Bracketed(Box::new(e?)))
        })(),

        "{" <body:BlockBody> "}" => body,
    };

    BlockBody: Result<ast::Expression, ParseError> = {
        => Ok(ast::Expression::Block { statements: vec![], last: None }),

        <mut block:BlockBody> <s:Statement> => (||{
            let statement = s?;
            if let ast::Statement::SemiColon = statement { return block }
            match block {
                Ok(ast::Expression::Block {ref mut statements, last: _}) => {
                    statements.push(statement);
                    block
                },
                e => e
            }
        })(),

        <mut block:BlockBody> <e:Expr> => (||{
            match block {
                Ok(ast::Expression::Block {statements: _, ref mut last}) => {
                    *last = Some(Box::new(e?));
                    block
                },
                e => e
            }
        })(),
    };

    Statement: Result<ast::Statement, ParseError> = {
        ";" => Ok(ast::Statement::SemiColon),

        <e:Expr> ";" => (||{
            Ok(ast::Statement::ExpressionStatement(e?))
        })(),

        "let" <name:"var"> ":" <t:"var"> ";" => (||{
            Ok(ast::Statement::VariableDeclaration {
                name: name.0,
                var_type: ast::Type::Named(t.0),
                value: None,
            })
        })(),

        "let" <name:"var"> ":" <t:"var"> "=" <e:Expr> ";" => (||{
            Ok(ast::Statement::VariableDeclaration {
                name: name.0,
                var_type: ast::Type::Named(t.0),
                value: Some(Box::new(e?)),
            })
        })(),

        "let" <name:"var"> "=" <e:Expr> ";" => (||{
            Ok(ast::Statement::VariableDeclaration {
                name: name.0,
                var_type: ast::Type::Auto,
                value: Some(Box::new(e?)),
            })
        })(),

        "if" <condition:Assignment> <body:Assignment> ";" => (||{
            Ok(ast::Statement::If{
                condition: Box::new(condition?),
                then: Box::new(body?)
            })
        })(), 
    };
}

#[derive(Debug)]
pub struct ParseResult {
    pub root: ast::Root,
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

pub mod ast {

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
        Bracketed(Box<Expression>),
        Block {
            statements: Vec<Statement>,
            last: Option<Box<Expression>>,
        },
        IfElse {
            condition: Box<Expression>,
            then: Box<Expression>,
            else_: Box<Expression>,
        },
        Assignment(Box<Expression>, Box<Expression>),

        Member(Box<Expression>, String),
        FunctionCall {
            function: Box<Expression>,
            // TODO: named arguments
            arguments: Vec<Expression>,
        },

        Integer(i32),
        Long(i64),
        Float(f32),
        Double(f64),
        Boolean(bool),
        Variable(String),

        BinOp(Box<Expression>, BinOperator, Box<Expression>),
        UnOp(UnOperator, Box<Expression>),
    }

    #[derive(Debug, Clone)]
    pub enum BinOperator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Equals,
        NotEquals,
        Greater,
        GreaterEq,
        Less,
        LessEq,
        And,
        Or,
        BitAnd,
        BitOr,
        BitXor,
    }

    #[derive(Debug, Clone)]
    pub enum UnOperator {
        Negate,
        Not,
        BitNot,
        Deref,
    }

    #[derive(Debug, Clone)]
    pub enum Statement {
        SemiColon,
        VariableDeclaration {
            name: String,
            var_type: Type,
            value: Option<Box<Expression>>,
        },
        ExpressionStatement(Expression),
        If {
            condition: Box<Expression>,
            then: Box<Expression>,
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
            .map_err(|err| ParseError::new(format!("{} ({})", err, literal), None))
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

fn swap_assignment(left: ast::Expression, right: ast::Expression) -> ast::Expression {
    match left {
        ast::Expression::Assignment(var, tree) => {
            ast::Expression::Assignment(var, Box::new(swap_assignment(*tree, right)))
        }
        e => ast::Expression::Assignment(Box::new(e), Box::new(right)),
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
        Self {
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
