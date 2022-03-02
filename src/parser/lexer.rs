use lexgen::lexer;
use lexgen_util::Loc;
use std::{fmt::Write, fs::File, io::Read, path::Path};

pub type TokenList = Vec<TokenEntry>;

#[derive(Debug, Clone, strum_macros::Display)]
pub enum Token {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Hash,
    EqEq,
    TildeEq,
    LtEq,
    GtEq,
    Lt,
    Gt,
    Eq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    RBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotDotDot,
    RArrow,
    Proc,
    Return,
    Const,
    Let,
    Var(String),
    Integer(String),
    Long(String),
    Float(String),
    Double(String),
    String(String),
}

#[derive(Debug, Clone)]
pub struct TokenEntry(pub Loc, pub Token, pub Loc);

#[derive(Debug, Default, Clone)]
struct LexerState {
    string_buf: String,
}

lexer! {
    LexerImpl(LexerState) -> Token;

    let whitespace = [' ' '\t' '\n'] | "\r\n";

    let var_init = ['a'-'z' 'A'-'Z' '_'];
    let var_subseq = $var_init | ['0'-'9'];

    let digit = ['0'-'9'];
    let hex_digit = ['a'-'f' 'A'-'F' '0'-'9'];

    rule Init {
        $whitespace,

        "+" = Token::Plus,
        "-" = Token::Minus,
        "*" = Token::Star,
        "/" = Token::Slash,
        "%" = Token::Percent,
        "^" = Token::Caret,
        "#" = Token::Hash,
        "==" = Token::EqEq,
        "~=" = Token::TildeEq,
        "<=" = Token::LtEq,
        ">=" = Token::GtEq,
        "<" = Token::Lt,
        ">" = Token::Gt,
        "=" = Token::Eq,
        "(" = Token::LParen,
        ")" = Token::RParen,
        "{" = Token::LBrace,
        "}" = Token::RBrace,
        "]" = Token::RBracket,
        ";" = Token::Semicolon,
        ":" = Token::Colon,
        "," = Token::Comma,
        "." = Token::Dot,
        ".." = Token::DotDot,
        "..." = Token::DotDotDot,
        "->" = Token::RArrow,
        "proc" = Token::Proc,
        "return" = Token::Return,
        "const" = Token::Const,
        "let" = Token::Let,

        $var_init $var_subseq* => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Var(match_.to_string()))
        },

        $digit+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Integer(match_.to_string()))
        },

        $digit+ ('l' | 'L') => |lexer| {
            let match_ = lexer.match_();
            let mut s = match_.to_string();
            s.pop();
            lexer.return_(Token::Long(s))
        },

        $digit+ ('f' | 'F') => |lexer| {
            let match_ = lexer.match_();
            let mut s = match_.to_string();
            s.pop();
            lexer.return_(Token::Float(s))
        },

        $digit+ ('d' | 'D') => |lexer| {
            let match_ = lexer.match_();
            let mut s = match_.to_string();
            s.pop();
            lexer.return_(Token::Double(s))
        },

        "0x" $hex_digit+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Integer(match_.to_string()))
        },

        "0x" $hex_digit+ ('l' | 'L') => |lexer| {
            let match_ = lexer.match_();
            let mut s = match_.to_string();
            s.pop();
            lexer.return_(Token::Long(s))
        },

        "0b" ('0' | '1')+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Integer(match_.to_string()))
        },

        "0b" ('0' | '1')+ ('l' | 'L') => |lexer| {
            let match_ = lexer.match_();
            let mut s = match_.to_string();
            s.pop();
            lexer.return_(Token::Long(s))
        },

        ($digit+ ('.' $digit*)? | '.' $digit+) ('e' | 'E') ('+'|'-')? $digit+ ('f' | 'F')? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Float(match_.to_string()))
        },

        ($digit+ ('.' $digit*)? | '.' $digit+) ('e' | 'E') ('+'|'-')? $digit+ ('d' | 'D') => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Double(match_.to_string()))
        },

        ($digit+ '.' $digit* | $digit* '.' $digit+) ('f' | 'F')? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Float(match_.to_string()))
        },

        ($digit+ '.' $digit* | $digit* '.' $digit+) ('d' | 'D') => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Double(match_.to_string()))
        },

        '"' => |lexer| {
            lexer.state().string_buf.clear();
            lexer.switch(LexerImplRule::String)
        },

        "//" => |lexer| lexer.switch(LexerImplRule::Comment),

        "/*" => |lexer| lexer.switch(LexerImplRule::BlockComment),
    }

    rule String {
        '"' => |lexer| {
            let str = lexer.state().string_buf.clone();
            lexer.switch_and_return(LexerImplRule::Init, Token::String(str))
        },

        "\\a" => |lexer| {
            lexer.state().string_buf.push('\u{7}');
            lexer.continue_()
        },

        "\\b" => |lexer| {
            lexer.state().string_buf.push('\u{8}');
            lexer.continue_()
        },

        "\\f" => |lexer| {
            lexer.state().string_buf.push('\u{c}');
            lexer.continue_()
        },

        "\\n" => |lexer| {
            lexer.state().string_buf.push('\n');
            lexer.continue_()
        },

        "\\r" => |lexer| {
            lexer.state().string_buf.push('\r');
            lexer.continue_()
        },

        "\\t" => |lexer| {
            lexer.state().string_buf.push('\t');
            lexer.continue_()
        },

        "\\v" => |lexer| {
            lexer.state().string_buf.push('\u{b}');
            lexer.continue_()
        },

        "\\\\" => |lexer| {
            lexer.state().string_buf.push('\\');
            lexer.continue_()
        },

        "\\\"" => |lexer| {
            lexer.state().string_buf.push('"');
            lexer.continue_()
        },

        "\\'" => |lexer| {
            lexer.state().string_buf.push('\'');
            lexer.continue_()
        },

        "\\\n" => |lexer| {
            lexer.state().string_buf.push('\n');
            lexer.continue_()
        },

        _ => |lexer| {
            let char = lexer.match_().chars().next_back().unwrap();
            lexer.state().string_buf.push(char);
            lexer.continue_()
        },
    }

    rule Comment {
        "\r\n" | '\n' => |lexer| lexer.switch(LexerImplRule::Init),

        _ => |lexer| lexer.continue_(),
    }

    rule BlockComment {
        "*/" => |lexer| lexer.switch(LexerImplRule::Init),

        _ => |lexer| lexer.continue_(),
    }
}

#[derive(Debug)]
pub struct LexResult {
    pub tokens: TokenList,
}

impl LexResult {
    pub fn new(tokens: TokenList) -> Self {
        LexResult { tokens }
    }
}

pub struct Lexer<'path> {
    source_path: &'path Path,
    content: Option<String>,
}

impl<'path> Lexer<'path> {
    pub fn new(source_path: &'path Path) -> Self {
        Lexer {
            source_path,
            content: None,
        }
    }

    pub fn read(mut self) -> Result<Self, String> {
        match File::open(self.source_path) {
            Ok(ref mut file) => {
                let mut content = String::new();
                file.read_to_string(&mut content)
                    .map_err(|err| format!("{}", err))?;
                self.content = Some(content);
                Ok(self)
            }
            Err(reason) => Err(format!("{}", reason)),
        }
    }

    pub fn lex(&self) -> Result<LexResult, String> {
        if let Some(ref content) = self.content {
            let lexer = LexerImpl::new(content);
            let mut tokens = Vec::new();
            for token in lexer {
                match token {
                    Ok((start, token, end)) => {
                        tokens.push(TokenEntry(start, token, end));
                    }
                    Err(reason) => {
                        let c: char = content.as_bytes()[reason.location.byte_idx].into();
                        return Err(format!("{:?}, char: '{}'", reason, c));
                    }
                }
            }
            Ok(LexResult::new(tokens))
        } else {
            Err("File has not been read!".to_string())
        }
    }
}

// Display

impl std::fmt::Display for TokenEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TokenEntry(ref start, ref token, ref end) = self;
        f.write_char('(')?;
        fmt_token(&token, f)?;
        f.write_str(", ")?;
        fmt_loc(&start, f)?;
        f.write_char('-')?;
        fmt_loc(&end, f)?;
        f.write_char(')')?;
        Ok(())
    }
}

impl std::fmt::Display for LexResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(_) = f.precision() {
            return reconstruct(&self.tokens, f);
        }
        let mut is_first = true;
        for token in self.tokens.iter() {
            if is_first {
                is_first = false;
            } else {
                f.write_str(", ")?;
            }
            f.write_fmt(format_args!("{}", token))?;
        }
        Ok(())
    }
}

fn reconstruct(tokens: &TokenList, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut is_first = true;
    for TokenEntry(_, token, _) in tokens {
        if is_first {
            is_first = false;
        } else {
            f.write_char(' ')?;
        }
        match token {
            Token::Plus => f.write_char('+'),
            Token::Minus => f.write_char('-'),
            Token::Star => f.write_char('*'),
            Token::Slash => f.write_char('/'),
            Token::Percent => f.write_char('%'),
            Token::Caret => f.write_char('^'),
            Token::Hash => f.write_char('#'),
            Token::EqEq => f.write_str("=="),
            Token::TildeEq => f.write_str("~="),
            Token::LtEq => f.write_str("<="),
            Token::GtEq => f.write_str(">="),
            Token::Lt => f.write_char('<'),
            Token::Gt => f.write_char('>'),
            Token::Eq => f.write_char('='),
            Token::LParen => f.write_char('('),
            Token::RParen => f.write_char(')'),
            Token::LBrace => f.write_char('{'),
            Token::RBrace => f.write_char('}'),
            Token::RBracket => f.write_char(']'),
            Token::Semicolon => f.write_char(';'),
            Token::Colon => f.write_char(':'),
            Token::Comma => f.write_char(','),
            Token::Dot => f.write_char('.'),
            Token::DotDot => f.write_str(".."),
            Token::DotDotDot => f.write_str("..."),
            Token::RArrow => f.write_str("->"),
            Token::Proc => f.write_str("proc"),
            Token::Return => f.write_str("return"),
            Token::Const => f.write_str("const"),
            Token::Let => f.write_str("let"),
            Token::Var(s)
            | Token::Integer(s)
            | Token::Long(s)
            | Token::Float(s)
            | Token::Double(s) => f.write_str(s),
            Token::String(s) => f.write_fmt(format_args!("{:?}", s.as_str())),
        }?;
    }
    Ok(())
}

fn fmt_loc(loc: &Loc, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("{}:{}", loc.line, loc.col))
}

fn fmt_token(token: &Token, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match token {
        Token::Var(s)
        | Token::Integer(s)
        | Token::Long(s)
        | Token::Float(s)
        | Token::Double(s)
        | Token::String(s) => f.write_fmt(format_args!("{}{{{}}}", token, s)),
        _ => f.write_fmt(format_args!("{}", token)),
    }
}
