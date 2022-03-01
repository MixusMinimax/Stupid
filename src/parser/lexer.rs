use lexgen::lexer;
use lexgen_util::Loc;
use std::{fs::File, io::Read, path::Path};

lexer! {
    LexerImpl -> Token;

    rule Init {

    }
}

#[derive(Debug)]
pub enum Token {
    String(String),
}

#[derive(Debug)]
pub struct LexResult {
    pub tokens: Vec<(Loc, Token, Loc)>,
}

impl LexResult {
    pub fn new(tokens: Vec<(Loc, Token, Loc)>) -> Self {
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
                        tokens.push((start, token, end));
                    }
                    Err(reason) => return Err(format!("{:?}", reason)),
                }
            }
            Ok(LexResult::new(tokens))
        } else {
            Err("File has not been read!".to_string())
        }
    }
}
