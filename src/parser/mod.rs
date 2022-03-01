mod lexer;

use self::lexer::{LexResult, Lexer};
use std::path::{Path, PathBuf};

pub struct CodeParser {
    source_path: PathBuf,
}

#[derive(Default, Debug)]
pub struct ParseResult {}

impl CodeParser {
    pub fn new<P: AsRef<Path>>(source_path: P) -> CodeParser {
        CodeParser {
            source_path: source_path.as_ref().into(),
        }
    }

    pub fn parse(&self) -> Result<ParseResult, String> {
        Lexer::new(&self.source_path)
            .read()
            .and_then(|lexer| lexer.lex())
            .map(|result| {
                println!("{:?}", result);
                result
            })
            .and_then(|result| self.to_ast(result))
    }

    fn to_ast(&self, lex_result: LexResult) -> Result<ParseResult, String> {
        Ok(ParseResult::default())
    }
}
