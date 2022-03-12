pub mod display;
mod lexer;
pub mod syntax;

use self::lexer::Lexer;
use self::syntax::SyntaxParser;
use std::path::{Path, PathBuf};

pub type ParseResult = self::syntax::ParseResult;

pub struct CodeParser {
    source_path: PathBuf,
}

impl CodeParser {
    pub fn new<P: AsRef<Path>>(source_path: P) -> CodeParser {
        CodeParser {
            source_path: source_path.as_ref().into(),
        }
    }

    pub fn parse(&self) -> Result<ParseResult, String> {
        let lexed = Lexer::new(&self.source_path).read()?.lex()?;
        // println!("{}", lexed);
        let parsed = SyntaxParser::new()
            .parse(lexed.tokens)
            .map_err(|e| format!("{}", e))?;
        // println!("{}", parsed);
        Ok(parsed)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
