use parser::ParseResult;

pub mod display;
pub mod program;
mod types;

#[derive(Debug)]
pub struct TypeAnalyzer {
    parsed: ParseResult,
}

#[derive(Debug)]
pub struct TypeResult {
    pub program: program::analyzed::Program,
}

impl TypeAnalyzer {
    pub fn new(parsed: ParseResult) -> Self {
        TypeAnalyzer { parsed }
    }

    pub fn analyze(&self) -> Result<TypeResult, String> {
        // TODO(implement type analysis):
        //   1. Scan the program for type definitions and functions
        //   2. Derive Types of expressions recursively
        //      - for internal types, implicit conversion is possible:
        //        (1.1f + 2) => (1.1f + 2.0f)
        //        (4 + 2l) => (4l + 2l)
        //      - for function calls, the return type of the function is used.
        //        Maybe I will implement function overloads or even C++-like templates?
        //   3. Apply expression type to untyped variable declarations
        //   4. Potentially implicit conversion for
        //      specific variable declarations or function calls
        let mut program = program::convert(&self.parsed).map_err(|e| format!("{}", e))?;
        types::analyze_program(&mut program).or_else::<String, _>(|e| {
            println!("{}", e);
            Ok(())
        })?;
        Ok(TypeResult { program })
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
