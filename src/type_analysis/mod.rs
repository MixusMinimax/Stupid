use super::parser::ParseResult;

#[derive(Debug)]
pub struct TypeAnalyzer {
    parsed: ParseResult,
}

#[derive(Debug)]
pub struct TypeResult {}

#[derive(Debug)]
pub struct TypeError {}

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
        Ok(TypeResult {})
    }
}
