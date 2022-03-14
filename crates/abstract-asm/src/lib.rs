//! Responsible for converting the analyzed AST into intermediary language.
//!
//! Intermediary language is comparable to assembly, however,
//! infinite variables may exist.  Which registers to use for what, and when
//! to store variables in memory, is decided based on this intermediary
//! language.

pub mod compiler;
pub mod intermediary_language;
pub use compiler::Compiler;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
