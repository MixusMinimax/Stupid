use indexmap::IndexMap;
use strum_macros::{self, Display};

#[derive(Debug, Clone, strum_macros::Display)]
pub enum VariableSize {
    LowByte,
    HighByte,
    Word,
    DoubleWord,
    Long,
}

/// Represents a variable in memory.
///
/// In the intermediary language, it is not yet known which registers
/// will be used for what.  In contrast to in actual assembly, an
/// infinite amount of variables may exist.
#[derive(Debug, Clone)]
pub struct Variable {
    pub id: u64,
    pub size: VariableSize,
}

#[derive(Debug, Clone)]
pub enum Immediate {
    Integer { size: VariableSize, value: u64 },
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Label(String),
    Return(Variable),
    AssignVariable(Variable, Variable),
    AssignImmediate(Variable, Immediate),
}

#[derive(Debug)]
pub struct Constant {
    pub name: String,
    pub init: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

impl Function {
    pub fn new<S: AsRef<str>>(name: S) -> Self {
        Function {
            name: name.as_ref().to_string(),
            instructions: vec![],
        }
    }
}

#[derive(Debug)]
pub struct IntermediaryLanguage {
    pub constants: IndexMap<String, Constant>,
    pub functions: IndexMap<String, Function>,
}
