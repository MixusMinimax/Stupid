use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub enum VariableSize {
    LowByte,
    HighByte,
    Word,
    DoubleWord,
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
pub enum Instruction {}

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

#[derive(Debug)]
pub struct IntermediaryLanguage {
    pub constants: IndexMap<String, Constant>,
    pub functions: IndexMap<String, Function>,
}
