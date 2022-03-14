/// Represents a variable in memory.
///
/// In the intermediary language, it is not yet known which registers
/// will be used for what.  In contrast to in actual assembly, an
/// infinite amount of variables may exist.
#[derive(Debug, Clone)]
pub struct Variable {
    pub id: u64,
    pub size: u64,
}

#[derive(Debug, Clone)]
pub enum Instruction {}


#[derive(Debug)]
pub struct IntermediaryLanguage {
    pub instructions: Vec<Instruction>,
}
