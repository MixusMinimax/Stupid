use std::{fmt::Display, iter::Inspect};

use crate::intermediary_language::{Constant, Function, Instruction, IntermediaryLanguage};

impl Display for IntermediaryLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#############\n")?;
        write!(f, "# Constants #\n")?;
        write!(f, "#############\n")?;
        for constant in self.constants.values() {
            write!(f, "{}", constant)?;
        }

        write!(f, "\n")?;
        write!(f, "#############\n")?;
        write!(f, "# Functions #\n")?;
        write!(f, "#############\n")?;

        for function in self.functions.values() {
            write!(f, "{}", function)?;
        }
        Ok(())
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n# {}:\n", self.name)?;
        display_instructions(&self.init, f)?;
        Ok(())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n# {}:\n", self.name)?;
        display_instructions(&self.instructions, f)?;
        Ok(())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            Label(name) => write!(f, "{}:", name)?,
            Return => write!(f, "  return")?,
            _ => write!(f, "  # Not Implemented!")?,
        };
        Ok(())
    }
}

fn display_instructions(
    instructions: &Vec<Instruction>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    for instruction in instructions.iter() {
        write!(f, "{}\n", instruction)?;
    }
    return Ok(());
}
