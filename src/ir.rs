use thiserror::Error;

use crate::subs::Substitution;

pub type Opcode = u8;
pub type Operand = i32;
pub type Instruction = u32;

pub const OPCODE_WIDTH: usize = (Opcode::BITS / 8) as usize;

pub const OPERAND_WIDTH: usize = PADDED_OPERAND_WIDTH - OPCODE_WIDTH;

pub const PADDED_OPERAND_WIDTH: usize = (Operand::BITS / 8) as usize;

pub const INSTRUCTION_WIDTH: usize = (Instruction::BITS / 8) as usize;

pub const MAX_OPERAND: Operand = Operand::MAX >> 8;

pub const MIN_OPERAND: Operand = !MAX_OPERAND + 1;

#[derive(Debug, Error)]
pub enum Error {
    #[error("operand {0} is too small")]
    OperandTooSmall(Operand),
    #[error("operand {0} is too big")]
    OperandTooBig(Operand),
    #[error("label {label} is out of bounds 0..={len}")]
    InvalidLabel { label: Operand, len: usize },
    #[error("labels exhausted")]
    LabelsExhausted,
    #[error("substitution exhausted")]
    SubsExhausted,
    #[error("substitution identifier {id} is out of bounds 0..={count}")]
    InvalidSubsId { id: Operand, count: usize },
}

pub fn extract_operand_bytes(
    operand: Operand,
) -> Result<[u8; OPERAND_WIDTH], Error> {
    if operand < MIN_OPERAND {
        Err(Error::OperandTooSmall(operand))?
    }
    if operand > MAX_OPERAND {
        Err(Error::OperandTooBig(operand))?
    }

    let mut operand_bytes = [0; OPERAND_WIDTH];
    operand_bytes.copy_from_slice(&operand.to_le_bytes()[.. OPERAND_WIDTH]);
    Ok(operand_bytes)
}

pub fn pack_instruction(
    opcode: Opcode,
    operand: Operand,
) -> Result<[u8; INSTRUCTION_WIDTH], Error> {
    let opcode_bytes = opcode.to_le_bytes();
    let operand_bytes = extract_operand_bytes(operand)?;
    let mut instr_bytes = [0; INSTRUCTION_WIDTH];
    instr_bytes[.. OPCODE_WIDTH].copy_from_slice(&opcode_bytes);
    instr_bytes[OPCODE_WIDTH ..]
        .copy_from_slice(&operand_bytes[.. OPERAND_WIDTH]);
    Ok(instr_bytes)
}

pub fn encode_instruction(
    opcode: Opcode,
    operand: Operand,
) -> Result<Instruction, Error> {
    let bytes = pack_instruction(opcode, operand)?;
    Ok(Instruction::from_le_bytes(bytes))
}

pub fn set_operand(
    instruction: Instruction,
    operand: Operand,
) -> Result<Instruction, Error> {
    let operand_bytes = extract_operand_bytes(operand)?;
    let mut instr_bytes = instruction.to_le_bytes();
    instr_bytes[OPCODE_WIDTH ..].copy_from_slice(&operand_bytes);
    Ok(Instruction::from_le_bytes(instr_bytes))
}

pub fn decode_instruction(instruction: Instruction) -> (Opcode, Operand) {
    let instr_bytes = instruction.to_le_bytes();
    let mut opcode = [0; OPCODE_WIDTH];
    let mut operand = [0; PADDED_OPERAND_WIDTH];
    opcode.copy_from_slice(&instr_bytes[.. OPCODE_WIDTH]);
    operand[.. OPERAND_WIDTH].copy_from_slice(&instr_bytes[OPCODE_WIDTH ..]);
    let sign = operand[OPCODE_WIDTH - 1] & 0x80;
    if sign != 0 {
        for byte in &mut operand[OPCODE_WIDTH ..] {
            *byte = 0xff;
        }
    }
    (Opcode::from_le_bytes(opcode), Operand::from_le_bytes(operand))
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    instructions: Vec<Instruction>,
    substitutions: Vec<Substitution>,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn substitution_count(&self) -> usize {
        self.substitutions.len()
    }

    pub fn instruction(&self, label: Operand) -> Result<Instruction, Error> {
        self.instructions
            .get(label as usize)
            .copied()
            .ok_or_else(|| Error::InvalidLabel { label, len: self.len() })
    }

    pub fn push(&mut self, instruction: Instruction) -> Result<Operand, Error> {
        let index = self.instructions.len();
        let Ok(label) = Operand::try_from(index) else {
            Err(Error::LabelsExhausted)?
        };
        self.instructions.push(instruction);

        Ok(label)
    }

    pub fn push_encode(
        &mut self,
        opcode: Opcode,
        operand: Operand,
    ) -> Result<Operand, Error> {
        self.push(encode_instruction(opcode, operand)?)
    }

    pub fn set_operand(
        &mut self,
        label: Operand,
        operand: Operand,
    ) -> Result<(), Error> {
        let len = self.len();
        let Ok(index) = usize::try_from(label) else {
            Err(Error::InvalidLabel { label, len })?
        };
        let Some(instruction) = self.instructions.get_mut(index) else {
            Err(Error::InvalidLabel { label, len })?
        };
        *instruction = set_operand(*instruction, operand)?;
        Ok(())
    }

    pub fn get_instruction(
        &self,
        label: Operand,
    ) -> Result<Instruction, Error> {
        let len = self.len();
        let Ok(index) = usize::try_from(label) else {
            Err(Error::InvalidLabel { label, len })?
        };
        self.instructions
            .get(index)
            .copied()
            .ok_or_else(|| Error::InvalidLabel { label, len })
    }

    pub fn create_substitution(
        &mut self,
        substitution: Substitution,
    ) -> Result<Operand, Error> {
        let index = self.substitutions.len();
        let Ok(operand) = Operand::try_from(index) else {
            Err(Error::SubsExhausted)?
        };
        self.substitutions.push(substitution);
        Ok(operand)
    }

    pub fn get_substitution(
        &self,
        id: Operand,
    ) -> Result<&Substitution, Error> {
        let Ok(index) = usize::try_from(id) else {
            Err(Error::InvalidSubsId { id, count: self.substitution_count() })?
        };
        self.substitutions.get(index).ok_or_else(|| Error::InvalidSubsId {
            id,
            count: self.substitution_count(),
        })
    }
}

pub mod opcodes {
    use crate::ir::Opcode;

    pub const NOP: Opcode = tags::set::CORE
        | tags::operands::ONE
        | tags::typ::FLOW
        | tags::typ::flow::which::UNCONDITIONAL
        | tags::typ::flow::direction::NOT;

    pub const JMP: Opcode = tags::set::CORE
        | tags::operands::ONE
        | tags::typ::FLOW
        | tags::typ::flow::which::UNCONDITIONAL
        | tags::typ::flow::direction::POSITIVE;

    pub const JZ: Opcode = tags::set::CORE
        | tags::operands::ONE
        | tags::typ::FLOW
        | tags::typ::flow::which::ZERO
        | tags::typ::flow::direction::POSITIVE;

    pub const JNZ: Opcode = tags::set::CORE
        | tags::operands::ONE
        | tags::typ::FLOW
        | tags::typ::flow::which::ZERO
        | tags::typ::flow::direction::NOT;

    pub const DUPT: Opcode = tags::set::CORE
        | tags::operands::NONE
        | tags::typ::STACK
        | tags::typ::stack::operation::DUP
        | tags::typ::stack::which::TEXT;

    pub const DUPF: Opcode = tags::set::CORE
        | tags::operands::NONE
        | tags::typ::STACK
        | tags::typ::stack::operation::DUP
        | tags::typ::stack::which::FLAG;

    pub const POPT: Opcode = tags::set::CORE
        | tags::operands::NONE
        | tags::typ::STACK
        | tags::typ::stack::operation::POP
        | tags::typ::stack::which::TEXT;

    pub const POPF: Opcode = tags::set::CORE
        | tags::operands::NONE
        | tags::typ::STACK
        | tags::typ::stack::operation::POP
        | tags::typ::stack::which::FLAG;

    pub const SUBS: Opcode = tags::set::CORE
        | tags::operands::NONE
        | tags::typ::EXPRESSION
        | tags::typ::expression::SUBS;

    pub const NOT: Opcode = tags::set::CORE
        | tags::operands::NONE
        | tags::typ::EXPRESSION
        | tags::typ::expression::NOT;

    pub mod tags {
        use crate::ir::Opcode;

        pub const SET: Opcode = 0b_1000_0000;
        pub const OPERAND: Opcode = 0b_0100_0000;
        pub const TYP: Opcode = 0b_0011_0000;

        pub mod set {
            use crate::ir::Opcode;

            pub const CORE: Opcode = 0b_0000_0000;
            pub const EXTENSION: Opcode = 0b_1000_0000;
        }

        pub mod operands {
            use crate::ir::Opcode;

            pub const ONE: Opcode = 0b_0000_0000;
            pub const NONE: Opcode = 0b_0100_0000;
        }

        pub mod typ {
            use crate::ir::Opcode;

            pub const FLOW: Opcode = 0b_0000_0000;
            pub const STACK: Opcode = 0b_0001_0000;
            pub const EXPRESSION: Opcode = 0b_0010_0000;

            pub mod flow {
                use crate::ir::Opcode;

                pub const WHICH: Opcode = 0b_0000_1100;
                pub const DIRECTION: Opcode = 0b_0000_0011;

                pub mod which {
                    use crate::ir::Opcode;

                    pub const UNCONDITIONAL: Opcode = 0b_0000_0000;
                    pub const ZERO: Opcode = 0b_0000_0010;
                }

                pub mod direction {
                    use crate::ir::Opcode;

                    pub const NOT: Opcode = 0b_0000_0000;
                    pub const POSITIVE: Opcode = 0b_0000_0001;
                }
            }

            pub mod stack {
                use crate::ir::Opcode;

                pub const OPERATION: Opcode = 0b_0000_1100;
                pub const WHICH: Opcode = 0b_0000_0011;

                pub mod operation {
                    use crate::ir::Opcode;

                    pub const DUP: Opcode = 0b_0000_0000;
                    pub const POP: Opcode = 0b_0000_0100;
                }

                pub mod which {
                    use crate::ir::Opcode;

                    pub const FLAG: Opcode = 0b_0000_0000;
                    pub const TEXT: Opcode = 0b_0000_0001;
                }
            }

            pub mod expression {
                use crate::ir::Opcode;

                pub const SUBS: Opcode = 0b_0010_0000;
                pub const NOT: Opcode = 0b_0010_0001;
            }
        }
    }
}
