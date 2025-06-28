use std::{collections::HashSet, fmt};

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

    pub fn past_last_label(&self) -> Operand {
        Operand::try_from(self.len()).unwrap_or(Operand::MIN)
    }

    pub fn instruction(&self, label: Operand) -> Result<Instruction, Error> {
        self.instructions
            .get(label as usize)
            .copied()
            .ok_or_else(|| Error::InvalidLabel { label, len: self.len() })
    }

    pub fn push(&mut self, instruction: Instruction) -> Result<Operand, Error> {
        let label = self.next_label()?;
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

    pub fn instructions(&self) -> impl Iterator<Item = (Operand, Instruction)> {
        self.instructions
            .iter()
            .zip(0 ..)
            .map(|(instruction, i)| (i, *instruction))
    }
    pub fn substitutions(
        &self,
    ) -> impl Iterator<Item = (Operand, &Substitution)> {
        self.substitutions.iter().zip(0 ..).map(|(subs, i)| (i, subs))
    }

    fn next_label(&self) -> Result<Operand, Error> {
        let last = self.past_last_label();
        if last < 0 {
            Err(Error::LabelsExhausted)?
        }
        Ok(last)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Emit<'a> {
    pub program: &'a Program,
    pub expand_subs: bool,
}

impl<'a> fmt::Display for Emit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.expand_subs {
            let mut empty = true;
            for (id, subs) in self.program.substitutions() {
                write!(f, "#{id}:\n")?;
                write!(f, "  {subs}\n")?;
                empty = false;
            }
            if !empty {
                write!(f, "\n")?;
            }
        }

        let mut labels = HashSet::new();
        for (label, instruction) in self.program.instructions() {
            let (opcode, operand) = decode_instruction(instruction);
            let dest = match opcode {
                opcodes::JMP | opcodes::JZ | opcodes::JNZ => {
                    label + 1 + operand
                },
                _ => continue,
            };
            labels.insert(dest);
        }

        for (label, instruction) in self.program.instructions() {
            if labels.contains(&label) || label == 0 {
                write!(f, "L_{}:\n", label)?;
            }
            write!(f, "  ")?;
            let (opcode, operand) = decode_instruction(instruction);
            match opcode {
                opcodes::NOP => write!(f, "nop")?,
                opcodes::JMP => write!(f, "jmp  L_{}", label + 1 + operand)?,
                opcodes::JZ => write!(f, "jz   L_{}", label + 1 + operand)?,
                opcodes::JNZ => write!(f, "jnz  L_{}", label + 1 + operand)?,
                opcodes::DUP => write!(f, "dup")?,
                opcodes::POP => write!(f, "pop")?,
                opcodes::SWAP => write!(f, "swap")?,
                opcodes::SUBS => {
                    if self.expand_subs {
                        match self.program.get_substitution(operand) {
                            Ok(subs) => write!(f, "subs {}", subs)?,
                            Err(_) => write!(f, "subs ??")?,
                        }
                    } else {
                        write!(f, "subs #{}", operand)?;
                    }
                },
                opcodes::NOT => write!(f, "not")?,
                _ => write!(f, "??")?,
            }
            write!(f, "\n")?;
        }
        if labels.contains(&self.program.past_last_label()) {
            write!(f, "L_{}:\n", self.program.past_last_label())?;
        }
        Ok(())
    }
}

pub mod opcodes {
    use crate::ir::Opcode;

    pub const NOP: Opcode = 0;

    pub const JMP: Opcode = 1;

    pub const JZ: Opcode = 2;

    pub const JNZ: Opcode = 3;

    pub const DUP: Opcode = 4;

    pub const POP: Opcode = 5;

    pub const SWAP: Opcode = 6;

    pub const SUBS: Opcode = 7;

    pub const NOT: Opcode = 8;
}
