use std::collections::HashMap;

use thiserror::Error;

use crate::{assembly, bytecode};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to create substitution")]
    CreateSubs(#[source] bytecode::Error),
    #[error("failed to find substitution")]
    GetSubs(#[source] assembly::Error),
    #[error("failed to encode instruction")]
    EncodeInstr(#[source] bytecode::Error),
    #[error("failed to insert instruction")]
    PushInstr(#[source] bytecode::Error),
    #[error("failed to find label {0} in jump table")]
    UnknownLabel(assembly::LabelId),
    #[error("failed to set instruction operand")]
    SetOperand(#[source] bytecode::Error),
}

#[derive(Debug, Clone)]
pub struct Assembler {
    jmps: HashMap<assembly::LabelId, bytecode::Operand>,
    bytecode_prog: bytecode::Program,
}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self { bytecode_prog: bytecode::Program::new(), jmps: HashMap::new() }
    }

    pub fn assemble(
        mut self,
        assembly: &assembly::Program,
    ) -> Result<bytecode::Program, Error> {
        for (label, line) in assembly.lines() {
            self.line_first_pass(label, line, assembly)?;
        }
        for (label, line) in assembly.lines() {
            self.line_second_pass(label, line)?;
        }
        Ok(self.bytecode_prog)
    }

    fn line_first_pass(
        &mut self,
        label: assembly::LabelId,
        line: assembly::Line,
        assembly: &assembly::Program,
    ) -> Result<(), Error> {
        let bytecode_label = match line {
            assembly::Line::Placeholder => self.bytecode_prog.past_last_label(),
            assembly::Line::Instruction(instr) => {
                let (opcode, operand) = match instr {
                    assembly::Instruction::Nop => (bytecode::opcodes::NOP, 0),
                    assembly::Instruction::Dup => (bytecode::opcodes::DUP, 0),
                    assembly::Instruction::Pop => (bytecode::opcodes::POP, 0),
                    assembly::Instruction::Not => (bytecode::opcodes::NOT, 0),
                    assembly::Instruction::Swap => (bytecode::opcodes::SWAP, 0),
                    assembly::Instruction::Subs(subs_id) => {
                        let subs = assembly
                            .get_substitution(subs_id)
                            .map_err(Error::GetSubs)?;
                        let operand = self
                            .bytecode_prog
                            .create_substitution(subs.clone())
                            .map_err(Error::CreateSubs)?;
                        (bytecode::opcodes::SUBS, operand)
                    },
                    assembly::Instruction::Jmp(_) => {
                        (bytecode::opcodes::JMP, 0)
                    },
                    assembly::Instruction::Jz(_) => (bytecode::opcodes::JZ, 0),
                    assembly::Instruction::Jnz(_) => {
                        (bytecode::opcodes::JNZ, 0)
                    },
                };

                let encoded_instr =
                    bytecode::encode_instruction(opcode, operand)
                        .map_err(Error::EncodeInstr)?;
                self.bytecode_prog
                    .push(encoded_instr)
                    .map_err(Error::PushInstr)?
            },
        };

        self.jmps.insert(label, bytecode_label);

        Ok(())
    }

    fn line_second_pass(
        &mut self,
        label: assembly::LabelId,
        line: assembly::Line,
    ) -> Result<(), Error> {
        let assembly::Line::Instruction(instr) = line else { return Ok(()) };

        match instr {
            assembly::Instruction::Jmp(dest)
            | assembly::Instruction::Jz(dest)
            | assembly::Instruction::Jnz(dest) => {
                let bytecode_label = self
                    .jmps
                    .get(&label)
                    .copied()
                    .ok_or(Error::UnknownLabel(label))?;
                let bytecode_dest = self
                    .jmps
                    .get(&dest)
                    .copied()
                    .ok_or(Error::UnknownLabel(dest))?;
                self.bytecode_prog
                    .set_operand(
                        bytecode_label,
                        bytecode_dest - bytecode_label - 1,
                    )
                    .map_err(Error::SetOperand)?;
            },
            _ => (),
        }

        Ok(())
    }
}
