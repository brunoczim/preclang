use std::collections::HashMap;

use thiserror::Error;

use crate::{assembly, ir};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to create substitution")]
    CreateSubs(#[source] ir::Error),
    #[error("failed to find substitution")]
    GetSubs(#[source] assembly::Error),
    #[error("failed to encode instruction")]
    EncodeInstr(#[source] ir::Error),
    #[error("failed to insert instruction")]
    PushInstr(#[source] ir::Error),
    #[error("failed to find label {0} in jump table")]
    UnknownLabel(assembly::LabelId),
    #[error("failed to set instruction operand")]
    SetOperand(#[source] ir::Error),
}

#[derive(Debug, Clone)]
pub struct Assembler {
    jmps: HashMap<assembly::LabelId, ir::Operand>,
    ir_prog: ir::Program,
}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self { ir_prog: ir::Program::new(), jmps: HashMap::new() }
    }

    pub fn assemble(
        mut self,
        assembly: &assembly::Program,
    ) -> Result<ir::Program, Error> {
        for (label, line) in assembly.lines() {
            self.instr_first_pass(label, line, assembly)?;
        }
        for (label, line) in assembly.lines() {
            self.instr_second_pass(label, line)?;
        }
        Ok(self.ir_prog)
    }

    fn instr_first_pass(
        &mut self,
        label: assembly::LabelId,
        line: assembly::Line,
        assembly: &assembly::Program,
    ) -> Result<(), Error> {
        let ir_label = match line {
            assembly::Line::Placeholder => self.ir_prog.past_last_label(),
            assembly::Line::Instruction(instr) => {
                let (opcode, operand) = match instr {
                    assembly::Instruction::Nop => (ir::opcodes::NOP, 0),
                    assembly::Instruction::Dup => (ir::opcodes::DUP, 0),
                    assembly::Instruction::Pop => (ir::opcodes::POP, 0),
                    assembly::Instruction::Not => (ir::opcodes::NOT, 0),
                    assembly::Instruction::Swap => (ir::opcodes::SWAP, 0),
                    assembly::Instruction::Subs(subs_id) => {
                        let subs = assembly
                            .get_substitution(subs_id)
                            .map_err(Error::GetSubs)?;
                        let operand = self
                            .ir_prog
                            .create_substitution(subs.clone())
                            .map_err(Error::CreateSubs)?;
                        (ir::opcodes::SUBS, operand)
                    },
                    assembly::Instruction::Jmp(_) => (ir::opcodes::JMP, 0),
                    assembly::Instruction::Jz(_) => (ir::opcodes::JZ, 0),
                    assembly::Instruction::Jnz(_) => (ir::opcodes::JNZ, 0),
                };

                let encoded_instr = ir::encode_instruction(opcode, operand)
                    .map_err(Error::EncodeInstr)?;
                self.ir_prog.push(encoded_instr).map_err(Error::PushInstr)?
            },
        };

        self.jmps.insert(label, ir_label);

        Ok(())
    }

    fn instr_second_pass(
        &mut self,
        label: assembly::LabelId,
        line: assembly::Line,
    ) -> Result<(), Error> {
        let assembly::Line::Instruction(instr) = line else { return Ok(()) };

        match instr {
            assembly::Instruction::Jmp(dest)
            | assembly::Instruction::Jz(dest)
            | assembly::Instruction::Jnz(dest) => {
                let ir_label = self
                    .jmps
                    .get(&label)
                    .copied()
                    .ok_or(Error::UnknownLabel(label))?;
                let ir_dest = self
                    .jmps
                    .get(&dest)
                    .copied()
                    .ok_or(Error::UnknownLabel(dest))?;
                self.ir_prog
                    .set_operand(ir_label, ir_dest - ir_label - 1)
                    .map_err(Error::SetOperand)?;
            },
            _ => (),
        }

        Ok(())
    }
}
