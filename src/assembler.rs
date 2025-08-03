use std::collections::HashMap;

use thiserror::Error;

use crate::{bytecode, ir};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to create substitution")]
    CreateSubs(#[source] bytecode::Error),
    #[error("failed to find substitution")]
    GetSubs(#[source] ir::Error),
    #[error("failed to encode instruction")]
    EncodeInstr(#[source] bytecode::Error),
    #[error("failed to insert instruction")]
    PushInstr(#[source] bytecode::Error),
    #[error("failed to find label {0} in jump table")]
    UnknownLabel(ir::LabelId),
    #[error("failed to set instruction operand")]
    SetOperand(#[source] bytecode::Error),
}

#[derive(Debug, Clone)]
pub struct Assembler {
    jmps: HashMap<ir::LabelId, bytecode::Operand>,
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
        ir: &ir::Program,
    ) -> Result<bytecode::Program, Error> {
        for (label, line) in ir.lines() {
            self.line_first_pass(label, line, ir)?;
        }
        for (label, line) in ir.lines() {
            self.line_second_pass(label, line)?;
        }
        Ok(self.bytecode_prog)
    }

    fn line_first_pass(
        &mut self,
        label: ir::LabelId,
        line: ir::Line,
        ir: &ir::Program,
    ) -> Result<(), Error> {
        let bytecode_label = match line {
            ir::Line::Placeholder => self.bytecode_prog.past_last_label(),
            ir::Line::Instruction(instr) => {
                let (opcode, operand) = match instr {
                    ir::Instruction::Nop => (bytecode::opcodes::NOP, 0),
                    ir::Instruction::Dup => (bytecode::opcodes::DUP, 0),
                    ir::Instruction::Pop => (bytecode::opcodes::POP, 0),
                    ir::Instruction::Not => (bytecode::opcodes::NOT, 0),
                    ir::Instruction::Swap => (bytecode::opcodes::SWAP, 0),
                    ir::Instruction::Subs(subs_id) => {
                        let subs = ir
                            .get_substitution(subs_id)
                            .map_err(Error::GetSubs)?;
                        let operand = self
                            .bytecode_prog
                            .create_substitution(subs.clone())
                            .map_err(Error::CreateSubs)?;
                        (bytecode::opcodes::SUBS, operand)
                    },
                    ir::Instruction::Jmp(_) => (bytecode::opcodes::JMP, 0),
                    ir::Instruction::Jz(_) => (bytecode::opcodes::JZ, 0),
                    ir::Instruction::Jnz(_) => (bytecode::opcodes::JNZ, 0),
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
        label: ir::LabelId,
        line: ir::Line,
    ) -> Result<(), Error> {
        let ir::Line::Instruction(instr) = line else { return Ok(()) };

        match instr {
            ir::Instruction::Jmp(dest)
            | ir::Instruction::Jz(dest)
            | ir::Instruction::Jnz(dest) => {
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
