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
    UnknownLabel(ir::BlockId),
    #[error("failed to set instruction operand")]
    SetOperand(#[source] bytecode::Error),
}

#[derive(Debug, Clone)]
pub struct Assembler {
    jmps: HashMap<ir::BlockId, bytecode::Operand>,
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
        for (label, block) in ir.blocks() {
            self.block_first_pass(label, block, ir)?;
        }
        for (label, block) in ir.blocks() {
            self.block_second_pass(label, block)?;
        }
        Ok(self.bytecode_prog)
    }

    fn block_first_pass(
        &mut self,
        label: ir::BlockId,
        block: &ir::Block,
        ir: &ir::Program,
    ) -> Result<(), Error> {
        let bytecode_label = self.bytecode_prog.past_last_label();
        self.jmps.insert(label, bytecode_label);

        for instr in block.instructions() {
            let (opcode, operand) = match instr {
                ir::Instruction::Nop => (bytecode::opcodes::NOP, 0),
                ir::Instruction::Dup => (bytecode::opcodes::DUP, 0),
                ir::Instruction::Pop => (bytecode::opcodes::POP, 0),
                ir::Instruction::Not => (bytecode::opcodes::NOT, 0),
                ir::Instruction::Swap => (bytecode::opcodes::SWAP, 0),
                ir::Instruction::Subs(subs_id) => {
                    let subs =
                        ir.get_substitution(subs_id).map_err(Error::GetSubs)?;
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

            let encoded_instr = bytecode::encode_instruction(opcode, operand)
                .map_err(Error::EncodeInstr)?;

            self.bytecode_prog.push(encoded_instr).map_err(Error::PushInstr)?;
        }

        Ok(())
    }

    fn block_second_pass(
        &mut self,
        label: ir::BlockId,
        block: &ir::Block,
    ) -> Result<(), Error> {
        for (instr, i) in block.instructions().zip(0 ..) {
            match instr {
                ir::Instruction::Jmp(dest)
                | ir::Instruction::Jz(dest)
                | ir::Instruction::Jnz(dest) => {
                    let block_src = self
                        .jmps
                        .get(&label)
                        .copied()
                        .ok_or(Error::UnknownLabel(label))?;
                    let bytecode_src = block_src + i;
                    let bytecode_dest = self
                        .jmps
                        .get(&dest)
                        .copied()
                        .ok_or(Error::UnknownLabel(dest))?;
                    self.bytecode_prog
                        .set_operand(
                            bytecode_src,
                            bytecode_dest - bytecode_src - 1,
                        )
                        .map_err(Error::SetOperand)?;
                },
                _ => (),
            }
        }

        Ok(())
    }
}
