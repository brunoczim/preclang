use crate::{ast::Expr, subs::Substitution, vm::{Instruction, Opcode}};

pub fn write_unsigned_operand(
    opcode: Opcode,
) -> [u8; INSTRUCTION_WIDTH]{
    instruction_bytes[0] = 0;
    instruction_bytes.rotate_right(1);
    Instruction::from_le_bytes(instruction_bytes)
}

pub fn write_signed_operand(
    instruction: Instruction
) -> [u8; INSTRUCTION_WIDTH] {
    let high_bit = instruction_bytes[instruction_bytes.len() - 1] & 0x80;
    instruction_bytes[0] = if high_bit == 0 { 0 } else { 0xff };
    instruction_bytes.rotate_right(1);
    Instruction::from_le_bytes(instruction_bytes)
}

#[derive(Debug)]
pub struct Compiler {
    bytecode: Vec<Instruction>,
    subs_table: Vec<Substitution>,
}

impl Compiler {
    pub fn new() -> Self {
        Self { bytecode: Vec::new(), subs_table: Vec::new() }
    }

    pub fn compile(&mut self, expr: Expr) {
        match expr {
            Expr::Substitution(subs) => {
                let index = self.subs_table.len() as u32;
                self.subs_table.push(subs);
                self.bytecode.push(
            }
        }
    }
}
