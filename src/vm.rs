use std::mem;

use thiserror::Error;

use crate::{
    eval::Evaluate,
    ir::{self, Opcode, Operand, Program, decode_instruction, opcodes},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to manipulate program")]
    Ir(
        #[source]
        #[from]
        ir::Error,
    ),
    #[error("opcode {0:8b} is invalid")]
    InvalidOpcode(Opcode),
}

#[derive(Debug, Clone)]
pub struct Machine {
    program: Program,
    ip: Operand,
    stack_tail: Vec<String>,
    flag: bool,
    text: String,
}

impl Machine {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            ip: 0,
            stack_tail: Vec::new(),
            flag: false,
            text: String::new(),
        }
    }

    pub fn set_text(&mut self, text: impl Into<String>) {
        self.text = text.into();
    }

    pub fn get_text(&self) -> &str {
        &self.text[..]
    }

    pub fn into_text(self) -> String {
        self.text
    }

    pub fn jmp_absolute(&mut self, label: Operand) {
        self.ip = label;
    }

    pub fn jmp_relative(&mut self, label_offset: Operand) {
        self.jmp_absolute(self.ip.wrapping_add(label_offset))
    }

    pub fn jmp_if_zero(&mut self, label_offset: Operand) {
        if !self.flag {
            self.jmp_relative(label_offset);
        }
    }

    pub fn jmp_if_not_zero(&mut self, label_offset: Operand) {
        if self.flag {
            self.jmp_relative(label_offset);
        }
    }

    pub fn set_flag(&mut self, flag: bool) {
        self.flag = flag;
    }

    pub fn get_flag(&self) -> bool {
        self.flag
    }

    pub fn dup(&mut self) {
        self.stack_tail.push(self.text.clone());
    }

    pub fn pop(&mut self) {
        self.text = self.stack_tail.pop().unwrap_or_default();
    }

    pub fn swap(&mut self) {
        let prev_top = mem::replace(
            &mut self.text,
            self.stack_tail.pop().unwrap_or_default(),
        );
        self.stack_tail.push(prev_top);
    }

    pub fn substitution(&mut self, id: Operand) -> Result<(), Error> {
        let subs = self.program.get_substitution(id)?;
        (self.text, self.flag) = subs.evaluate(&self.text);
        Ok(())
    }

    pub fn not(&mut self) {
        self.flag = !self.flag;
    }

    pub fn run_until_end(&mut self) -> Result<(), Error> {
        while self.cycle()? {}
        Ok(())
    }

    pub fn cycle(&mut self) -> Result<bool, Error> {
        let Ok(instruction) = self.program.get_instruction(self.ip) else {
            return Ok(false);
        };
        self.ip = self.ip.wrapping_add(1);
        let (opcode, operand) = decode_instruction(instruction);
        match opcode {
            opcodes::NOP => (),
            opcodes::JMP => self.jmp_relative(operand),
            opcodes::JZ => self.jmp_if_zero(operand),
            opcodes::JNZ => self.jmp_if_not_zero(operand),
            opcodes::DUP => self.dup(),
            opcodes::POP => self.pop(),
            opcodes::SWAP => self.swap(),
            opcodes::SUBS => self.substitution(operand)?,
            opcodes::NOT => self.not(),
            _ => Err(Error::InvalidOpcode(opcode))?,
        }
        Ok(true)
    }
}
