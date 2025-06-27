use thiserror::Error;

use crate::{
    eval::Evaluate,
    ir::{self, Opcode, Operand, Program, decode_instruction, opcodes},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("label {0} is invalid")]
    InvalidLabel(Operand),
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
    flag_stack: Vec<bool>,
    text_stack: Vec<String>,
    flag: bool,
    text: String,
}

impl Machine {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            ip: 0,
            flag_stack: Vec::new(),
            text_stack: Vec::new(),
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

    pub fn jmp_absolute(&mut self, label: Operand) -> Result<(), Error> {
        if label < 0 {
            Err(Error::InvalidLabel(label))?
        }
        self.ip = label;
        Ok(())
    }

    pub fn jmp_relative(&mut self, label_offset: Operand) -> Result<(), Error> {
        self.jmp_absolute(self.ip.wrapping_add(label_offset))
    }

    pub fn jmp_if_zero(&mut self, label_offset: Operand) -> Result<(), Error> {
        if !self.flag {
            self.jmp_relative(label_offset)?;
        }
        Ok(())
    }

    pub fn jmp_if_not_zero(
        &mut self,
        label_offset: Operand,
    ) -> Result<(), Error> {
        if self.flag {
            self.jmp_relative(label_offset)?;
        }
        Ok(())
    }

    pub fn set_flag(&mut self, flag: bool) {
        self.flag = flag;
    }

    pub fn get_flag(&self) -> bool {
        self.flag
    }

    pub fn dup_text(&mut self) {
        self.text_stack.push(self.text.clone());
    }

    pub fn dup_flag(&mut self) {
        self.flag_stack.push(self.flag);
    }

    pub fn pop_text(&mut self) {
        self.text = self.text_stack.pop().unwrap_or_default();
    }

    pub fn pop_flag(&mut self) {
        self.flag = self.flag_stack.pop().unwrap_or_default();
    }

    pub fn subs(&mut self, id: Operand) -> Result<(), Error> {
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
            opcodes::JMP => self.jmp_relative(operand)?,
            opcodes::JZ => self.jmp_if_zero(operand)?,
            opcodes::JNZ => self.jmp_if_not_zero(operand)?,
            opcodes::DUPF => self.dup_flag(),
            opcodes::DUPT => self.dup_text(),
            opcodes::POPF => self.pop_flag(),
            opcodes::POPT => self.pop_text(),
            opcodes::SUBS => self.subs(operand)?,
            opcodes::NOT => self.not(),
            _ => Err(Error::InvalidOpcode(opcode))?,
        }
        Ok(true)
    }
}
