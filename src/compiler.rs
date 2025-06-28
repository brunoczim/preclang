use thiserror::Error;

use crate::{
    ast::{BinaryOperation, Expr, PrefixOperation},
    ir::{self, Program, opcodes},
    subs::Substitution,
    token::{BinaryOperator, PrefixOperator},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to manipulate intermediate representation")]
    Ir(
        #[source]
        #[from]
        ir::Error,
    ),
}

#[derive(Debug, Clone)]
pub struct Compiler {
    program: Program,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self { program: Program::new() }
    }

    pub fn compile(mut self, expr: Expr) -> Result<Program, Error> {
        self.compile_expr(expr)?;
        Ok(self.program)
    }

    fn compile_expr(&mut self, expr: Expr) -> Result<(), Error> {
        match expr {
            Expr::Substitution(subs) => self.compile_subs(*subs),
            Expr::BinaryOperation(operation) => self.compile_bin_op(*operation),
            Expr::PrefixOperation(operation) => self.compile_pre_op(*operation),
        }
    }

    fn compile_subs(&mut self, subs: Substitution) -> Result<(), Error> {
        let subs_id = self.program.create_substitution(subs)?;
        self.program.push_encode(opcodes::SUBS, subs_id)?;
        Ok(())
    }

    fn compile_bin_op(
        &mut self,
        operation: BinaryOperation,
    ) -> Result<(), Error> {
        match operation.operator.data {
            BinaryOperator::Semicolon => {
                self.compile_expr(operation.lhs.data)?;
                self.compile_expr(operation.rhs.data)?;
            },
            BinaryOperator::Ampersand => {
                self.program.push_encode(opcodes::DUP, 0)?;
                self.compile_expr(operation.lhs.data)?;
                let jmp_src_restore =
                    self.program.push_encode(opcodes::JZ, 0)?;
                self.compile_expr(operation.rhs.data)?;
                self.program.push_encode(opcodes::SWAP, 0)?;
                let jmp_dest_restore = self.program.past_last_label();
                self.program.set_operand(jmp_src_restore, jmp_dest_restore)?;
                self.program.push_encode(opcodes::POP, 0)?;
            },
            BinaryOperator::Pipe => {
                self.program.push_encode(opcodes::DUP, 0)?;
                self.compile_expr(operation.lhs.data)?;
                let jmp_src_restore =
                    self.program.push_encode(opcodes::JNZ, 0)?;
                self.compile_expr(operation.rhs.data)?;
                let jmp_dest_restore = self.program.past_last_label();
                self.program.set_operand(jmp_src_restore, jmp_dest_restore)?;
                self.program.push_encode(opcodes::SWAP, 0)?;
                self.program.push_encode(opcodes::POP, 0)?;
            },
        }
        Ok(())
    }

    fn compile_pre_op(
        &mut self,
        operation: PrefixOperation,
    ) -> Result<(), Error> {
        match operation.operator.data {
            PrefixOperator::Bang => {
                self.compile_expr(operation.operand.data)?;
                self.program.push_encode(opcodes::NOT, 0)?;
            },
        }
        Ok(())
    }
}
