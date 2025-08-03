use thiserror::Error;

use crate::{
    assembly::{self, Instruction, Line, Program},
    ast::{BinaryOperation, Expr, PrefixOperation},
    subs::Substitution,
    token::{BinaryOperator, PrefixOperator},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to manipulate assembly representation")]
    Asm(
        #[source]
        #[from]
        assembly::Error,
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
        let subs_id = self.program.create_substitution(subs);
        self.program.create_line(Instruction::Subs(subs_id));
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
                self.program.create_line(Instruction::Dup);
                self.compile_expr(operation.lhs.data)?;
                let restore_lhs_fail_src =
                    self.program.create_line(Line::Placeholder);
                self.compile_expr(operation.rhs.data)?;
                let restore_rhs_fail_src =
                    self.program.create_line(Line::Placeholder);
                self.program.create_line(Instruction::Swap);
                let restore_fail_dest =
                    self.program.create_line(Line::Placeholder);
                self.program.set_line(
                    restore_lhs_fail_src,
                    Instruction::Jz(restore_fail_dest),
                )?;
                self.program.set_line(
                    restore_rhs_fail_src,
                    Instruction::Jz(restore_fail_dest),
                )?;
                self.program.create_line(Instruction::Pop);
            },
            BinaryOperator::Pipe => {
                self.program.create_line(Instruction::Dup);
                self.compile_expr(operation.lhs.data)?;
                let jmp_src_restore_lhs =
                    self.program.create_line(Line::Placeholder);
                self.program.create_line(Instruction::Pop);
                self.compile_expr(operation.rhs.data)?;
                let jmp_src_restore_rhs =
                    self.program.create_line(Line::Placeholder);
                let jmp_dest_restore_lhs =
                    self.program.create_line(Line::Placeholder);
                self.program.set_line(
                    jmp_src_restore_lhs,
                    Instruction::Jnz(jmp_dest_restore_lhs),
                )?;
                self.program.create_line(Instruction::Swap);
                self.program.create_line(Instruction::Pop);
                let jmp_dest_restore_rhs =
                    self.program.create_line(Line::Placeholder);
                self.program.set_line(
                    jmp_src_restore_rhs,
                    Instruction::Jmp(jmp_dest_restore_rhs),
                )?;
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
                self.program.create_line(Instruction::Not);
            },
        }
        Ok(())
    }
}
