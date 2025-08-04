use thiserror::Error;

use crate::{
    ast::{BinaryOperation, Expr, PrefixOperation},
    ir::{self, Block, BlockId, Instruction, Program},
    subs::Substitution,
    token::{BinaryOperator, PrefixOperator},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to manipulate assembly IR representation")]
    Asm(
        #[source]
        #[from]
        ir::Error,
    ),
}

#[derive(Debug, Clone)]
pub struct Compiler {
    program: Program,
    curr_block: BlockId,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        let mut program = Program::new();
        let block = Block::new();
        let curr_block = program.create_block(block);
        Self { program, curr_block }
    }

    pub fn compile(mut self, expr: Expr) -> Result<Program, Error> {
        self.compile_expr(expr)?;
        Ok(self.program)
    }

    fn require_block(&mut self) -> Result<&mut Block, Error> {
        let block = self.program.get_block_mut(self.curr_block)?;
        Ok(block)
    }

    fn setup_block(&mut self, label: BlockId) {
        self.program.setup_block(label, Block::new());
        self.curr_block = label;
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
        self.require_block()?.push(Instruction::Subs(subs_id));
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
                let restore_label = self.program.reserve_block();

                self.require_block()?.push(Instruction::Dup);
                self.compile_expr(operation.lhs.data)?;
                self.require_block()?.push(Instruction::Jz(restore_label));
                self.compile_expr(operation.rhs.data)?;
                self.require_block()?.push(Instruction::Jz(restore_label));
                self.require_block()?.push(Instruction::Swap);

                self.setup_block(restore_label);
                self.require_block()?.push(Instruction::Pop);
            },
            BinaryOperator::Pipe => {
                let lhs_restore_label = self.program.reserve_block();
                let rhs_restore_label = self.program.reserve_block();

                self.require_block()?.push(Instruction::Dup);
                self.compile_expr(operation.lhs.data)?;
                self.require_block()?.push(Instruction::Jnz(lhs_restore_label));
                self.require_block()?.push(Instruction::Pop);
                self.compile_expr(operation.rhs.data)?;
                self.require_block()?.push(Instruction::Jmp(rhs_restore_label));

                self.setup_block(lhs_restore_label);
                self.require_block()?.push(Instruction::Swap);
                self.require_block()?.push(Instruction::Pop);

                self.setup_block(rhs_restore_label);
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
                self.require_block()?.push(Instruction::Not);
            },
        }
        Ok(())
    }
}
