use std::{collections::HashMap, iter, mem};

use thiserror::Error;

use crate::{
    ast::{BinaryOperation, Expr, Ident, LetIn, PrefixOperation},
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
    #[error("unknown identifier {}", _0.content.data)]
    UnknownIdent(Ident),
    #[error("duplicate identifier {}", dup.content.data)]
    DuplicateIdent { dup: Ident },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    prev_scopes: Vec<HashMap<String, BlockId>>,
    curr_scope: HashMap<String, BlockId>,
}

impl Env {
    pub fn new() -> Self {
        Self { prev_scopes: Vec::new(), curr_scope: HashMap::new() }
    }

    pub fn enter(&mut self) {
        let scope = mem::take(&mut self.curr_scope);
        self.prev_scopes.push(scope);
    }

    pub fn leave(&mut self) {
        let Some(prev_scope) = self.prev_scopes.pop() else {
            return;
        };
        self.curr_scope = prev_scope;
    }

    pub fn define(
        &mut self,
        ident: Ident,
        block: BlockId,
    ) -> Result<(), Error> {
        if self.curr_scope.contains_key(&ident.content.data) {
            Err(Error::DuplicateIdent { dup: ident.clone() })?
        }
        self.curr_scope.insert(ident.content.data, block);
        Ok(())
    }

    pub fn get(&self, ident: &Ident) -> Result<BlockId, Error> {
        let scopes =
            iter::once(&self.curr_scope).chain(self.prev_scopes.iter().rev());
        for scope in scopes {
            if let Some(block) = scope.get(&ident.content.data).copied() {
                return Ok(block);
            }
        }
        Err(Error::UnknownIdent(ident.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct Compiler {
    program: Program,
    curr_block: BlockId,
    env: Env,
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
        let env = Env::new();
        Self { program, curr_block, env }
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
            Expr::Ident(ident) => self.compile_ident(*ident),
            Expr::LetInClause(let_in) => self.compile_let_in(*let_in),
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

    fn compile_ident(&mut self, ident: Ident) -> Result<(), Error> {
        let block = self.env.get(&ident)?;
        self.require_block()?.push(Instruction::Call(block));
        Ok(())
    }

    fn compile_let_in(&mut self, let_in: LetIn) -> Result<(), Error> {
        self.env.enter();
        let main_block = self.program.reserve_block();
        for binding in &let_in.bindings {
            let block = self.program.reserve_block();
            self.env.define(binding.data.name.clone(), block)?;
        }
        self.setup_block(main_block);
        self.compile_expr(let_in.main.data)?;
        let end_block = self.program.reserve_block();
        self.require_block()?.push(Instruction::Jmp(end_block));
        for binding in let_in.bindings {
            let block = self.env.get(&binding.data.name)?;
            self.setup_block(block);
            self.compile_expr(binding.data.value.data)?;
            self.require_block()?.push(Instruction::Ret);
        }
        self.env.leave();
        self.setup_block(end_block);
        Ok(())
    }
}
