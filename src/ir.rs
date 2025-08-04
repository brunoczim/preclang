use std::{fmt, hash::Hash};

use indexmap::IndexMap;
use thiserror::Error;

use crate::subs::Substitution;

#[derive(Debug, Error)]
pub enum Error {
    #[error("label {0} is unknown")]
    UnknwonBlock(BlockId),
    #[error("substitution id {0} is unknown")]
    UnknwonSubs(SubsId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubsId(usize);

impl fmt::Display for SubsId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction {
    Nop,
    Jmp(BlockId),
    Jz(BlockId),
    Jnz(BlockId),
    Dup,
    Pop,
    Swap,
    Subs(SubsId),
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    instructions: Vec<Instruction>,
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

impl Block {
    pub fn new() -> Self {
        Self { instructions: Vec::new() }
    }

    pub fn push(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    pub fn instructions(&self) -> impl Iterator<Item = Instruction> + Send {
        self.instructions.iter().copied()
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    blocks: IndexMap<BlockId, Block>,
    substitutions: IndexMap<SubsId, Substitution>,
    next_label: BlockId,
    next_subs: SubsId,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            blocks: IndexMap::new(),
            substitutions: IndexMap::new(),
            next_label: BlockId(0),
            next_subs: SubsId(0),
        }
    }

    pub fn reserve_block(&mut self) -> BlockId {
        let label = self.next_label;
        self.next_label.0 += 1;
        label
    }

    pub fn setup_block(&mut self, label: BlockId, block: Block) {
        self.blocks.insert(label, block);
    }

    pub fn create_block(&mut self, block: Block) -> BlockId {
        let label = self.reserve_block();
        self.setup_block(label, block);
        label
    }

    pub fn get_block(&self, label: BlockId) -> Result<&Block, Error> {
        self.blocks.get(&label).ok_or(Error::UnknwonBlock(label))
    }

    pub fn get_block_mut(
        &mut self,
        label: BlockId,
    ) -> Result<&mut Block, Error> {
        self.blocks.get_mut(&label).ok_or(Error::UnknwonBlock(label))
    }

    pub fn remove_block(&mut self, label: BlockId) -> Result<Block, Error> {
        self.blocks.shift_remove(&label).ok_or(Error::UnknwonBlock(label))
    }

    pub fn blocks(&self) -> impl Iterator<Item = (BlockId, &Block)> + Send {
        self.blocks.iter().map(|(&label, block)| (label, block))
    }

    pub fn blocks_mut(
        &mut self,
    ) -> impl Iterator<Item = (BlockId, &mut Block)> + Send {
        self.blocks.iter_mut().map(|(&label, block)| (label, block))
    }

    pub fn create_substitution(&mut self, subs: Substitution) -> SubsId {
        let subs_id = self.next_subs;
        self.next_subs.0 += 1;
        self.substitutions.insert(subs_id, subs);
        subs_id
    }

    pub fn get_substitution(
        &self,
        subs_id: SubsId,
    ) -> Result<&Substitution, Error> {
        self.substitutions.get(&subs_id).ok_or(Error::UnknwonSubs(subs_id))
    }

    pub fn get_substitution_mut(
        &mut self,
        subs_id: SubsId,
    ) -> Result<&mut Substitution, Error> {
        self.substitutions.get_mut(&subs_id).ok_or(Error::UnknwonSubs(subs_id))
    }

    pub fn remove_substitution(
        &mut self,
        subs_id: SubsId,
    ) -> Result<Substitution, Error> {
        self.substitutions
            .shift_remove(&subs_id)
            .ok_or(Error::UnknwonSubs(subs_id))
    }

    pub fn substitutions(
        &self,
    ) -> impl Iterator<Item = (SubsId, &Substitution)> + Send {
        self.substitutions.iter().map(|(&subs_id, subs)| (subs_id, subs))
    }

    pub fn substitutions_mut(
        &mut self,
    ) -> impl Iterator<Item = (SubsId, &mut Substitution)> + Send {
        self.substitutions.iter_mut().map(|(&subs_id, subs)| (subs_id, subs))
    }
}
