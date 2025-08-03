use std::{collections::HashSet, fmt, hash::Hash};

use indexmap::IndexMap;
use thiserror::Error;

use crate::subs::Substitution;

#[derive(Debug, Error)]
pub enum Error {
    #[error("label {0} is unknown")]
    UnknwonLabel(LabelId),
    #[error("substitution id {0} is unknown")]
    UnknwonSubs(SubsId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelId(usize);

impl fmt::Display for LabelId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PastLastLabel(LabelId);

impl fmt::Display for PastLastLabel {
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
pub enum Line {
    Placeholder,
    Instruction(Instruction),
}

impl From<Instruction> for Line {
    fn from(instr: Instruction) -> Self {
        Self::Instruction(instr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction {
    Nop,
    Jmp(LabelId),
    Jz(LabelId),
    Jnz(LabelId),
    Dup,
    Pop,
    Swap,
    Subs(SubsId),
    Not,
}

#[derive(Debug, Clone)]
pub struct Program {
    lines: IndexMap<LabelId, Line>,
    substitutions: IndexMap<SubsId, Substitution>,
    next_label: LabelId,
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
            lines: IndexMap::new(),
            substitutions: IndexMap::new(),
            next_label: LabelId(0),
            next_subs: SubsId(0),
        }
    }

    pub fn create_line(&mut self, line: impl Into<Line>) -> LabelId {
        let label = self.next_label;
        self.next_label.0 += 1;
        self.lines.insert(label, line.into());
        label
    }

    pub fn get_line(&self, label: LabelId) -> Result<Line, Error> {
        self.lines.get(&label).copied().ok_or(Error::UnknwonLabel(label))
    }

    pub fn set_line(
        &mut self,
        label: LabelId,
        line: impl Into<Line>,
    ) -> Result<(), Error> {
        let line_ref =
            self.lines.get_mut(&label).ok_or(Error::UnknwonLabel(label))?;
        *line_ref = line.into();
        Ok(())
    }

    pub fn remove_line(&mut self, label: LabelId) -> Result<Line, Error> {
        self.lines.shift_remove(&label).ok_or(Error::UnknwonLabel(label))
    }

    pub fn lines(&self) -> impl Iterator<Item = (LabelId, Line)> + Send {
        self.lines.iter().map(|(&label, &line)| (label, line))
    }

    pub fn lines_mut(
        &mut self,
    ) -> impl Iterator<Item = (LabelId, &mut Line)> + Send {
        self.lines.iter_mut().map(|(&label, line)| (label, line))
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

    pub fn set_substitution(
        &mut self,
        subs_id: SubsId,
        subs: Substitution,
    ) -> Result<(), Error> {
        let subs_ref = self
            .substitutions
            .get_mut(&subs_id)
            .ok_or(Error::UnknwonSubs(subs_id))?;
        *subs_ref = subs;
        Ok(())
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

    pub fn past_last_label(&self) -> PastLastLabel {
        PastLastLabel(self.next_label)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Emit<'a> {
    pub program: &'a Program,
    pub expand_subs: bool,
}

impl<'a> fmt::Display for Emit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.expand_subs {
            let mut empty = true;
            for (id, subs) in self.program.substitutions() {
                write!(f, "#{id}:\n")?;
                write!(f, "  {subs}\n")?;
                empty = false;
            }
            if !empty {
                write!(f, "\n")?;
            }
        }

        let mut labels = HashSet::new();
        for instruction in
            self.program.lines().filter_map(|(_, line)| match line {
                Line::Instruction(instr) => Some(instr),
                Line::Placeholder => None,
            })
        {
            let dest = match instruction {
                Instruction::Jmp(label)
                | Instruction::Jz(label)
                | Instruction::Jnz(label) => label,
                _ => continue,
            };
            labels.insert(dest);
        }

        let mut first = true;
        for (label, line) in self.program.lines() {
            if labels.contains(&label) || first {
                write!(f, "L_{}:\n", label)?;
            }
            first = false;
            let Line::Instruction(instr) = line else {
                continue;
            };
            write!(f, "  ")?;
            match instr {
                Instruction::Nop => write!(f, "nop")?,
                Instruction::Jmp(label) => write!(f, "jmp  L_{}", label)?,
                Instruction::Jz(label) => write!(f, "jz   L_{}", label)?,
                Instruction::Jnz(label) => write!(f, "jnz  L_{}", label)?,
                Instruction::Dup => write!(f, "dup")?,
                Instruction::Pop => write!(f, "pop")?,
                Instruction::Swap => write!(f, "swap")?,
                Instruction::Subs(subs_id) => {
                    if self.expand_subs {
                        match self.program.get_substitution(subs_id) {
                            Ok(subs) => write!(f, "subs {}", subs)?,
                            Err(_) => write!(f, "subs ??")?,
                        }
                    } else {
                        write!(f, "subs #{}", subs_id)?;
                    }
                },
                Instruction::Not => write!(f, "not")?,
            }
            write!(f, "\n")?;
        }
        let past_last_label = self.program.past_last_label();
        if labels.contains(&past_last_label.0) {
            write!(f, "L_{}:\n", past_last_label)?;
        }
        Ok(())
    }
}
