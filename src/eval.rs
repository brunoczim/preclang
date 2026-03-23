use std::{collections::HashMap, iter, mem, rc::Rc, sync::Arc};

use thiserror::Error;

use crate::{
    ast::{BinaryOperation, Expr, Ident, LetIn, PrefixOperation},
    location::{Location, Spanned},
    subs::Substitution,
    token::{BinaryOperator, PrefixOperator},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("unknown identifier {}", _0.content.data)]
    UnknownIdent(Ident),
    #[error("duplicate identifier {}", dup.content.data)]
    DuplicateIdent { dup: Ident },
    #[error("stack overflow")]
    StackOverflow(Location),
    #[error("stack underflow")]
    StackUnderflow(Location),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Env<'a> {
    limit: usize,
    prev_scopes: Vec<HashMap<&'a String, &'a Expr>>,
    curr_scope: HashMap<&'a String, &'a Expr>,
}

impl<'a> Env<'a> {
    pub fn new(limit: usize) -> Self {
        Self { limit, prev_scopes: Vec::new(), curr_scope: HashMap::new() }
    }

    pub fn enter(&mut self, loc: Location) -> Result<(), Error> {
        if self.prev_scopes.len() + 1 > self.limit {
            Err(Error::StackOverflow(loc))?
        }
        let scope = mem::take(&mut self.curr_scope);
        self.prev_scopes.push(scope);
        Ok(())
    }

    pub fn leave(&mut self, loc: Location) -> Result<(), Error> {
        let Some(prev_scope) = self.prev_scopes.pop() else {
            Err(Error::StackUnderflow(loc))?
        };
        self.curr_scope = prev_scope;
        Ok(())
    }

    pub fn define(
        &mut self,
        ident: &'a Ident,
        value: &'a Expr,
    ) -> Result<(), Error> {
        if self.curr_scope.contains_key(&&ident.content.data) {
            Err(Error::DuplicateIdent { dup: ident.clone() })?
        }
        self.curr_scope.insert(&ident.content.data, value);
        Ok(())
    }

    pub fn get(&self, ident: &'a Ident) -> Result<&'a Expr, Error> {
        let scopes =
            iter::once(&self.curr_scope).chain(self.prev_scopes.iter().rev());
        for scope in scopes {
            if let Some(expr) = scope.get(&ident.content.data).copied() {
                return Ok(expr);
            }
        }
        Err(Error::UnknownIdent(ident.clone()))
    }
}

pub trait Evaluate {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error>;
}

impl<'a, T> Evaluate for &'a T
where
    T: Evaluate + ?Sized,
{
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        (**self).evaluate(input, env)
    }
}

impl<T> Evaluate for Box<T>
where
    T: Evaluate + ?Sized,
{
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        (**self).evaluate(input, env)
    }
}

impl<T> Evaluate for Rc<T>
where
    T: Evaluate + ?Sized,
{
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        (**self).evaluate(input, env)
    }
}

impl<T> Evaluate for Arc<T>
where
    T: Evaluate + ?Sized,
{
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        (**self).evaluate(input, env)
    }
}

impl<T> Evaluate for Spanned<T>
where
    T: Evaluate,
{
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        self.data.evaluate(input, env)
    }
}

impl Evaluate for Expr {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        match self {
            Expr::Substitution(subs) => subs.evaluate(input, env),
            Expr::BinaryOperation(bin) => bin.evaluate(input, env),
            Expr::PrefixOperation(pre) => pre.evaluate(input, env),
            Expr::Ident(ident) => ident.evaluate(input, env),
            Expr::LetInClause(let_in) => let_in.evaluate(input, env),
        }
    }
}

impl Evaluate for Substitution {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        _env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        Ok(self.evaluate_raw(input))
    }
}

impl Evaluate for BinaryOperation {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        match self.operator.data {
            BinaryOperator::Semicolon => {
                let (output, _) = self.lhs.data.evaluate(input, env)?;
                let (output, flag) = self.rhs.data.evaluate(&output, env)?;
                Ok((output, flag))
            },
            BinaryOperator::Pipe => {
                let (mut output, mut flag) =
                    self.lhs.data.evaluate(input, env)?;
                if !flag {
                    (output, flag) = self.rhs.data.evaluate(input, env)?;
                }
                Ok((output, flag))
            },
            BinaryOperator::Ampersand => {
                let (mut output, mut flag) =
                    self.lhs.data.evaluate(input, env)?;
                if flag {
                    (output, flag) = self.rhs.data.evaluate(&output, env)?;
                }
                if !flag {
                    output = input.to_owned();
                }
                Ok((output, flag))
            },
        }
    }
}

impl Evaluate for PrefixOperation {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        match self.operator.data {
            PrefixOperator::Bang => {
                let (output, flag) = self.operand.data.evaluate(input, env)?;
                Ok((output, !flag))
            },
        }
    }
}

impl Evaluate for Ident {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        env.get(self)?.evaluate(input, env)
    }
}

impl Evaluate for LetIn {
    fn evaluate<'s>(
        &'s self,
        input: &str,
        env: &mut Env<'s>,
    ) -> Result<(String, bool), Error> {
        env.enter(self.full_span().start)?;
        for binding in &self.bindings {
            if let Err(e) =
                env.define(&binding.data.name, &binding.data.value.data)
            {
                _ = env.leave(self.full_span().end);
                Err(e)?
            }
        }
        match self.main.evaluate(input, env) {
            Ok(output) => {
                env.leave(self.full_span().end)?;
                Ok(output)
            },
            Err(e) => {
                _ = env.leave(self.full_span().end);
                Err(e)
            },
        }
    }
}
