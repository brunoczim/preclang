use std::{rc::Rc, sync::Arc};

use crate::{
    ast::{BinaryOperation, Expr, PrefixOperation},
    location::Spanned,
    subs::{Flag, Fragment, Substitution},
    token::{BinaryOperator, PrefixOperator},
};

pub trait Evaluate {
    fn evaluate(&self, input: &str) -> (String, bool);
}

impl<'a, T> Evaluate for &'a T
where
    T: Evaluate + ?Sized,
{
    fn evaluate(&self, input: &str) -> (String, bool) {
        (**self).evaluate(input)
    }
}

impl<T> Evaluate for Box<T>
where
    T: Evaluate + ?Sized,
{
    fn evaluate(&self, input: &str) -> (String, bool) {
        (**self).evaluate(input)
    }
}

impl<T> Evaluate for Rc<T>
where
    T: Evaluate + ?Sized,
{
    fn evaluate(&self, input: &str) -> (String, bool) {
        (**self).evaluate(input)
    }
}

impl<T> Evaluate for Arc<T>
where
    T: Evaluate + ?Sized,
{
    fn evaluate(&self, input: &str) -> (String, bool) {
        (**self).evaluate(input)
    }
}

impl<T> Evaluate for Spanned<T>
where
    T: Evaluate,
{
    fn evaluate(&self, input: &str) -> (String, bool) {
        self.data.evaluate(input)
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, input: &str) -> (String, bool) {
        match self {
            Expr::Substitution(subs) => subs.evaluate(input),
            Expr::BinaryOperation(bin) => bin.evaluate(input),
            Expr::PrefixOperation(pre) => pre.evaluate(input),
        }
    }
}

impl Evaluate for Substitution {
    fn evaluate(&self, input: &str) -> (String, bool) {
        let mut output = String::new();
        let mut flag = false;
        let mut i = 0;

        for captures in self.pattern.regex.captures_iter(input) {
            let Some(main) = captures.get(0) else { break };
            let j = main.start();
            output.push_str(&input[i .. j]);

            flag = true;
            for fragment in &self.replacement.fragments {
                match fragment {
                    Fragment::Text(text) => output.push_str(text),
                    Fragment::RefInt(i) => {
                        if let Some(capture) = captures.get(*i as usize) {
                            output.push_str(capture.as_str());
                        }
                    },
                    Fragment::RefName(ident) => {
                        if let Some(capture) = captures.name(ident) {
                            output.push_str(capture.as_str());
                        }
                    },
                }
            }

            i = main.end();
            if !self.flags.test(Flag::Global) {
                break;
            }
        }

        output.push_str(&input[i ..]);
        (output, flag)
    }
}

impl Evaluate for BinaryOperation {
    fn evaluate(&self, input: &str) -> (String, bool) {
        match self.operator.data {
            BinaryOperator::Semicolon => {
                let (output, _) = self.lhs.data.evaluate(input);
                let (output, flag) = self.rhs.data.evaluate(&output);
                (output, flag)
            },
            BinaryOperator::Pipe => {
                let (mut output, mut flag) = self.lhs.data.evaluate(input);
                if !flag {
                    (output, flag) = self.rhs.data.evaluate(input);
                }
                (output, flag)
            },
            BinaryOperator::Ampersand => {
                let (mut output, mut flag) = self.lhs.data.evaluate(input);
                if flag {
                    (output, flag) = self.rhs.data.evaluate(&output);
                }
                if !flag {
                    output = input.to_owned();
                }
                (output, flag)
            },
        }
    }
}

impl Evaluate for PrefixOperation {
    fn evaluate(&self, input: &str) -> (String, bool) {
        match self.operator.data {
            PrefixOperator::Bang => {
                let (output, flag) = self.operand.data.evaluate(input);
                (output, !flag)
            },
        }
    }
}
