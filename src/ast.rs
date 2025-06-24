use crate::{
    location::{Span, SpanlessEq, Spanned},
    subs::Substitution,
    token::{BinaryOperator, PrefixOperator},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Substitution(Box<Substitution>),
    BinaryOperation(Box<BinaryOperation>),
    PrefixOperation(Box<PrefixOperation>),
}

impl Expr {
    pub fn new_subs(span: Span, subs: Substitution) -> Spanned<Self> {
        Spanned { span, data: Self::Substitution(Box::new(subs)) }
    }

    pub fn new_pre_op(
        operator: Spanned<PrefixOperator>,
        operand: Spanned<Self>,
    ) -> Spanned<Self> {
        let span = Span { start: operator.span.start, end: operand.span.end };
        let data = Self::PrefixOperation(Box::new(PrefixOperation {
            operator,
            operand,
        }));
        Spanned { span, data }
    }

    pub fn new_bin_op(
        lhs: Spanned<Self>,
        operator: Spanned<BinaryOperator>,
        rhs: Spanned<Self>,
    ) -> Spanned<Self> {
        let span = Span { start: lhs.span.start, end: rhs.span.end };
        let data = Self::BinaryOperation(Box::new(BinaryOperation {
            operator,
            lhs,
            rhs,
        }));
        Spanned { span, data }
    }
}

impl SpanlessEq for Expr {
    fn spanless_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Substitution(lhs), Self::Substitution(rhs)) => {
                lhs.spanless_eq(rhs)
            },
            (Self::BinaryOperation(lhs), Self::BinaryOperation(rhs)) => {
                lhs.spanless_eq(rhs)
            },
            (Self::PrefixOperation(lhs), Self::PrefixOperation(rhs)) => {
                lhs.spanless_eq(rhs)
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryOperation {
    pub lhs: Spanned<Expr>,
    pub operator: Spanned<BinaryOperator>,
    pub rhs: Spanned<Expr>,
}

impl SpanlessEq for BinaryOperation {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.lhs.spanless_eq(&other.lhs)
            && self.operator.spanless_eq(&other.operator)
            && self.rhs.spanless_eq(&other.rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrefixOperation {
    pub operator: Spanned<PrefixOperator>,
    pub operand: Spanned<Expr>,
}

impl PrefixOperation {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.operator.spanless_eq(&other.operator)
            && self.operand.spanless_eq(&other.operand)
    }
}
