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
    Ident(Box<Ident>),
    LetInClause(Box<LetIn>),
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

    pub fn new_let_in(
        bindings: Vec<Spanned<Binding>>,
        main: Spanned<Self>,
    ) -> Spanned<Self> {
        let let_in = LetIn { bindings, main };
        let span = let_in.full_span();
        let data = Self::LetInClause(Box::new(let_in));
        Spanned { data, span }
    }

    pub fn new_ident(content: Spanned<String>) -> Spanned<Self> {
        let span = content.span;
        let data = Self::Ident(Box::new(Ident { content }));
        Spanned { data, span }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub content: Spanned<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub name: Ident,
    pub value: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetIn {
    pub bindings: Vec<Spanned<Binding>>,
    pub main: Spanned<Expr>,
}

impl LetIn {
    pub fn full_span(&self) -> Span {
        let start = self
            .bindings
            .first()
            .map_or(self.main.span, |binding| binding.span)
            .start;
        let end = self.main.span.end;
        Span { start, end }
    }
}
