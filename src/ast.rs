use crate::subs::Substitution;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Substitution(Substitution),
    Sequence(Sequence),
    Disjunction(Disjunction),
    Conjunction(Conjunction),
    Negation(Negation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Sequence {
    pub operands: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Disjunction {
    pub operands: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conjunction {
    pub operands: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Negation {
    pub operand: Box<Expr>,
}
