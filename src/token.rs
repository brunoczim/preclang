use crate::{location::SpanlessEq, subs::Substitution};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Semicolon,
    Pipe,
    Ampersand,
}

impl SpanlessEq for BinaryOperator {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    Bang,
}

impl SpanlessEq for PrefixOperator {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Binary(BinaryOperator),
    Prefix(PrefixOperator),
}

impl SpanlessEq for Operator {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
}

impl SpanlessEq for Punctuation {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Operator(Operator),
    Substitution(Substitution),
    Punct(Punctuation),
}

impl SpanlessEq for Token {}
