use crate::subs::Substitution;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Semicolon,
    Pipe,
    Ampersand,
    Bang,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
}

#[derive(Debug, Clone)]
pub enum Token {
    Operator(Operator),
    Substitution(Substitution),
    Punct(Punctuation),
}
