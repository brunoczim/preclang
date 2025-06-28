use std::fmt;

use regex::Regex;

use crate::location::SpanlessEq;

#[derive(Debug, Clone)]
pub struct Pattern {
    pub regex: Regex,
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str()
    }
}

impl Eq for Pattern {}

impl SpanlessEq for Pattern {}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.regex.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum Flag {
    Global = 0x1,
    Insensitive = 0x2,
    Multiline = 0x4,
    Unicode = 0x8,
    DotNewline = 0x10,
    Crlf = 0x20,
    SwapGreed = 0x40,
    IgnoreWhitespace = 0x80,
}

impl SpanlessEq for Flag {}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global => write!(f, "g"),
            Self::Insensitive => write!(f, "i"),
            Self::Multiline => write!(f, "m"),
            Self::Unicode => write!(f, "u"),
            Self::DotNewline => write!(f, "s"),
            Self::Crlf => write!(f, "r"),
            Self::SwapGreed => write!(f, "G"),
            Self::IgnoreWhitespace => write!(f, "S"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Flags {
    bits: u16,
}

impl Flags {
    pub fn set(&mut self, flag: Flag) {
        self.bits |= flag as u16;
    }

    pub fn test(&self, flag: Flag) -> bool {
        self.bits & flag as u16 != 0
    }

    pub fn with(mut self, flag: Flag) -> Self {
        self.set(flag);
        self
    }
}

impl SpanlessEq for Flags {}

impl fmt::Display for Flags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for flag in [
            Flag::Global,
            Flag::Insensitive,
            Flag::Multiline,
            Flag::Unicode,
            Flag::DotNewline,
            Flag::Crlf,
            Flag::SwapGreed,
            Flag::IgnoreWhitespace,
        ] {
            if self.test(flag) {
                write!(f, "{flag}")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fragment {
    Text(String),
    RefInt(u32),
    RefName(String),
}

impl SpanlessEq for Fragment {}

impl fmt::Display for Fragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Text(text) => write!(f, "{text}"),
            Self::RefInt(int) => write!(f, "\\{}{}{}", '{', int, '}'),
            Self::RefName(name) => write!(f, "\\{}{}{}", '{', name, '}'),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Replacement {
    pub fragments: Vec<Fragment>,
}

impl SpanlessEq for Replacement {}

impl fmt::Display for Replacement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for fragment in &self.fragments {
            write!(f, "{fragment}")?
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution {
    pub pattern: Pattern,
    pub replacement: Replacement,
    pub flags: Flags,
}

impl SpanlessEq for Substitution {}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s/{}/{}/{}", self.pattern, self.replacement, self.flags)
    }
}
