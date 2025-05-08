use regex::Regex;

#[derive(Debug, Clone)]
pub struct Pattern {
    pub regex: Regex,
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str()
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fragment {
    Text(String),
    RefInt(u32),
    RefName(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Replacement {
    pub fragments: Vec<Fragment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Substitution {
    pub pattern: Pattern,
    pub replacement: Replacement,
    pub flags: Flags,
}
