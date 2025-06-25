use std::{num::ParseIntError, str::Chars};

use regex::RegexBuilder;
use thiserror::Error;

use crate::{
    error::LangError,
    location::{Location, Span, Spanned},
    subs::{Flag, Flags, Fragment, Pattern, Replacement, Substitution},
    token::{BinaryOperator, Operator, PrefixOperator, Punctuation, Token},
};

#[derive(Debug, Clone, Error)]
pub enum LexError {
    #[error("premature substitution end")]
    PrematureSubsEnd(Span),
    #[error("invalid number reference")]
    InvalidRefInt(Span),
    #[error("invalid name reference start")]
    InvalidRefNameStart(Location),
    #[error("invalid name reference tail")]
    InvalidRefNameTail(Location),
    #[error("failed to parse number reference, {0}")]
    ParseRefInt(ParseIntError, Span),
    #[error("failed to parse regular expression, {0}")]
    Regex(regex::Error, Span),
    #[error("invalid regular expression flag")]
    InvalidFlag(Location),
    #[error("invalid operator")]
    InvalidOperator(Span),
    #[error("invalid punctuation")]
    InvalidPunct(Location),
    #[error("invalid character")]
    InvalidChar(Location),
}

impl LangError for LexError {
    fn span(&self) -> Span {
        match self {
            Self::PrematureSubsEnd(span) => *span,
            Self::InvalidRefInt(span) => *span,
            Self::InvalidRefNameStart(start) => Span::unitary(*start),
            Self::InvalidRefNameTail(start) => Span::unitary(*start),
            Self::ParseRefInt(_, span) => *span,
            Self::Regex(_, span) => *span,
            Self::InvalidFlag(start) => Span::unitary(*start),
            Self::InvalidOperator(span) => *span,
            Self::InvalidPunct(start) => Span::unitary(*start),
            Self::InvalidChar(start) => Span::unitary(*start),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'input> {
    full_input: &'input str,
    position: usize,
    current: Option<char>,
    chars: Chars<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.chars();
        let current = chars.next();
        Lexer { full_input: input, position: 0, current, chars }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let current = self.current?;
        let position = self.position;
        self.current = self.chars.next();
        self.position += current.len_utf8();
        Some((position, current))
    }

    fn starts_subs(&self, ch: char) -> bool {
        ch == 's'
    }

    fn is_operator(&self, ch: char) -> bool {
        ";&|!".contains(ch)
    }

    fn is_punct(&self, ch: char) -> bool {
        "()".contains(ch)
    }

    fn lex_subs(&mut self, ch: char) -> Result<Spanned<Token>, LexError> {
        let start = self.position;
        self.advance();
        let Some(current) = self.current else {
            Err(LexError::PrematureSubsEnd(Span {
                start,
                end: start + ch.len_utf8(),
            }))?
        };
        if current != '/' {
            Err(LexError::PrematureSubsEnd(Span {
                start,
                end: start + ch.len_utf8(),
            }))?
        }
        self.advance();
        let pattern_range = self.lex_pattern_range(start)?;
        let replacement = self.lex_replacement(start)?;
        let flags = self.lex_flags()?;
        let mut regex_builder = RegexBuilder::new(
            &self.full_input[pattern_range.start .. pattern_range.end],
        );
        let regex = regex_builder
            .unicode(flags.test(Flag::Unicode))
            .dot_matches_new_line(flags.test(Flag::DotNewline))
            .case_insensitive(flags.test(Flag::Insensitive))
            .ignore_whitespace(flags.test(Flag::IgnoreWhitespace))
            .swap_greed(flags.test(Flag::SwapGreed))
            .multi_line(flags.test(Flag::Multiline))
            .crlf(flags.test(Flag::Crlf))
            .build()
            .map_err(|source| {
                LexError::Regex(source, Span { start, end: self.position })
            })?;
        let pattern = Pattern { regex };
        let token =
            Token::Substitution(Substitution { replacement, flags, pattern });
        Ok(Spanned { data: token, span: Span { start, end: self.position } })
    }

    fn lex_pattern_range(
        &mut self,
        subs_start: usize,
    ) -> Result<Span, LexError> {
        let start = self.position;
        let mut end = start;
        loop {
            let Some(ch) = self.current else {
                Err(LexError::PrematureSubsEnd(Span {
                    start: subs_start,
                    end: self.position,
                }))?
            };

            self.advance();

            if ch == '\\' {
                if self.advance().is_none() {
                    Err(LexError::PrematureSubsEnd(Span {
                        start: subs_start,
                        end: self.position,
                    }))?
                }
            } else if ch == '/' {
                break;
            }
            end = self.position;
        }

        Ok(Span { start, end })
    }

    fn lex_replacement(
        &mut self,
        subs_start: usize,
    ) -> Result<Replacement, LexError> {
        let mut fragments = Vec::new();
        let mut text_start = self.position;

        loop {
            let Some(ch) = self.current else { break };
            if ch == '\\' {
                let text_end = self.position;
                self.advance();
                let Some(ch) = self.current else {
                    Err(LexError::PrematureSubsEnd(Span {
                        start: subs_start,
                        end: self.position,
                    }))?
                };
                let mut next_fragment = None;
                if ch.is_digit(10) {
                    next_fragment = Some(self.lex_number_fragment()?);
                } else if ch == '{' {
                    next_fragment = Some(self.lex_curly_fragment(subs_start)?);
                }
                if let Some(fragment) = next_fragment {
                    if text_start != text_end {
                        fragments.push(Fragment::Text(
                            self.full_input[text_start .. text_end].to_owned(),
                        ));
                    }
                    text_start = self.position;
                    fragments.push(fragment);
                }
            } else if ch == '/' {
                break;
            } else {
                self.advance();
            }
        }

        if text_start != self.position {
            fragments.push(Fragment::Text(
                self.full_input[text_start .. self.position].to_owned(),
            ));
        }

        Ok(Replacement { fragments })
    }

    fn lex_number_fragment(&mut self) -> Result<Fragment, LexError> {
        let start = self.position;
        loop {
            let Some(ch) = self.current else { break };
            if ch.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        self.full_input[start .. self.position]
            .parse()
            .map(Fragment::RefInt)
            .map_err(|source| {
                LexError::ParseRefInt(
                    source,
                    Span { start, end: self.position },
                )
            })
    }

    fn lex_curly_fragment(
        &mut self,
        subs_start: usize,
    ) -> Result<Fragment, LexError> {
        self.advance();
        let start = self.position;
        let Some(ch) = self.current else {
            Err(LexError::PrematureSubsEnd(Span {
                start: subs_start,
                end: self.position,
            }))?
        };
        if ch.is_digit(10) {
            let mut end;
            loop {
                let Some(ch) = self.current else {
                    Err(LexError::PrematureSubsEnd(Span {
                        start: subs_start,
                        end: self.position,
                    }))?
                };
                end = self.position;
                self.advance();
                if ch == '}' {
                    break;
                }
            }

            self.full_input[start .. end].parse().map(Fragment::RefInt).map_err(
                |source| LexError::ParseRefInt(source, Span { start, end }),
            )
        } else if ch.is_alphabetic() {
            let mut end;
            loop {
                let Some(ch) = self.current else {
                    Err(LexError::PrematureSubsEnd(Span {
                        start: subs_start,
                        end: self.position,
                    }))?
                };
                end = self.position;
                self.advance();
                if ch == '}' {
                    break;
                }
                if !ch.is_alphanumeric() && ch != '_' {
                    Err(LexError::InvalidRefNameTail(end))?
                }
            }

            self.full_input[start .. end].parse().map(Fragment::RefInt).map_err(
                |source| LexError::ParseRefInt(source, Span { start, end }),
            )
        } else {
            Err(LexError::InvalidRefNameStart(start))?
        }
    }

    fn lex_flags(&mut self) -> Result<Flags, LexError> {
        let mut flags = Flags::default();
        if self.current == Some('/') {
            self.advance();
            loop {
                let Some(ch) = self.current else { break };
                if !ch.is_alphanumeric() {
                    break;
                }
                let flag = match ch {
                    'g' => Flag::Global,
                    'i' => Flag::Insensitive,
                    'm' => Flag::Multiline,
                    'u' => Flag::Unicode,
                    's' => Flag::DotNewline,
                    'r' => Flag::Crlf,
                    'G' => Flag::SwapGreed,
                    'S' => Flag::IgnoreWhitespace,
                    _ => Err(LexError::InvalidFlag(self.position))?,
                };
                flags.set(flag);
                self.advance();
            }
        }
        Ok(flags)
    }

    fn lex_operator(&mut self) -> Result<Spanned<Token>, LexError> {
        let start = self.position;
        self.advance();
        let mut end = self.position;
        while self.current.is_some_and(|ch| self.is_operator(ch)) {
            self.advance();
            end = self.position;
        }
        let token = Token::Operator(match &self.full_input[start .. end] {
            ";" => Operator::Binary(BinaryOperator::Semicolon),
            "|" => Operator::Binary(BinaryOperator::Pipe),
            "&" => Operator::Binary(BinaryOperator::Ampersand),
            "!" => Operator::Prefix(PrefixOperator::Bang),
            _ => Err(LexError::InvalidOperator(Span { start, end }))?,
        });
        Ok(Spanned { data: token, span: Span { start, end } })
    }

    fn lex_punct(&mut self) -> Result<Spanned<Token>, LexError> {
        let start = self.position;
        self.advance();
        let end = self.position;
        let token = Token::Punct(match &self.full_input[start .. end] {
            "(" => Punctuation::OpenParen,
            ")" => Punctuation::CloseParen,
            _ => Err(LexError::InvalidPunct(start))?,
        });
        Ok(Spanned { data: token, span: Span { start, end } })
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Spanned<Token>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let ch = self.current?;
            if ch.is_whitespace() {
                self.advance();
                continue;
            }
            if self.starts_subs(ch) {
                break Some(self.lex_subs(ch));
            }
            if self.is_operator(ch) {
                break Some(self.lex_operator());
            }
            if self.is_punct(ch) {
                break Some(self.lex_punct());
            }
            let error = LexError::InvalidChar(self.position);
            self.advance();
            break Some(Err(error));
        }
    }
}
