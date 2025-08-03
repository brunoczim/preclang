use std::fmt;

use thiserror::Error;

use crate::{
    assembler,
    compiler,
    location::{Location, Span},
    vm,
};

#[derive(Debug, Error)]
pub enum Ice {
    #[error(transparent)]
    InvalidSpan(#[from] InvalidSpan),
    #[error(transparent)]
    Compile(#[from] compiler::Error),
    #[error(transparent)]
    Assemble(#[from] assembler::Error),
    #[error(transparent)]
    Vm(#[from] vm::Error),
}

#[derive(Debug, Error)]
pub enum InvalidLocation {
    #[error("location {0} is invalid")]
    Unknown(Location),
}

#[derive(Debug, Error)]
pub enum InvalidSpan {
    #[error("invalid start")]
    Start(#[source] InvalidLocation),
    #[error("invalid end")]
    End(#[source] InvalidLocation),
    #[error("bad span order, start {}, end {}", .0.start, .0.end)]
    BadOrder(Span),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedLocation {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for ResolvedLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedSpan<'a> {
    pub start: ResolvedLocation,
    pub end: ResolvedLocation,
    pub content: &'a str,
}

impl<'a> fmt::Display for ResolvedSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedError<'a> {
    pub span: ResolvedSpan<'a>,
    pub message: String,
}

impl<'a> fmt::Display for ResolvedError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error in {}: {}\n>>> {}\n",
            self.span,
            self.message,
            self.span.content.trim_end(),
        )
    }
}

impl<'a> std::error::Error for ResolvedError<'a> {}

#[derive(Debug, Clone)]
pub struct ResolvedDiagnostics<'a> {
    pub errors: Vec<ResolvedError<'a>>,
}

impl<'a> fmt::Display for ResolvedDiagnostics<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, error) in self.errors.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{error}")?;
        }
        Ok(())
    }
}

impl<'a> std::error::Error for ResolvedDiagnostics<'a> {}

pub trait LangError: fmt::Display {
    fn span(&self) -> Span;
}

impl<'a, E> LangError for &'a E
where
    E: LangError + ?Sized,
{
    fn span(&self) -> Span {
        (**self).span()
    }
}

#[derive(Debug)]
pub struct Resolver<'a> {
    input: &'a str,
    newlines: Vec<usize>,
}

impl<'a> Resolver<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            newlines: input
                .char_indices()
                .filter(|(_, char)| *char == '\n')
                .map(|(i, _)| i)
                .collect(),
        }
    }

    fn find_line(&self, location: Location) -> usize {
        match self.newlines.binary_search(&location) {
            Ok(i) => i + 1,
            Err(i) => i + 1,
        }
    }

    fn find_column(&self, line: usize, location: Location) -> usize {
        match (line - 1).checked_sub(1) {
            Some(prev_line) => location - self.newlines[prev_line] + 1,
            None => location + 1,
        }
    }

    pub fn resolve_location(
        &self,
        location: Location,
    ) -> Result<ResolvedLocation, InvalidLocation> {
        if location > self.input.len() {
            Err(InvalidLocation::Unknown(location))?
        }
        let line = self.find_line(location);
        let column = self.find_column(line, location);
        Ok(ResolvedLocation { line, column, index: location })
    }

    pub fn resolve_span(
        &self,
        span: Span,
    ) -> Result<ResolvedSpan<'a>, InvalidSpan> {
        if span.start > span.end {
            Err(InvalidSpan::BadOrder(span))?
        }
        let start =
            self.resolve_location(span.start).map_err(InvalidSpan::Start)?;
        let end = self.resolve_location(span.end).map_err(InvalidSpan::End)?;
        let content = &self.input[span.start .. span.end];
        Ok(ResolvedSpan { start, end, content })
    }

    pub fn resolve_error<T>(
        &self,
        error: T,
    ) -> Result<ResolvedError<'a>, InvalidSpan>
    where
        T: LangError,
    {
        let span = self.resolve_span(error.span())?;
        let message = error.to_string();
        Ok(ResolvedError { span, message })
    }

    pub fn resolve_errors<I>(
        &self,
        errors: I,
    ) -> Result<ResolvedDiagnostics<'a>, InvalidSpan>
    where
        I: IntoIterator,
        I::Item: LangError,
    {
        let iter = errors.into_iter();
        let (low, _) = iter.size_hint();
        let mut diagnostics =
            ResolvedDiagnostics { errors: Vec::with_capacity(low) };
        for error in iter {
            diagnostics.errors.push(self.resolve_error(&error)?);
        }
        Ok(diagnostics)
    }
}
