use std::{
    borrow::Cow,
    error::Error as _,
    fmt,
    fs,
    io::{self, Read},
    path::PathBuf,
    process,
};

use clap::Parser;
use preclang::{
    error::{Ice, ResolvedDiagnostics},
    recipes,
};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Source {
    Inline(String),
    Stdin,
    File(PathBuf),
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inline(_) => write!(f, "<inline>"),
            Self::Stdin => write!(f, "<stdin>"),
            Self::File(path) => write!(f, "{}", path.display()),
        }
    }
}

impl Source {
    pub fn read(&self) -> Result<Cow<str>, Error> {
        match self {
            Self::Inline(text) => Ok(Cow::Borrowed(&text[..])),
            Self::Stdin => {
                let mut buf = String::new();
                io::stdin()
                    .read_to_string(&mut buf)
                    .map_err(ErrorKind::from)
                    .map_err(|kind| Error { source: self.clone(), kind })?;
                Ok(Cow::Owned(buf))
            },
            Self::File(path) => {
                let buf = fs::read_to_string(path)
                    .map_err(ErrorKind::from)
                    .map_err(|kind| Error { source: self.clone(), kind })?;
                Ok(Cow::Owned(buf))
            },
        }
    }
}

#[derive(Debug, Error)]
#[error("{source}: {kind}")]
struct Error {
    source: Source,
    #[source]
    kind: ErrorKind,
}

#[derive(Debug, Error)]
enum ErrorKind {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Ice(#[from] Ice),
    #[error("{0}")]
    Lang(String),
}

impl<'a> From<ResolvedDiagnostics<'a>> for ErrorKind {
    fn from(diagnostics: ResolvedDiagnostics<'a>) -> Self {
        Self::Lang(diagnostics.to_string())
    }
}

#[derive(Debug, clap::Parser)]
struct Cli {
    #[clap(short, long)]
    program: Option<String>,
    #[clap(conflicts_with = "program")]
    file_program: Option<PathBuf>,
    #[clap(
        long,
        conflicts_with = "file_program",
        conflicts_with = "program",
        requires = "input",
        requires = "file_input"
    )]
    stdin_program: bool,
    #[clap(short, long)]
    input: Option<String>,
    #[clap(short, long, conflicts_with = "input")]
    file_input: Option<PathBuf>,
}

fn try_main(cli: Cli) -> Result<(), Error> {
    let program_source = if let Some(program) = cli.program {
        Source::Inline(program)
    } else if let Some(program_path) = cli.file_program {
        Source::File(program_path)
    } else {
        assert!(cli.stdin_program);
        Source::Stdin
    };

    let input_source = if let Some(input) = cli.input {
        Source::Inline(input)
    } else if let Some(input_path) = cli.file_input {
        Source::File(input_path)
    } else {
        assert!(!cli.stdin_program);
        Source::Stdin
    };

    let program = program_source.read()?;
    let input = input_source.read()?;

    let output =
        recipes::run_directly_on_ast(&program[..], &input[..], usize::MAX)
            .map_err(ErrorKind::from)
            .map_err(|kind| Error { kind, source: program_source.clone() })?
            .map_err(ErrorKind::from)
            .map_err(|kind| Error { kind, source: program_source })?;
    print!("{output}");
    Ok(())
}

fn main() {
    let cli = Cli::parse();
    if let Err(error) = try_main(cli) {
        eprintln!("{error}");
        let mut next = error.kind.source();
        while let Some(current) = next {
            eprintln!("Caused by:");
            eprintln!("  {current}");
            next = current.source();
        }
        process::exit(1);
    }
}
