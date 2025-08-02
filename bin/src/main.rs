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
    recipes::{self, Interpreter, emit_asm},
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
    pub fn read<'a>(&'a self) -> Result<Cow<'a, str>, Error> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum InterpreterArg {
    Ast,
    Bytecode,
}

impl From<InterpreterArg> for Interpreter {
    fn from(value: InterpreterArg) -> Self {
        match value {
            InterpreterArg::Ast => Self::Ast,
            InterpreterArg::Bytecode => Self::Bytecode,
        }
    }
}

/// Executes programs of the Pure Regular Expression Combinator Language
/// (Preclang).
///
/// By default, source code is provided through a file, whose path is provided
/// as a positional argument, and input text is provided through the standard
/// input.
#[derive(Debug, clap::Parser)]
struct Cli {
    /// Inline source code.
    ///
    /// If provided, positional source code path will not be accepted.
    /// Standard input will still be available to input text.
    #[clap(short, long, value_name = "CODE")]
    program: Option<String>,
    /// Path to the file containing the source code.
    ///
    /// Accepted and required only if no other means of providing source code
    /// is specified.
    #[clap(conflicts_with = "program", value_name = "PROGRAM_PATH")]
    file_program: Option<PathBuf>,
    /// Uses standard input to receive source code.
    ///
    /// If provided, neither positional source code path nor inline source code
    /// will be accepted.
    /// Standard input will not be available to input text and alternative
    /// means must be used to provide it.
    #[clap(
        long,
        conflicts_with = "file_program",
        conflicts_with = "program",
        requires = "input",
        requires = "file_input"
    )]
    stdin_program: bool,
    /// Inline input text.
    ///
    /// If provided, standard input will not be used to receive input text,
    /// and path to file will also not be accepted as means to provide it.
    #[clap(short, long, value_name = "INPUT")]
    input: Option<String>,
    /// Path to file containing input text.
    ///
    /// If provided, standard input will not be used to receive input text,
    /// and the inline argument will also not be accepted as means to provide
    /// it.
    #[clap(short, long, value_name = "INPUT_PATH", conflicts_with = "input")]
    file_input: Option<PathBuf>,
    /// Which interpreter method to use.
    ///
    /// ast - interpret by directly walking through the AST.
    ///
    /// bytecode - interpret by first compiling the AST to bytecode and then
    ///            executing the bytecode.
    #[clap(
        short = 'I',
        long,
        value_name = "INTERPRETER",
        default_value = "ast"
    )]
    interpreter: InterpreterArg,
    /// Emits textual representation of bytecode.
    ///
    /// If provided, input text will not be accepted and the programm will not
    /// be  executed.
    #[clap(
        short = 'S',
        long,
        conflicts_with = "input",
        conflicts_with = "file_input",
        conflicts_with = "interpreter"
    )]
    emit_asm: bool,
    /// Expands substitution tokens in textual representation of byte.
    ///
    /// This requires assembly emission mode.
    /// By default, emitting assembly will print numeric identifiers of
    /// substitution; this option enables expanding it directly into
    /// instructions instead.
    #[clap(short = 'x', long, requires = "emit_asm")]
    expand_subs: bool,
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

    let program = program_source.read()?;

    let result = if cli.emit_asm {
        emit_asm(&program, usize::MAX, cli.expand_subs)
    } else {
        let input_source = if let Some(input) = cli.input {
            Source::Inline(input)
        } else if let Some(input_path) = cli.file_input {
            Source::File(input_path)
        } else {
            assert!(!cli.stdin_program);
            Source::Stdin
        };

        let input = input_source.read()?;
        let interpreter = cli.interpreter.into();

        recipes::run(&program[..], &input[..], usize::MAX, interpreter)
    };

    let output = result
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
