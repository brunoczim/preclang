use std::{fmt, str::FromStr};

use thiserror::Error;

use crate::{
    compiler::Compiler,
    error::{Ice, ResolvedDiagnostics, Resolver},
    eval::Evaluate,
    ir,
    lexer::Lexer,
    parser::Parser,
    vm::Machine,
};

#[derive(Debug, Error)]
#[error("interpreter {0} is unknown")]
pub struct UnknownInterpreter(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Interpreter {
    Ast,
    Bytecode,
}

impl fmt::Display for Interpreter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ast => write!(f, "AST"),
            Self::Bytecode => write!(f, "bytecode"),
        }
    }
}

impl FromStr for Interpreter {
    type Err = UnknownInterpreter;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(if s.eq_ignore_ascii_case("AST") {
            Self::Ast
        } else if s.eq_ignore_ascii_case("bytecode") {
            Self::Bytecode
        } else {
            Err(UnknownInterpreter(s.to_owned()))?
        })
    }
}

pub fn run<'a>(
    code: &'a str,
    input: &str,
    parse_nest_limit: usize,
    interpreter: Interpreter,
) -> Result<Result<String, ResolvedDiagnostics<'a>>, Ice> {
    match interpreter {
        Interpreter::Ast => run_directly_on_ast(code, input, parse_nest_limit),
        Interpreter::Bytecode => run_on_bytecode(code, input, parse_nest_limit),
    }
}

pub fn run_directly_on_ast<'a>(
    code: &'a str,
    input: &str,
    parse_nest_limit: usize,
) -> Result<Result<String, ResolvedDiagnostics<'a>>, Ice> {
    let resolver = Resolver::new(code);
    let lexer = Lexer::new(code);
    let parser = Parser::new(lexer, parse_nest_limit);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => return Ok(Err(resolver.resolve_errors(errors)?)),
    };
    let (output, _) = ast.evaluate(input);
    Ok(Ok(output))
}

pub fn run_on_bytecode<'a>(
    code: &'a str,
    input: &str,
    parse_nest_limit: usize,
) -> Result<Result<String, ResolvedDiagnostics<'a>>, Ice> {
    let resolver = Resolver::new(code);
    let lexer = Lexer::new(code);
    let parser = Parser::new(lexer, parse_nest_limit);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => return Ok(Err(resolver.resolve_errors(errors)?)),
    };
    let compiler = Compiler::new();
    let program = compiler.compile(ast.data)?;
    let mut machine = Machine::new(program);
    machine.set_text(input);
    machine.run_until_end()?;
    let output = machine.into_text();
    Ok(Ok(output))
}

pub fn emit_asm<'a>(
    code: &'a str,
    parse_nest_limit: usize,
    expand_subs: bool,
) -> Result<Result<String, ResolvedDiagnostics<'a>>, Ice> {
    let resolver = Resolver::new(code);
    let lexer = Lexer::new(code);
    let parser = Parser::new(lexer, parse_nest_limit);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => return Ok(Err(resolver.resolve_errors(errors)?)),
    };
    let compiler = Compiler::new();
    let program = compiler.compile(ast.data)?;
    let emissor = ir::Emit { expand_subs, program: &program };
    Ok(Ok(emissor.to_string()))
}
