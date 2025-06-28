use crate::{
    compiler::Compiler,
    error::{Ice, ResolvedDiagnostics, Resolver},
    eval::Evaluate,
    lexer::Lexer,
    parser::Parser,
    vm::Machine,
};

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
