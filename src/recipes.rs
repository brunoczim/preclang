use crate::{
    error::{Ice, ResolvedDiagnostics, Resolver},
    eval::Evaluate,
    lexer::Lexer,
    parser::Parser,
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
