use nom::{
    IResult,
    Parser,
    branch::alt,
    combinator::{complete, fail, map, verify},
    multi::many1,
    sequence::{delimited, preceded, terminated},
};

use crate::{
    ast::{Conjunction, Disjunction, Expr, Negation, Sequence},
    lexer::{
        lex_operator_token,
        lex_punctuation_token,
        lex_substitution_token,
    },
    subs::Substitution,
    token::{Operator, Punctuation},
};

pub type Error<'a> = nom::error::Error<&'a str>;

pub fn parse_nesting<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = usize, Error = Error<'a>> {
    move |input| match limit.checked_sub(1) {
        Some(new_limit) => Ok((input, new_limit)),
        None => fail().parse(input),
    }
}

pub fn expect_punctuation<'a>(
    punct: Punctuation,
) -> impl Parser<&'a str, Output = Punctuation, Error = Error<'a>> {
    verify(lex_punctuation_token, move |parsed| punct == *parsed)
}

pub fn expect_operator<'a>(
    operator: Operator,
) -> impl Parser<&'a str, Output = Operator, Error = Error<'a>> {
    verify(lex_operator_token, move |parsed| operator == *parsed)
}

pub fn parse_substitution(input: &str) -> IResult<&str, Substitution> {
    lex_substitution_token(input)
}

pub fn parse_sequence<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = Sequence, Error = Error<'a>> {
    (
        many1(terminated(
            parse_expr(limit),
            expect_operator(Operator::Semicolon),
        )),
        parse_expr(limit),
    )
        .map(|(mut operands, last)| {
            operands.push(last);
            Sequence { operands }
        })
}

pub fn parse_disjunction<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = Disjunction, Error = Error<'a>> {
    (
        many1(terminated(parse_expr(limit), expect_operator(Operator::Pipe))),
        parse_expr(limit),
    )
        .map(|(mut operands, last)| {
            operands.push(last);
            Disjunction { operands }
        })
}

pub fn parse_conjunction<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = Conjunction, Error = Error<'a>> {
    (
        many1(terminated(
            parse_expr(limit),
            expect_operator(Operator::Ampersand),
        )),
        parse_expr(limit),
    )
        .map(|(mut operands, last)| {
            operands.push(last);
            Conjunction { operands }
        })
}

pub fn parse_negation<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = Negation, Error = Error<'a>> {
    preceded(expect_operator(Operator::Bang), parse_expr(limit))
        .map(|operand| Negation { operand: Box::new(operand) })
}

pub fn parse_expr<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = Expr, Error = Error<'a>> {
    move |input: &'a str| {
        parse_nesting(limit)
            .flat_map(|limit| {
                alt((
                    map(parse_sequence(limit), Expr::Sequence),
                    map(parse_disjunction(limit), Expr::Disjunction),
                    map(parse_conjunction(limit), Expr::Conjunction),
                    map(parse_negation(limit), Expr::Negation),
                    delimited(
                        expect_punctuation(Punctuation::OpenParen),
                        parse_expr(limit),
                        expect_punctuation(Punctuation::CloseParen),
                    ),
                    map(parse_substitution, Expr::Substitution),
                ))
            })
            .parse(input)
    }
}

pub fn parse_program<'a>(
    limit: usize,
) -> impl Parser<&'a str, Output = Expr, Error = Error<'a>> {
    complete(parse_expr(limit))
}

#[cfg(test)]
mod test {
    use nom::Parser;
    use regex::RegexBuilder;

    use crate::{
        ast::{Conjunction, Disjunction, Expr, Negation, Sequence},
        subs::{Flag, Flags, Fragment, Pattern, Replacement, Substitution},
    };

    use super::parse_program;

    #[test]
    fn parse_full_program_correctly() {
        let input = "! (s/foo/bar/g ; s/a/b/i ) | s/x(y)/z\\1/ & s/0/";
        let (rest, program) = parse_program(10).parse(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            program,
            Expr::Conjunction(Conjunction {
                operands: vec![
                    Expr::Disjunction(Disjunction {
                        operands: vec![
                            Expr::Negation(Negation {
                                operand: Box::new(Expr::Sequence(Sequence {
                                    operands: vec![
                                        Expr::Substitution(Substitution {
                                            pattern: Pattern {
                                                regex: RegexBuilder::new("foo")
                                                    .build()
                                                    .unwrap(),
                                            },
                                            flags: Flags::default()
                                                .with(Flag::Global),
                                            replacement: Replacement {
                                                fragments: vec![
                                                    Fragment::Text(
                                                        "bar".to_owned()
                                                    )
                                                ],
                                            }
                                        }),
                                        Expr::Substitution(Substitution {
                                            pattern: Pattern {
                                                regex: RegexBuilder::new("a")
                                                    .build()
                                                    .unwrap(),
                                            },
                                            flags: Flags::default()
                                                .with(Flag::Insensitive),
                                            replacement: Replacement {
                                                fragments: vec![
                                                    Fragment::Text(
                                                        "b".to_owned()
                                                    )
                                                ],
                                            }
                                        }),
                                    ],
                                }))
                            }),
                            Expr::Substitution(Substitution {
                                pattern: Pattern {
                                    regex: RegexBuilder::new("x(y)")
                                        .build()
                                        .unwrap(),
                                },
                                flags: Flags::default(),
                                replacement: Replacement {
                                    fragments: vec![
                                        Fragment::Text("z".to_owned()),
                                        Fragment::RefInt(1),
                                    ],
                                }
                            }),
                        ]
                    }),
                    Expr::Substitution(Substitution {
                        pattern: Pattern {
                            regex: RegexBuilder::new("0").build().unwrap(),
                        },
                        flags: Flags::default(),
                        replacement: Replacement {
                            fragments: vec![Fragment::Text("".to_owned())],
                        }
                    }),
                ],
            })
        );
    }
}
