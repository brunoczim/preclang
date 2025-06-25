use std::mem;

use thiserror::Error;

use crate::{
    ast::Expr,
    error::LangError,
    lexer::{LexError, Lexer},
    location::{Location, Span, Spanned},
    token::{BinaryOperator, Operator, PrefixOperator, Punctuation, Token},
};

fn bin_precedence(operator: BinaryOperator) -> i32 {
    match operator {
        BinaryOperator::Semicolon => 0,
        BinaryOperator::Pipe => 100,
        BinaryOperator::Ampersand => 200,
    }
}

fn pre_precedence(operator: PrefixOperator) -> i32 {
    match operator {
        PrefixOperator::Bang => 300,
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error(transparent)]
    Lex(LexError),
    #[error("too much nesting")]
    NestLimit(Location),
    #[error("unmatched open parenthesis")]
    UnmatchedOpenParen(Span),
    #[error("bad operand position")]
    BadOperand(Location),
    #[error("bad operator position")]
    BadOperator(Location),
    #[error("missing operand")]
    MissingOperand(Location),
    #[error("missing operator")]
    MissingOperator(Location),
}

impl LangError for ParseError {
    fn span(&self) -> Span {
        match self {
            Self::Lex(error) => error.span(),
            Self::NestLimit(start) => Span::unitary(*start),
            Self::UnmatchedOpenParen(span) => *span,
            Self::BadOperand(start) => Span::unitary(*start),
            Self::BadOperator(start) => Span::unitary(*start),
            Self::MissingOperand(start) => Span::unitary(*start),
            Self::MissingOperator(start) => Span::unitary(*start),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseFailed;

#[derive(Debug, Clone, PartialEq, Eq)]
enum ExprItem {
    Operand(Spanned<Expr>),
    Operator(Spanned<Operator>),
}

#[derive(Debug, Clone)]
struct ExprFrame {
    open_paren: Span,
    stack: Vec<ExprItem>,
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    location: usize,
    limit: usize,
    errors: Vec<ParseError>,
    expr_stacks: Vec<ExprFrame>,
    expr_stack: Vec<ExprItem>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, limit: usize) -> Self {
        Self {
            lexer,
            limit,
            location: 0,
            errors: Vec::new(),
            expr_stacks: Vec::new(),
            expr_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Spanned<Expr>, Vec<ParseError>> {
        self.parse_expr().map_err(|_| self.errors)
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseFailed> {
        loop {
            let Some(token) = self.next_token()? else {
                if let Some(frame) = self.expr_stacks.last() {
                    self.errors
                        .push(ParseError::UnmatchedOpenParen(frame.open_paren));
                    Err(ParseFailed)?
                }
                return self.finish_expr();
            };
            match token.data {
                Token::Punct(Punctuation::OpenParen) => {
                    self.enter(token.span)?;
                },
                Token::Punct(Punctuation::CloseParen) => {
                    self.leave()?;
                },
                Token::Substitution(subs) => {
                    self.push_operand(Expr::new_subs(token.span, subs))?;
                },
                Token::Operator(operator) => {
                    self.push_operator(Spanned {
                        data: operator,
                        span: token.span,
                    })?;
                },
            }
        }
    }

    fn next_token(&mut self) -> Result<Option<Spanned<Token>>, ParseFailed> {
        let Some(result) = self.lexer.next() else {
            return Ok(None);
        };
        let token = match result {
            Ok(tok) => tok,
            Err(error) => {
                self.errors.push(ParseError::Lex(error));
                Err(ParseFailed)?
            },
        };
        self.location = token.span.start;
        Ok(Some(token))
    }

    fn enter(&mut self, open_paren: Span) -> Result<(), ParseFailed> {
        if self.limit <= self.expr_stacks.len() {
            self.errors.push(ParseError::NestLimit(self.location));
            Err(ParseFailed)?
        }
        self.expr_stacks.push(ExprFrame {
            open_paren,
            stack: mem::take(&mut self.expr_stack),
        });
        Ok(())
    }

    fn leave(&mut self) -> Result<(), ParseFailed> {
        let finish_result = self.finish_expr();
        let Some(frame) = self.expr_stacks.pop() else {
            self.expr_stack = Vec::new();
            self.errors.push(ParseError::NestLimit(self.location));
            Err(ParseFailed)?
        };
        self.expr_stack = frame.stack;
        if let Ok(expr) = finish_result {
            self.push_operand(expr)?;
        }
        Ok(())
    }

    fn push_operand(
        &mut self,
        operand: Spanned<Expr>,
    ) -> Result<(), ParseFailed> {
        if let Some(prev_item) = self.expr_stack.last() {
            match prev_item {
                ExprItem::Operand(_) => {
                    self.errors
                        .push(ParseError::MissingOperator(operand.span.start));
                    Err(ParseFailed)?
                },
                ExprItem::Operator(_) => (),
            }
        }

        self.expr_stack.push(ExprItem::Operand(operand));

        Ok(())
    }

    fn push_operator(
        &mut self,
        operator: Spanned<Operator>,
    ) -> Result<(), ParseFailed> {
        match operator.data {
            Operator::Prefix(pre) => self.push_prefix_operator(Spanned {
                span: operator.span,
                data: pre,
            }),

            Operator::Binary(bin) => self.push_binary_operator(Spanned {
                span: operator.span,
                data: bin,
            }),
        }
    }

    fn push_prefix_operator(
        &mut self,
        operator: Spanned<PrefixOperator>,
    ) -> Result<(), ParseFailed> {
        if let Some(ExprItem::Operand(_)) = self.expr_stack.last() {
            self.errors.push(ParseError::BadOperator(operator.span.start));
            Err(ParseFailed)?
        }
        self.expr_stack.push(ExprItem::Operator(Spanned {
            data: Operator::Prefix(operator.data),
            span: operator.span,
        }));
        Ok(())
    }

    fn push_binary_operator(
        &mut self,
        operator: Spanned<BinaryOperator>,
    ) -> Result<(), ParseFailed> {
        let Some(prev_item) = self.expr_stack.last() else {
            self.errors.push(ParseError::MissingOperand(operator.span.start));
            Err(ParseFailed)?
        };
        match prev_item {
            ExprItem::Operator(optor) => {
                self.errors.push(ParseError::MissingOperand(optor.span.start));
                Err(ParseFailed)?
            },
            ExprItem::Operand(_) => (),
        }
        self.reduce_expr(bin_precedence(operator.data))?;
        self.expr_stack.push(ExprItem::Operator(Spanned {
            data: Operator::Binary(operator.data),
            span: operator.span,
        }));
        Ok(())
    }

    fn reduce_expr(&mut self, min_precedence: i32) -> Result<(), ParseFailed> {
        let Some(item) = self.expr_stack.pop() else { return Ok(()) };

        let mut operand = match item {
            ExprItem::Operand(op) => op,
            ExprItem::Operator(operator) => {
                self.expr_stack.push(ExprItem::Operator(operator));
                return Ok(());
            },
        };

        loop {
            if let Some(ExprItem::Operator(operator)) = self.expr_stack.pop() {
                match operator.data {
                    Operator::Prefix(pre_optor) => {
                        if pre_precedence(pre_optor) >= min_precedence {
                            operand = Expr::new_pre_op(
                                Spanned {
                                    span: operator.span,
                                    data: pre_optor,
                                },
                                operand,
                            );
                            continue;
                        }
                    },

                    Operator::Binary(bin_optor) => {
                        if bin_precedence(bin_optor) >= min_precedence {
                            let Some(ExprItem::Operand(lhs)) =
                                self.expr_stack.pop()
                            else {
                                self.errors.push(ParseError::MissingOperand(
                                    operator.span.start,
                                ));
                                Err(ParseFailed)?
                            };

                            operand = Expr::new_bin_op(
                                lhs,
                                Spanned {
                                    data: bin_optor,
                                    span: operator.span,
                                },
                                operand,
                            );
                            continue;
                        }
                    },
                }

                self.expr_stack.push(ExprItem::Operator(operator));
            }

            break;
        }
        self.expr_stack.push(ExprItem::Operand(operand));
        Ok(())
    }

    fn finish_expr(&mut self) -> Result<Spanned<Expr>, ParseFailed> {
        self.reduce_expr(i32::MIN)?;

        let Some(item) = self.expr_stack.pop() else {
            self.errors.push(ParseError::MissingOperand(self.location));
            Err(ParseFailed)?
        };

        match item {
            ExprItem::Operator(optor) => {
                self.errors.push(ParseError::BadOperator(optor.span.start));
                Err(ParseFailed)?
            },

            ExprItem::Operand(operand) => Ok(operand),
        }
    }
}

#[cfg(test)]
mod test {
    use regex::RegexBuilder;

    use crate::{
        ast::{BinaryOperation, Expr, PrefixOperation},
        lexer::Lexer,
        location::Spanned,
        subs::{Flag, Flags, Fragment, Pattern, Replacement, Substitution},
        token::{BinaryOperator, PrefixOperator},
    };

    use super::Parser;

    #[test]
    fn parse_subs() {
        let source = "s/abc/def/g";
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let subs = Box::new(Substitution {
            flags: Flags::default().with(Flag::Global),
            pattern: Pattern {
                regex: RegexBuilder::new("abc").build().unwrap(),
            },
            replacement: Replacement {
                fragments: vec![Fragment::Text("def".to_owned())],
            },
        });
        let expected = Spanned::anywhere(Expr::Substitution(subs));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_or() {
        let source = r##"s/abc/def/g | s/123/\2"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let lhs =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        let rhs =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("123").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::RefInt(2)],
                },
            })));
        let bin_operation = Box::new(BinaryOperation {
            lhs,
            operator: Spanned::anywhere(BinaryOperator::Pipe),
            rhs,
        });
        let expected = Spanned::anywhere(Expr::BinaryOperation(bin_operation));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_and() {
        let source = r##"s/abc/def/g & s/123/\2"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let lhs =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        let rhs =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("123").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::RefInt(2)],
                },
            })));
        let bin_operation = Box::new(BinaryOperation {
            lhs,
            operator: Spanned::anywhere(BinaryOperator::Ampersand),
            rhs,
        });
        let expected = Spanned::anywhere(Expr::BinaryOperation(bin_operation));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_semicolon() {
        let source = r##"s/abc/def/g ; s/123/\2"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let lhs =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        let rhs =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("123").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::RefInt(2)],
                },
            })));
        let bin_operation = Box::new(BinaryOperation {
            lhs,
            operator: Spanned::anywhere(BinaryOperator::Semicolon),
            rhs,
        });
        let expected = Spanned::anywhere(Expr::BinaryOperation(bin_operation));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_not() {
        let source = r##"! s/abc/def/g"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let operand =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        let pre_operation = Box::new(PrefixOperation {
            operand,
            operator: Spanned::anywhere(PrefixOperator::Bang),
        });
        let expected = Spanned::anywhere(Expr::PrefixOperation(pre_operation));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_naive_parens() {
        let source = r##"(s/abc/def/g)"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let expected =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_naive_double_parens() {
        let source = r##"((s/abc/def/g))"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let expected =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_precedence() {
        let source =
            r##"s/abc/def/g ; s/123/\2/ & s/a/0/ | s/// & ! ! s/xy/./"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let subs_abcdef =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        let subs_123 =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("123").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::RefInt(2)],
                },
            })));
        let subs_a0 =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("a").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("0".to_owned())],
                },
            })));
        let subs_empty =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("").build().unwrap(),
                },
                replacement: Replacement { fragments: Vec::new() },
            })));
        let subs_xy =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("xy").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text(".".to_owned())],
                },
            })));

        let expected = Spanned::anywhere(Expr::BinaryOperation(Box::new(
            BinaryOperation {
                operator: Spanned::anywhere(BinaryOperator::Semicolon),
                lhs: subs_abcdef,
                rhs: Spanned::anywhere(Expr::BinaryOperation(Box::new(
                    BinaryOperation {
                        operator: Spanned::anywhere(BinaryOperator::Pipe),
                        lhs: Spanned::anywhere(Expr::BinaryOperation(
                            Box::new(BinaryOperation {
                                operator: Spanned::anywhere(
                                    BinaryOperator::Ampersand,
                                ),
                                lhs: subs_123,
                                rhs: subs_a0,
                            }),
                        )),
                        rhs: Spanned::anywhere(Expr::BinaryOperation(
                            Box::new(BinaryOperation {
                                operator: Spanned::anywhere(
                                    BinaryOperator::Ampersand,
                                ),
                                lhs: subs_empty,
                                rhs: Spanned::anywhere(Expr::PrefixOperation(
                                    Box::new(PrefixOperation {
                                        operator: Spanned::anywhere(
                                            PrefixOperator::Bang,
                                        ),
                                        operand: Spanned::anywhere(
                                            Expr::PrefixOperation(Box::new(
                                                PrefixOperation {
                                                    operator: Spanned::anywhere(
                                                        PrefixOperator::Bang,
                                                    ),
                                                    operand: subs_xy,
                                                },
                                            )),
                                        ),
                                    }),
                                )),
                            }),
                        )),
                    },
                ))),
            },
        )));
        assert_spanless_eq!(actual, expected);
    }

    #[test]
    fn parse_good_parens() {
        let source =
            r##"(s/abc/def/g ; s/123/\2/ & !(s/a/0/ | s///)) & s/xy/./"##;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer, usize::MAX);
        let actual = parser.parse().unwrap();

        let subs_abcdef =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default().with(Flag::Global),
                pattern: Pattern {
                    regex: RegexBuilder::new("abc").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("def".to_owned())],
                },
            })));
        let subs_123 =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("123").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::RefInt(2)],
                },
            })));
        let subs_a0 =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("a").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text("0".to_owned())],
                },
            })));
        let subs_empty =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("").build().unwrap(),
                },
                replacement: Replacement { fragments: Vec::new() },
            })));
        let subs_xy =
            Spanned::anywhere(Expr::Substitution(Box::new(Substitution {
                flags: Flags::default(),
                pattern: Pattern {
                    regex: RegexBuilder::new("xy").build().unwrap(),
                },
                replacement: Replacement {
                    fragments: vec![Fragment::Text(".".to_owned())],
                },
            })));

        let expected = Spanned::anywhere(Expr::BinaryOperation(Box::new(
            BinaryOperation {
                operator: Spanned::anywhere(BinaryOperator::Ampersand),
                lhs: Spanned::anywhere(Expr::BinaryOperation(Box::new(
                    BinaryOperation {
                        operator: Spanned::anywhere(BinaryOperator::Semicolon),
                        lhs: subs_abcdef,
                        rhs: Spanned::anywhere(Expr::BinaryOperation(
                            Box::new(BinaryOperation {
                                operator: Spanned::anywhere(
                                    BinaryOperator::Ampersand,
                                ),
                                lhs: subs_123,
                                rhs: Spanned::anywhere(Expr::PrefixOperation(
                                    Box::new(PrefixOperation {
                                        operator: Spanned::anywhere(
                                            PrefixOperator::Bang,
                                        ),
                                        operand: Spanned::anywhere(
                                            Expr::BinaryOperation(Box::new(
                                                BinaryOperation {
                                                    operator: Spanned::anywhere(
                                                        BinaryOperator::Pipe,
                                                    ),
                                                    lhs: subs_a0,
                                                    rhs: subs_empty,
                                                },
                                            )),
                                        ),
                                    }),
                                )),
                            }),
                        )),
                    },
                ))),
                rhs: subs_xy,
            },
        )));
        assert_spanless_eq!(actual, expected);
    }
}
