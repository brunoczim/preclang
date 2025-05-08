use nom::{
    IResult,
    Parser,
    branch::alt,
    bytes::complete::{escaped, tag, take_while1},
    character::{
        self,
        complete::{alphanumeric0, anychar, char, multispace0, none_of},
    },
    combinator::{cut, opt, value, verify},
    multi::{fold_many0, fold_many1, many0},
    sequence::{delimited, preceded, terminated},
};
use regex::RegexBuilder;

use crate::{
    subs::{Flag, Flags, Fragment, Pattern, Replacement, Substitution},
    token::{Operator, Punctuation, Token},
};

fn wrap_token_lexing<'a, P>(
    parser: P,
) -> impl Parser<&'a str, Output = P::Output, Error = P::Error>
where
    P: Parser<&'a str>,
{
    delimited(multispace0, parser, multispace0)
}

pub fn lex_flag(input: &str) -> IResult<&str, Flag> {
    alt((
        value(Flag::Global, char('g')),
        value(Flag::Insensitive, char('i')),
        value(Flag::Multiline, char('m')),
        value(Flag::Unicode, char('u')),
        value(Flag::DotNewline, char('s')),
        value(Flag::Crlf, char('r')),
        value(Flag::SwapGreed, char('G')),
        value(Flag::IgnoreWhitespace, char('S')),
    ))
    .parse(input)
}

pub fn lex_flags(input: &str) -> IResult<&str, Flags> {
    terminated(
        fold_many0(lex_flag, Flags::default, |flags, flag| flags.with(flag)),
        cut(verify(alphanumeric0, str::is_empty)),
    )
    .parse(input)
}

pub fn lex_pattern_str(input: &str) -> IResult<&str, &str> {
    escaped(none_of("\\/"), '\\', anychar).parse(input)
}

pub fn lex_fragment(input: &str) -> IResult<&str, Fragment> {
    alt((
        preceded(char('\\'), character::complete::u32.map(Fragment::RefInt)),
        preceded(
            char('\\'),
            delimited(
                char('{'),
                character::complete::u32.map(Fragment::RefInt),
                char('}'),
            ),
        ),
        preceded(
            char('\\'),
            delimited(
                char('{'),
                character::complete::alphanumeric1
                    .map(|s: &str| Fragment::RefName(s.to_owned())),
                char('}'),
            ),
        ),
        preceded(char('\\'), anychar.map(|ch| Fragment::Text(ch.to_string()))),
        fold_many1(none_of("\\/"), String::new, |mut acc, ch| {
            acc.push(ch);
            acc
        })
        .map(Fragment::Text),
    ))
    .parse(input)
}

pub fn lex_replacement(input: &str) -> IResult<&str, Replacement> {
    many0(lex_fragment).map(|fragments| Replacement { fragments }).parse(input)
}

pub fn lex_substitution_token(input: &str) -> IResult<&str, Substitution> {
    let parser = preceded(
        tag("s/"),
        (
            lex_pattern_str,
            preceded(char('/'), lex_replacement),
            opt(preceded(char('/'), opt(lex_flags))),
        ),
    )
    .map_res(|(pattern_str, replacement, maybe_flags)| {
        let flags = maybe_flags.flatten().unwrap_or_default();
        let mut regex_builder = RegexBuilder::new(pattern_str);
        regex_builder
            .crlf(flags.test(Flag::Crlf))
            .case_insensitive(flags.test(Flag::Insensitive))
            .ignore_whitespace(flags.test(Flag::IgnoreWhitespace))
            .swap_greed(flags.test(Flag::SwapGreed))
            .unicode(flags.test(Flag::Unicode))
            .multi_line(flags.test(Flag::Multiline))
            .dot_matches_new_line(flags.test(Flag::DotNewline));
        let regex = regex_builder.build()?;
        Result::<_, regex::Error>::Ok(Substitution {
            pattern: Pattern { regex },
            replacement,
            flags,
        })
    });
    wrap_token_lexing(parser).parse(input)
}

pub fn lex_punctuation_token(input: &str) -> IResult<&str, Punctuation> {
    let parser = alt((
        value(Punctuation::OpenParen, char('(')),
        value(Punctuation::CloseParen, char(')')),
    ));
    wrap_token_lexing(parser).parse(input)
}

pub fn lex_operator_token(input: &str) -> IResult<&str, Operator> {
    let parser =
        take_while1(|ch: char| "&|!;".contains(ch)).map_opt(|tag: &str| {
            Some(match tag {
                ";" => Operator::Semicolon,
                "|" => Operator::Pipe,
                "&" => Operator::Ampersand,
                "!" => Operator::Bang,
                _ => None?,
            })
        });
    wrap_token_lexing(parser).parse(input)
}

pub fn lex_token(input: &str) -> IResult<&str, Token> {
    alt((
        lex_substitution_token.map(Token::Substitution),
        lex_punctuation_token.map(Token::Punct),
        lex_operator_token.map(Token::Operator),
    ))
    .parse(input)
}

#[cfg(test)]
mod test {
    use nom::Parser;

    use crate::{
        lexer::lex_operator_token,
        subs::{Flag, Flags, Fragment, Replacement},
        token::{Operator, Punctuation},
    };

    use super::{
        lex_pattern_str,
        lex_punctuation_token,
        lex_replacement,
        lex_substitution_token,
    };

    #[test]
    fn test_lex_pattern_simple() {
        lex_pattern_str("abc").unwrap();
    }

    #[test]
    fn test_lex_replacement_simple() {
        lex_replacement("def").unwrap();
    }

    #[test]
    fn test_lex_flags_simple() {
        lex_replacement("gm").unwrap();
    }

    #[test]
    fn test_lex_substitution_without_flags() {
        let (input, subs) = lex_substitution_token("s/abc/def").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement { fragments: vec![Fragment::Text("def".to_owned()),] }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_without_flags_trailing_slash() {
        let (input, subs) = lex_substitution_token("s/abc/def/").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement { fragments: vec![Fragment::Text("def".to_owned()),] }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_without_flags_replacement_empty() {
        let (input, subs) = lex_substitution_token("s/abc/").unwrap();
        assert_eq!(input, "");
        assert_eq!(subs.replacement, Replacement { fragments: Vec::new() });
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_without_flags_trailing_slash_repl_empty() {
        let (input, subs) = lex_substitution_token("s/abc//").unwrap();
        assert_eq!(input, "");
        assert_eq!(subs.replacement, Replacement { fragments: Vec::new() });
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_with_flags() {
        let (input, subs) = lex_substitution_token("s/abc/def/gm").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement { fragments: vec![Fragment::Text("def".to_owned()),] }
        );
        assert_eq!(
            subs.flags,
            Flags::default().with(Flag::Global).with(Flag::Multiline),
        );
    }

    #[test]
    fn test_lex_substitution_invalid_trailing_flags() {
        lex_substitution_token("s/abc/def/gA").unwrap_err();
    }

    #[test]
    fn test_lex_substitution_two_valid() {
        let (input_0, subs_0) =
            lex_substitution_token.parse("s/abc/def/g s/123/456/").unwrap();
        let (input_1, subs_1) = lex_substitution_token.parse(input_0).unwrap();
        assert_eq!(input_1, "");
        assert_eq!(
            subs_0.replacement,
            Replacement { fragments: vec![Fragment::Text("def".to_owned()),] }
        );
        assert_eq!(subs_0.flags, Flags::default().with(Flag::Global),);
        assert_eq!(
            subs_1.replacement,
            Replacement { fragments: vec![Fragment::Text("456".to_owned()),] }
        );
        assert_eq!(subs_1.flags, Flags::default(),);
    }

    #[test]
    fn test_lex_substitution_with_pattern_escapes() {
        let (input, subs) = lex_substitution_token("s/a\\/bc/def").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement { fragments: vec![Fragment::Text("def".to_owned()),] }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_with_pattern_escapes_escaper() {
        let (input, subs) = lex_substitution_token("s/a\\\\bc/def").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement { fragments: vec![Fragment::Text("def".to_owned()),] }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_with_replacement_fragment_number() {
        let (input, subs) = lex_substitution_token("s/abc/d\\13ef").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement {
                fragments: vec![
                    Fragment::Text("d".to_owned()),
                    Fragment::RefInt(13),
                    Fragment::Text("ef".to_owned()),
                ]
            }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_with_replacement_fragment_number_curly() {
        let (input, subs) = lex_substitution_token("s/abc/d\\{13}2ef").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement {
                fragments: vec![
                    Fragment::Text("d".to_owned()),
                    Fragment::RefInt(13),
                    Fragment::Text("2ef".to_owned()),
                ]
            }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_with_replacement_fragment_name() {
        let (input, subs) =
            lex_substitution_token("s/abc/d\\{foo}2ef").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement {
                fragments: vec![
                    Fragment::Text("d".to_owned()),
                    Fragment::RefName("foo".to_owned()),
                    Fragment::Text("2ef".to_owned()),
                ]
            }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_with_replacement_fragments() {
        let (input, subs) =
            lex_substitution_token("s/abc/d\\{foo}2ef\\30.\\{31}3\\\\")
                .unwrap();
        assert_eq!(input, "");
        assert_eq!(
            subs.replacement,
            Replacement {
                fragments: vec![
                    Fragment::Text("d".to_owned()),
                    Fragment::RefName("foo".to_owned()),
                    Fragment::Text("2ef".to_owned()),
                    Fragment::RefInt(30),
                    Fragment::Text(".".to_owned()),
                    Fragment::RefInt(31),
                    Fragment::Text("3".to_owned()),
                    Fragment::Text("\\".to_owned()),
                ]
            }
        );
        assert_eq!(subs.flags, Flags::default());
    }

    #[test]
    fn test_lex_substitution_correct_regex() {
        let (input, subs) = lex_substitution_token("s/abc\\/*\\\\3//").unwrap();
        assert_eq!(input, "");
        assert_eq!(subs.replacement, Replacement { fragments: Vec::new() });
        assert_eq!(subs.flags, Flags::default());

        assert!(subs.pattern.regex.is_match("abc\\3"));
        assert!(subs.pattern.regex.is_match("abc/\\3"));
        assert!(subs.pattern.regex.is_match("abc//\\3"));
        assert!(!subs.pattern.regex.is_match("abc//3"));
        assert!(!subs.pattern.regex.is_match("abc/3"));
        assert!(!subs.pattern.regex.is_match("xyz"));
    }

    #[test]
    fn test_lex_punctuation() {
        let (input, punct) = lex_punctuation_token("( )").unwrap();
        assert_eq!(punct, Punctuation::OpenParen);
        let (_, punct) = lex_punctuation_token(input).unwrap();
        assert_eq!(punct, Punctuation::CloseParen);
    }

    #[test]
    fn test_lex_operator() {
        let (input, operator) = lex_operator_token("; | & !").unwrap();
        assert_eq!(operator, Operator::Semicolon);
        let (input, operator) = lex_operator_token(input).unwrap();
        assert_eq!(operator, Operator::Pipe);
        let (input, operator) = lex_operator_token(input).unwrap();
        assert_eq!(operator, Operator::Ampersand);
        let (_, operator) = lex_operator_token(input).unwrap();
        assert_eq!(operator, Operator::Bang);
    }
}
