use std::{collections::HashMap, str::FromStr};

use chumsky::{
    input::{MapExtra, Stream, ValueInput},
    prelude::*,
};
use malachite::Natural;

use crate::{StaticError, types};

use logos::Logos;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
    PosIntegerOverflow,
    NegIntegerOverflow,
    #[default]
    Other,
}

impl From<std::num::ParseIntError> for LexingError {
    fn from(err: std::num::ParseIntError) -> Self {
        match err.kind() {
            std::num::IntErrorKind::PosOverflow => Self::PosIntegerOverflow,
            std::num::IntErrorKind::NegOverflow => Self::NegIntegerOverflow,
            _ => Self::Other,
        }
    }
}

impl std::fmt::Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexingError::PosIntegerOverflow => write!(f, "Integer caused overflow (positive)"),
            LexingError::NegIntegerOverflow => write!(f, "Integer caused overflow (negative)"),
            LexingError::Other => write!(f, "Unexpected Token"),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"([ \t\n\f]|%[^%]*%)+")] // Ignore this regex pattern between tokens
#[logos(error = LexingError)]
pub(super) enum Token<'a> {
    #[regex("[a-z][a-zA-Z0-9_]*")]
    IdentLower(&'a str),
    #[regex("[A-Z][a-zA-Z0-9_]*")]
    IdentUpper(&'a str),
    #[regex("[0-9]+", |lex| Natural::from_str(lex.slice()).ok())]
    Integer(Natural),
    #[token(",")]
    Comma,
    #[token("..")]
    Range,
    #[token(".")]
    Dot,
    #[token("[")]
    LSqBracket,
    #[token("]")]
    RSqBracket,
    #[token("(")]
    LBracket,
    #[token(")")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("|")]
    Bar,
    #[token(":")]
    Colon,
    #[token("::")]
    Append,
    #[token("->")]
    RArrow,
    #[token("=>")]
    RDArrow,
    #[token("+")]
    Add,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("<")]
    LAngleBracket,
    #[token(">")]
    RAngleBracket,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("==")]
    EqualTo,
    #[token("!=")]
    NotEqualTo,
    #[token("=")]
    Bind,
    #[token("match")]
    Match,
    #[token("as")]
    As,
    #[token("let")]
    Let,
    #[token("box")]
    Box,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("fix")]
    Fix,
    #[token("mfix")]
    MFix,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("type")]
    Type,
}

#[derive(Debug)]
pub struct Program {
    pub typedefs: HashMap<String, Vec<(String, types::Type)>>,
    pub term: Term,
}

#[derive(Debug)]
pub enum TermType {
    ArrLiteral(types::Type, Box<[Term]>),
    Tuple(Box<[Term]>),
    BoolLiteral(bool),
    IntLiteral(Natural),
    Bracketed(Box<Term>),
    Box(Box<Term>),
    Variable(Ident),
    Function(Func),
    Application(Application),
    Length(Box<Term>),
    IfElse(IfElse),
    LetBinding(LetBinding),
    LetBoxBinding(LetBinding),
    Fix(Fix),
    MFix(Fix),
    UnaryMinus(Box<Term>),
    BinaryPrimitive(BinaryPrimitive),
    Append(Append),
    Index(Index),
    Slice(Slice),
    TupleIndex(TupleIndex),
    Ascription(Box<Term>, types::Type),
    Ctor(Ctor),
    Match(Box<Term>, Vec<MatchArm>),
}

#[derive(Debug)]
pub struct Term {
    pub ty: TermType,
    pub span: SimpleSpan,
}

#[derive(Debug)]
pub struct Func {
    pub binding: Ident,
    pub arg_type: types::Type,
    pub body: Box<Term>,
}

#[derive(Debug)]
pub struct Fix {
    pub fix_binding: Ident,
    pub inner_binding: Ident,
    pub arg_type: types::Type,
    pub ret_type: types::Type,
    pub body: Box<Term>,
}

#[derive(Debug)]
pub struct Application {
    pub func: Box<Term>,
    pub arg: Box<Term>,
}

#[derive(Debug)]
pub struct IfElse {
    pub cond: Box<Term>,
    pub if_true: Box<Term>,
    pub if_false: Box<Term>,
}

#[derive(Debug)]
pub struct Ctor {
    pub ctor: Ident,
    pub term: Box<Term>,
}

#[derive(Debug)]
pub struct MatchArm {
    pub ctor: Ident,
    pub ident: Ident,
    pub body: Term,
}

#[derive(Debug)]
pub struct LetBinding {
    pub binding: Ident,
    pub expr: Box<Term>,
    pub body: Box<Term>,
}

#[derive(Debug)]
pub struct Append {
    pub list: Box<Term>,
    pub item: Box<Term>,
}

#[derive(Debug)]
pub struct Index {
    pub list: Box<Term>,
    pub index: Box<Term>,
}

#[derive(Debug)]
pub struct Slice {
    pub list: Box<Term>,
    pub lower: Option<Box<Term>>,
    pub upper: Option<Box<Term>>,
}

#[derive(Debug)]
pub struct TupleIndex {
    pub tup: Box<Term>,
    pub index: (Natural, SimpleSpan),
}

#[derive(Debug)]
pub struct Ident {
    pub ident: String,
    pub span: SimpleSpan,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Debug)]
pub struct BinaryPrimitive {
    pub op: BinaryOp,
    pub lhs: Box<Term>,
    pub rhs: Box<Term>,
}

type Full<'a> = extra::Full<Rich<'a, Token<'a>>, (), ()>;

pub fn parse_type<'a, I>() -> impl Clone + Parser<'a, I, types::Type, Full<'a>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(
        |ty: chumsky::recursive::Recursive<dyn chumsky::Parser<'_, I, types::Type, Full<'a>>>| {
            let atom = choice((
                ty.clone()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .map(|ty: types::Type| types::Type::List(ty.into())),
                ty.clone()
                    .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
                    .map(|ty: types::Type| types::Type::Mobile(ty.into())),
                ty.clone()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map(|ty: types::Type| types::Type::List(ty.into())),
                ty.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LAngleBracket), just(Token::RAngleBracket))
                    .map(|tys| types::Type::Tuple(tys.into())),
                select! {
                    Token::IdentUpper("Bool") => types::Type::Bool,
                    Token::IdentUpper("Int") => types::Type::Int,
                    Token::IdentUpper(name) => types::Type::Sum(name.to_string().into()),
                },
            ));

            atom.clone()
                .then(just(Token::RArrow).ignore_then(ty.clone()).or_not())
                .map(|(a, b)| {
                    if let Some(tgt) = b {
                        types::Type::Func(a.into(), tgt.into())
                    } else {
                        a
                    }
                })
        },
    )
}

pub fn parse_term<'a, I>() -> impl Parser<'a, I, Term, Full<'a>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let parse_int = select! { Token::Integer(val) => val };
    let parse_bool = select! {
        Token::True => true,
        Token::False => false,
    };
    let parse_ident_lower = (select! { Token::IdentLower(name) => name }).try_map_with(|v, e| {
        Ok(Ident {
            ident: String::from(v),
            span: e.span(),
        })
    });
    let parse_ident_upper = (select! { Token::IdentUpper(name) => name }).try_map_with(|v, e| {
        Ok(Ident {
            ident: String::from(v),
            span: e.span(),
        })
    });

    recursive(|term| {
        let parse_box = just(Token::Box).ignore_then(term.clone());

        let bracketed = term
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket));

        let parse_signature = parse_ident_lower
            .then_ignore(just(Token::Colon))
            .then(parse_type())
            .delimited_by(just(Token::Bar), just(Token::Bar));

        let parse_func =
            parse_signature
                .clone()
                .then(bracketed)
                .map(|((binding, arg_type), body)| Func {
                    binding,
                    arg_type,
                    body: Box::new(body),
                });

        let parse_fix_body = parse_ident_lower
            .then(parse_signature)
            .then_ignore(just(Token::Colon))
            .then(parse_type())
            .then(term.clone().map(Box::new))
            .map(
                |(((fix_binding, (inner_binding, arg_type)), ret_type), body)| Fix {
                    fix_binding,
                    inner_binding,
                    arg_type,
                    ret_type,
                    body,
                },
            );

        let parse_if_else = just(Token::If)
            .ignore_then(term.clone())
            .then_ignore(just(Token::Then))
            .then(term.clone())
            .then_ignore(just(Token::Else))
            .then(term.clone())
            .map(|((cond, if_true), if_false)| IfElse {
                cond: Box::new(cond),
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            });
        let parse_let = just(Token::Let)
            .ignore_then(parse_ident_lower)
            .then_ignore(just(Token::Bind))
            .then(term.clone())
            .then_ignore(just(Token::In))
            .then(term.clone())
            .map(|((binding, expr), body)| LetBinding {
                binding,
                expr: Box::new(expr),
                body: Box::new(body),
            });
        let parse_let_box = just(Token::Let)
            .ignore_then(just(Token::Box))
            .ignore_then(parse_ident_lower)
            .then_ignore(just(Token::Bind))
            .then(term.clone())
            .then_ignore(just(Token::In))
            .then(term.clone())
            .map(|((binding, expr), body)| LetBinding {
                binding,
                expr: Box::new(expr),
                body: Box::new(body),
            });
        let parse_match = just(Token::Match)
            .ignore_then(term.clone())
            .then_ignore(just(Token::Bar))
            .then(
                parse_ident_upper
                    .then(parse_ident_lower)
                    .then_ignore(just(Token::RDArrow))
                    .then(term.clone())
                    .map(|((ctor, ident), body)| MatchArm { ctor, ident, body })
                    .separated_by(just(Token::Bar))
                    .collect::<Vec<_>>(),
            );
        let parse_ctor = parse_ident_upper
            .then(term.clone())
            .map(|(ctor, term)| Ctor {
                ctor,
                term: Box::new(term),
            });
        let parse_tuple = term
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LAngleBracket), just(Token::RAngleBracket));
        let parse_arr = parse_type()
            .then_ignore(just(Token::LBrace))
            .then(
                term.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RBrace));

        let atom = choice((
            parse_ctor.map(TermType::Ctor),
            parse_tuple.map(|terms| TermType::Tuple(terms.into())),
            parse_let_box.map(TermType::LetBoxBinding),
            parse_let.map(TermType::LetBinding),
            parse_if_else.map(TermType::IfElse),
            parse_match.map(|(term, arms)| TermType::Match(Box::new(term), arms)),
            just(Token::Fix)
                .ignore_then(parse_fix_body.clone())
                .map(TermType::Fix),
            just(Token::MFix)
                .ignore_then(parse_fix_body)
                .map(TermType::MFix),
            parse_func.map(TermType::Function),
            parse_box.map(|t| TermType::Box(Box::new(t))),
            term.clone()
                .delimited_by(just(Token::Bar), just(Token::Bar))
                .map(|term| TermType::Length(Box::new(term))),
            // just(Token::Minus)
            //     .ignore_then(term.clone())
            //     .map(|t| TermType::UnaryMinus(Box::new(t))),
            term.clone()
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .map(|inner| TermType::Bracketed(Box::new(inner))),
            parse_arr.map(|(ty, terms)| TermType::ArrLiteral(ty, terms.into_boxed_slice())),
            parse_ident_lower.map(TermType::Variable),
            parse_bool.map(TermType::BoolLiteral),
            parse_int.map(TermType::IntLiteral),
        ))
        .try_map_with(|v, e| {
            Ok(Term {
                ty: v,
                span: e.span(),
            })
        })
        .boxed();

        fn maybe_map<
            'src,
            R,
            I: chumsky::input::Input<'src, Span = SimpleSpan>,
            E: extra::ParserExtra<'src, I>,
        >(
            map: impl Clone + Fn(Term, R) -> TermType,
        ) -> impl Clone + Fn((Term, Option<R>), &mut MapExtra<'src, '_, I, E>) -> Term {
            move |(expr, rhs), e| {
                if let Some(rhs) = rhs {
                    Term {
                        ty: map(expr, rhs),
                        span: e.span(),
                    }
                } else {
                    expr
                }
            }
        }

        // Tuple Indexing
        let atom = atom
            .then(
                just(Token::Dot)
                    .ignore_then(select! { Token::Integer(ind) => ind})
                    .map_with(|v, e| (v, e.span()))
                    .or_not(),
            )
            .map_with(maybe_map(|expr, ind| {
                TermType::TupleIndex(TupleIndex {
                    tup: Box::new(expr),
                    index: ind,
                })
            }));

        // Array Indexing
        let atom = atom
            .then(
                term.clone()
                    .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
                    .or_not(),
            )
            .map_with(maybe_map(|list, index| {
                TermType::Index(Index {
                    list: Box::new(list),
                    index: Box::new(index),
                })
            }));

        // Slicing
        let atom = atom
            .then(
                (term
                    .clone()
                    .or_not()
                    .then_ignore(just(Token::Range))
                    .then(term.clone().or_not()))
                .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
                .or_not(),
            )
            .map_with(maybe_map(
                |list, (lower, upper): (Option<Term>, Option<Term>)| {
                    TermType::Slice(Slice {
                        list: Box::new(list),
                        lower: lower.map(Box::new),
                        upper: upper.map(Box::new),
                    })
                },
            ));

        // Application
        let atom = atom
            .clone()
            .foldl_with(atom.repeated(), |lhs, rhs, e| Term {
                ty: TermType::Application(Application {
                    func: Box::new(lhs),
                    arg: Box::new(rhs),
                }),
                span: e.span(),
            });

        // Binary primitives
        let atom = atom
            .then(
                select! {
                    Token::Add => BinaryOp::Add,
                    Token::Minus => BinaryOp::Subtract,
                    Token::Multiply => BinaryOp::Multiply,
                    Token::Divide => BinaryOp::Divide,
                    Token::EqualTo => BinaryOp::Equal,
                    Token::NotEqualTo => BinaryOp::NotEqual,
                    Token::LAngleBracket => BinaryOp::LessThan,
                    Token::RAngleBracket => BinaryOp::GreaterThan,
                    Token::LessThanOrEqual => BinaryOp::LessThanOrEqual,
                    Token::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
                }
                .then(term.clone())
                .or_not(),
            )
            .map_with(maybe_map(|expr, a: (BinaryOp, Term)| {
                TermType::BinaryPrimitive(BinaryPrimitive {
                    op: a.0,
                    lhs: Box::new(expr),
                    rhs: Box::new(a.1),
                })
            }));

        // Appending
        let atom = atom
            .then(just(Token::Append).ignore_then(term.clone()).or_not())
            .map_with(maybe_map(|list, item| {
                TermType::Append(Append {
                    list: Box::new(list),
                    item: Box::new(item),
                })
            }));

        let atom = atom
            .then(just(Token::As).ignore_then(parse_type()).or_not())
            .map_with(maybe_map(|expr, ty| {
                TermType::Ascription(Box::new(expr), ty)
            }));

        atom
    })
}

pub fn parse_program<'a, I>() -> impl Parser<'a, I, Program, Full<'a>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let parse_typedef = just(Token::Type)
        .ignore_then(select! {Token::IdentUpper(name) => name.to_owned() })
        .then_ignore(just(Token::Bind))
        .then(
            select! {Token::IdentUpper(ctr) => ctr.to_owned()}
                .then(parse_type())
                .separated_by(just(Token::Bar))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::In));

    parse_typedef
        .repeated()
        .collect::<Vec<_>>()
        .then(parse_term())
        .map(|(typedefs, term)| Program {
            typedefs: typedefs.into_iter().collect(),
            term,
        })
}

pub fn generate_static_ast(input: &str) -> Result<Program, Vec<StaticError>> {
    use logos::Logos;

    let token_iter = Token::lexer(input).spanned();
    let (lex_errs, ok) = token_iter.partition::<Vec<_>, _>(|(tok, _)| tok.is_err());

    if !lex_errs.is_empty() {
        return Err(lex_errs
            .into_iter()
            .map(|(err, span)| StaticError::new(span, format!("Lexer error: {}", err.unwrap_err())))
            .collect());
    }

    let tokens = ok
        .into_iter()
        .map(|(tok, span)| (tok.unwrap(), span.into()));

    let token_stream = Stream::from_iter(tokens).map((0..input.len()).into(), |x| x);

    let (output, errs) = parse_program()
        .then_ignore(end())
        .parse(token_stream)
        .into_output_errors();

    if !errs.is_empty() {
        return Err(errs
            .into_iter()
            .map(|err| StaticError::new(*err.span(), format!("{err:?}")))
            .collect());
    }

    Ok(output.unwrap())
}
