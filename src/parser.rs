use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};

use crate::{types, StaticError};

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
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'a str),
    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),
    #[token("<>")]
    Unit,
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
    #[token("+")]
    Add,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("==")]
    EqualTo,
    #[token("!=")]
    NotEqualTo,
    #[token("=")]
    Bind,
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
    #[token("true")]
    True,
    #[token("false")]
    False,
}

#[derive(Debug)]
pub enum TermType {
    NilLiteral(types::Type),
    UnitLiteral,
    BoolLiteral(bool),
    IntLiteral(i64),
    Bracketed(Box<Term>),
    Box(Box<Term>),
    Variable(Ident),
    Function(Func),
    Application(Application),
    IfElse(IfElse),
    LetBinding(LetBinding),
    LetBoxBinding(LetBinding),
    Fix(Func),
    UnaryMinus(Box<Term>),
    BinaryPrimitive(BinaryPrimitive),
    Append(Append),
    Index(Index),
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
            choice((
                ty.clone()
                    .then_ignore(just(Token::RArrow))
                    .then(ty.clone())
                    .memoized()
                    .map(|(l, r)| types::Type::Func(l.into(), r.into())),
                ty.clone()
                    .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket))
                    .map(|ty: types::Type| types::Type::Mobile(ty.into())),
                ty.clone()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map(|ty: types::Type| types::Type::List(ty.into())),
                select! {
                    Token::Unit => types::Type::Unit,
                    Token::Ident("Bool") => types::Type::Bool,
                    Token::Ident("Int") => types::Type::Int,
                },
            ))
        },
    )
}

pub fn parse_term<'a, I>() -> impl Parser<'a, I, Term, Full<'a>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    recursive(|term| {
        let parse_int = select! { Token::Integer(val) => val };
        let parse_bool = select! {
            Token::True => true,
            Token::False => false,
        };
        let parse_box = just(Token::Box).ignore_then(term.clone());
        let parse_var = (select! { Token::Ident(name) => name }).try_map_with(|v, e| {
            Ok(Ident {
                ident: String::from(v),
                span: e.span(),
            })
        });
        let parse_func = parse_var
            .then_ignore(just(Token::Colon))
            .then(parse_type())
            .delimited_by(just(Token::Bar), just(Token::Bar))
            .then(term.clone())
            .map(|((binding, arg_type), body)| Func {
                binding,
                arg_type,
                body: Box::new(body),
            });
        let parse_appl = term
            .clone()
            .then(term.clone())
            .map(|(func, arg)| Application {
                func: Box::new(func),
                arg: Box::new(arg),
            })
            .memoized();
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
            .ignore_then(parse_var)
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
            .ignore_then(parse_var)
            .then_ignore(just(Token::Bind))
            .then(term.clone())
            .then_ignore(just(Token::In))
            .then(term.clone())
            .map(|((binding, expr), body)| LetBinding {
                binding,
                expr: Box::new(expr),
                body: Box::new(body),
            });
        let parse_binary_primitive = term
            .clone()
            .then(select! {
                Token::Add => BinaryOp::Add,
                Token::Minus => BinaryOp::Subtract,
                Token::Multiply => BinaryOp::Multiply,
                Token::Divide => BinaryOp::Divide,
                Token::EqualTo => BinaryOp::Equal,
                Token::NotEqualTo => BinaryOp::NotEqual,
                Token::LessThan => BinaryOp::LessThan,
                Token::GreaterThan => BinaryOp::GreaterThan,
                Token::LessThanOrEqual => BinaryOp::LessThanOrEqual,
                Token::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
            })
            .then(term.clone())
            .map(|((lhs, op), rhs)| BinaryPrimitive {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
            .memoized();
        let parse_append = term
            .clone()
            .then_ignore(just(Token::Append))
            .then(term.clone())
            .map(|(list, item)| Append {
                list: Box::new(list),
                item: Box::new(item),
            })
            .memoized();
        let parse_index = term
            .clone()
            .then(
                term.clone()
                    .delimited_by(just(Token::LSqBracket), just(Token::RSqBracket)),
            )
            .map(|(list, index)| Index {
                list: Box::new(list),
                index: Box::new(index),
            })
            .memoized();

        choice((
            just(Token::Unit).ignored().map(|_| TermType::UnitLiteral),
            parse_let_box.map(TermType::LetBoxBinding),
            parse_let.map(TermType::LetBinding),
            parse_if_else.map(TermType::IfElse),
            parse_appl.map(TermType::Application),
            just(Token::Fix)
                .ignore_then(parse_func.clone())
                .map(TermType::Fix),
            parse_func.map(TermType::Function),
            parse_box.map(|t| TermType::Box(Box::new(t))),
            parse_append.map(TermType::Append),
            parse_index.map(TermType::Index),
            parse_binary_primitive.map(TermType::BinaryPrimitive),
            just(Token::Minus)
                .ignore_then(term.clone())
                .map(|t| TermType::UnaryMinus(Box::new(t))),
            term.clone()
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .map(|inner| TermType::Bracketed(Box::new(inner))),
            parse_type()
                .then_ignore(just(Token::LBrace))
                .then_ignore(just(Token::RBrace))
                .map(TermType::NilLiteral),
            parse_var.map(TermType::Variable),
            parse_bool.map(TermType::BoolLiteral),
            parse_int.map(TermType::IntLiteral),
        ))
        .try_map_with(|v, e| {
            Ok(Term {
                ty: v,
                span: e.span(),
            })
        })
    })
}

pub fn generate_static_ast(input: &str) -> Result<Term, Vec<StaticError>> {
    use logos::Logos;

    let token_iter = Token::lexer(input).spanned();
    let (lex_errs, ok) = token_iter.partition::<Vec<_>, _>(|(tok, _)| tok.is_err());

    if !lex_errs.is_empty() {
        return Err(lex_errs
            .into_iter()
            .map(|(err, span)| StaticError {
                span: span.clone(),
                error: format!("Lexer error: {}", err.unwrap_err()),
                ..Default::default()
            })
            .collect());
    }

    let tokens = ok
        .into_iter()
        .map(|(tok, span)| (tok.unwrap(), span.into()));

    let token_stream = Stream::from_iter(tokens).spanned(SimpleSpan::new(input.len(), input.len()));

    let (output, errs) = parse_term().parse(token_stream).into_output_errors();

    if !errs.is_empty() {
        return Err(errs
            .into_iter()
            .map(|err| StaticError {
                span: (*err.span()).into(),
                error: format!("{err:?}"),
                help: None,
                note: None,
            })
            .collect());
    }

    Ok(output.unwrap())
}
