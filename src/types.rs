use std::{collections::HashSet, rc::Rc};

use chumsky::span::SimpleSpan;

use crate::{
    StaticError, UnaryMinus,
    dynamics::{self, IndexTuple, Match},
    parser::{self, Append, BinaryOp, MatchArm, Term},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Tuple(im::Vector<Type>),
    Mobile(Rc<Type>),
    List(Rc<Type>),
    Func(Rc<Type>, Rc<Type>),
    Sum(Rc<String>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Tuple(inner) => {
                let join = inner
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "<{join}>",)
            }
            Type::Mobile(inner) => write!(f, "[{inner}]"),
            Type::List(inner) => write!(f, "{{{inner}}}"),
            Type::Func(arg, ret) => write!(f, "({arg} -> {ret})"),
            Type::Sum(name) => write!(f, "{name}"),
        }
    }
}

enum TypeError {
    MissingVariable {
        ident: String,
    },
    InvalidCtor {
        ident: String,
    },
    TypeMismatch {
        expected: Type,
        got: Type,
    },
    NotAFunction {
        got: Type,
    },
    NotABox {
        got: Type,
    },
    NotAnArray {
        got: Type,
    },
    NotATuple {
        got: Type,
    },
    NotASumType {
        got: Type,
    },
    InvalidTupleIndex {
        index: u64,
        len: usize,
    },
    DuplicatedBranch {
        ident: String,
    },
    MissingBranch {
        ident: String,
    },
    CtorDoesNotBelongToType {
        ctor_ident: String,
        ty_ident: String,
    },
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::MissingVariable { ident } => {
                write!(f, "Variable {ident} not in any context at this point.")
            }
            TypeError::InvalidCtor { ident } => {
                write!(f, "Constructor {ident} does not exist on any type.")
            }
            TypeError::TypeMismatch { expected, got } => {
                write!(f, "Type Mismatch: Expected {expected}. Got {got}.")
            }
            TypeError::NotAFunction { got } => {
                write!(f, "Type Mismatch: Expected a function. Got {got}.")
            }
            TypeError::NotABox { got } => {
                write!(f, "Type Mismatch: Expected a box. Got {got}.")
            }
            TypeError::NotAnArray { got } => {
                write!(f, "Type Mismatch: Expected an array. Got {got}.")
            }
            TypeError::NotATuple { got } => {
                write!(f, "Type Mismatch: Expected a tuple. Got {got}.")
            }
            TypeError::NotASumType { got } => {
                write!(f, "Type Mismatch: Expected a sum type. Got {got}.")
            }
            TypeError::InvalidTupleIndex { index, len } => {
                write!(
                    f,
                    "Tried to index a tuple at position {index}. Tuple length is {len}."
                )
            }
            TypeError::DuplicatedBranch { ident } => {
                write!(f, "Match statement contains duplicate arms for {ident}.")
            }
            TypeError::MissingBranch { ident } => {
                write!(f, "Match statement is missing arm for {ident}.")
            }
            TypeError::CtorDoesNotBelongToType {
                ctor_ident,
                ty_ident,
            } => write!(
                f,
                "Constructor {ctor_ident} does not exist on type {ty_ident}."
            ),
        }
    }
}

#[derive(Clone)]
struct TypeEnvironment {
    sum_types: im::HashMap<String, im::Vector<(String, Type)>>,
    local: im::HashMap<String, (Type, SimpleSpan)>,
    global: im::HashMap<String, (Type, SimpleSpan)>,
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        let mut sums = im::HashMap::<_, _>::default();
        sums.insert(String::from("Option"), im::vector![
            (String::from("Some"), Type::Int),
            (String::from("None"), Type::Tuple(im::vector![])),
        ]);
        sums.insert(String::from("List"), im::vector![
            (String::from("Nil"), Type::Tuple(im::vector![])),
            (
                String::from("Cons"),
                Type::Tuple(im::vector![Type::Int, Type::Sum("List".to_owned().into())])
            ),
        ]);
        Self {
            sum_types: sums,
            local: Default::default(),
            global: Default::default(),
        }
    }
}

impl TypeEnvironment {
    pub fn remove_local(mut self) -> Self {
        self.local = Default::default();
        self
    }
}

fn type_check_impl(
    term: Term,
    types: TypeEnvironment,
) -> Result<(Type, dynamics::Term), Vec<StaticError>> {
    match term.ty {
        parser::TermType::NilLiteral(ty) => Ok((Type::List(ty.into()), dynamics::Term::NilLiteral)),
        parser::TermType::Tuple(terms) => {
            let (tys, terms): (im::Vector<_>, im::Vector<_>) = terms
                .into_iter()
                .map(|term| type_check_impl(term, types.clone()))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .unzip();
            Ok((Type::Tuple(tys), dynamics::Term::Tuple(terms)))
        }
        parser::TermType::BoolLiteral(val) => Ok((Type::Bool, dynamics::Term::BoolLiteral(val))),
        parser::TermType::IntLiteral(val) => Ok((Type::Int, dynamics::Term::IntLiteral(val))),
        parser::TermType::Bracketed(term) => type_check_impl(*term, types),
        parser::TermType::Box(term) => {
            let types = types.clone().remove_local();
            let (ty, body) = type_check_impl(*term, types)?;
            Ok((
                Type::Mobile(ty.into()),
                dynamics::Term::Box(dynamics::BxTerm { body: body.into() }),
            ))
        }
        parser::TermType::Variable(var) => {
            if let Some((ty, _)) = types.local.get(&var.ident) {
                Ok((ty.clone(), dynamics::Term::LocalVariable(var.ident.into())))
            } else if let Some((ty, _)) = types.global.get(&var.ident) {
                Ok((ty.clone(), dynamics::Term::GlobalVariable(var.ident.into())))
            } else {
                let err = TypeError::MissingVariable { ident: var.ident };
                StaticError::new(term.span, err).into()
            }
        }
        parser::TermType::Function(func) => {
            let mut new_types = types.clone();
            new_types.local.insert(
                func.binding.ident.clone(),
                (func.arg_type.clone(), func.binding.span),
            );
            let (body_ty, body_term) = type_check_impl(*func.body, new_types)?;

            Ok((
                Type::Func(func.arg_type.into(), body_ty.into()),
                dynamics::Term::Function(dynamics::FuncTerm {
                    binding: func.binding.ident.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::Application(app) => {
            let (func_span, arg_span) = (app.func.span, app.arg.span);
            let (func_ty, func_term) = type_check_impl(*app.func, types.clone())?;
            let (arg_ty, arg_term) = type_check_impl(*app.arg, types.clone())?;

            let Type::Func(farg_ty, ret_ty) = func_ty else {
                let err = TypeError::NotAFunction { got: func_ty };
                return StaticError::new(func_span, err).into();
            };
            let farg_ty = (*farg_ty).clone();

            if arg_ty != farg_ty {
                let err = TypeError::TypeMismatch {
                    expected: farg_ty,
                    got: arg_ty,
                };
                return StaticError::new(arg_span, err).into();
            }

            Ok((
                (*ret_ty).clone(),
                dynamics::Term::Application(crate::Application {
                    func: func_term.into(),
                    arg: arg_term.into(),
                }),
            ))
        }
        parser::TermType::IfElse(if_else) => {
            let (c_span, f_span) = (if_else.cond.span, if_else.if_false.span);
            let (cond_ty, cond_term) = type_check_impl(*if_else.cond, types.clone())?;
            let (true_ty, true_term) = type_check_impl(*if_else.if_true, types.clone())?;
            let (false_ty, false_term) = type_check_impl(*if_else.if_false, types.clone())?;

            if !matches!(cond_ty, Type::Bool) {
                let err = TypeError::TypeMismatch {
                    expected: Type::Bool,
                    got: cond_ty,
                };
                return StaticError::new(c_span, err).into();
            }
            if true_ty != false_ty {
                let err = TypeError::TypeMismatch {
                    expected: true_ty,
                    got: false_ty,
                };
                return StaticError::new(f_span, err).into();
            }

            Ok((
                true_ty,
                dynamics::Term::IfElse(crate::IfElse {
                    cond: cond_term.into(),
                    if_true: true_term.into(),
                    if_false: false_term.into(),
                }),
            ))
        }
        parser::TermType::LetBinding(let_binding) => {
            if let Some((_, prev_span)) = types.global.get(&let_binding.binding.ident) {
                return Err(vec![
                    StaticError::new(
                        let_binding.binding.span,
                        String::from("Conflicting variable binding"),
                    ),
                    StaticError::new(*prev_span, String::from("Previously defined here")),
                ]);
            }

            let (ev_ty, ev_term) = type_check_impl(*let_binding.expr, types.clone())?;
            let mut new_types = types.clone();
            new_types.local.insert(
                let_binding.binding.ident.clone(),
                (ev_ty, let_binding.binding.span),
            );
            let (body_ty, body_term) = type_check_impl(*let_binding.body, new_types)?;
            Ok((
                body_ty,
                dynamics::Term::LetBinding(dynamics::LetBinding {
                    binding: let_binding.binding.ident.into(),
                    expr: ev_term.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::LetBoxBinding(let_binding) => {
            if let Some((_, prev_span)) = types.local.get(&let_binding.binding.ident) {
                return Err(vec![
                    StaticError::new(
                        let_binding.binding.span,
                        String::from("Conflicting variable binding"),
                    ),
                    StaticError::new(*prev_span, String::from("Previously defined here")),
                ]);
            }

            let ev_span = let_binding.expr.span;
            let (ev_ty, ev_term) = type_check_impl(*let_binding.expr, types.clone())?;
            let Type::Mobile(ev_ty) = ev_ty else {
                let err = TypeError::NotABox { got: ev_ty };
                return StaticError::new(ev_span, err).into();
            };

            let mut new_types = types.clone();
            new_types.global.insert(
                let_binding.binding.ident.clone(),
                ((*ev_ty).clone(), let_binding.binding.span),
            );
            let (body_ty, body_term) = type_check_impl(*let_binding.body, new_types)?;

            Ok((
                body_ty,
                dynamics::Term::LetBoxBinding(dynamics::LetBinding {
                    binding: let_binding.binding.ident.into(),
                    expr: ev_term.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::Fix(fix) => {
            let mut new_types = types.clone();
            let body_span = fix.body.span;
            new_types.local.insert(
                fix.binding.ident.clone(),
                (fix.arg_type.clone(), fix.binding.span),
            );
            let (body_ty, body_term) = type_check_impl(*fix.body, new_types)?;

            if !matches!(fix.arg_type, Type::Func(..)) {
                let msg = String::from("Fix is not supported for non-functions types");
                return StaticError::new(term.span, msg).into();
            }

            if body_ty != fix.arg_type {
                let err = TypeError::TypeMismatch {
                    expected: fix.arg_type,
                    got: body_ty,
                };
                return StaticError::new(body_span, err).into();
            }

            Ok((
                fix.arg_type,
                dynamics::Term::Fix(crate::FuncTerm {
                    binding: fix.binding.ident.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::BinaryPrimitive(bin) => {
            let (lhs_span, rhs_span) = (bin.lhs.span, bin.rhs.span);
            let (lhs_ty, lhs_term) = type_check_impl(*bin.lhs, types.clone())?;
            let (rhs_ty, rhs_term) = type_check_impl(*bin.rhs, types.clone())?;

            let mut errs = vec![];
            if !matches!(lhs_ty, Type::Int) {
                let err = TypeError::TypeMismatch {
                    expected: Type::Int,
                    got: lhs_ty.clone(),
                };
                errs.push(StaticError::new(lhs_span, err));
            }
            if !matches!(lhs_ty, Type::Int)
                && matches!(rhs_ty, Type::Int)
                && matches!(bin.op, BinaryOp::Subtract)
            {
                let msg = String::from(
                    "If you intended to perform application with a negated argument, wrap this argument in brackets",
                );
                errs.push(StaticError::new(lhs_span.end..rhs_span.end, msg));
            }
            if rhs_ty != Type::Int {
                let err = TypeError::TypeMismatch {
                    expected: Type::Int,
                    got: rhs_ty.clone(),
                };
                errs.push(StaticError::new(rhs_span, err));
            }

            if !errs.is_empty() {
                return Err(errs);
            }

            let int_op = |op: dynamics::BinaryOp| -> Result<_, Vec<StaticError>> {
                Ok((
                    Type::Int,
                    dynamics::Term::BinaryPrimitive(dynamics::BinaryPrimitive {
                        op,
                        lhs: lhs_term.clone().into(),
                        rhs: rhs_term.clone().into(),
                    }),
                ))
            };

            let bool_op = |op: dynamics::BinaryOp| -> Result<_, Vec<StaticError>> {
                Ok((
                    Type::Bool,
                    dynamics::Term::BinaryPrimitive(dynamics::BinaryPrimitive {
                        op,
                        lhs: lhs_term.clone().into(),
                        rhs: rhs_term.clone().into(),
                    }),
                ))
            };

            use parser::BinaryOp::*;
            match bin.op {
                Add => int_op(dynamics::BinaryOp::Add),
                Subtract => int_op(dynamics::BinaryOp::Subtract),
                parser::BinaryOp::Multiply => int_op(dynamics::BinaryOp::Multiply),
                parser::BinaryOp::Divide => int_op(dynamics::BinaryOp::Divide),
                parser::BinaryOp::Equal => bool_op(dynamics::BinaryOp::Equal),
                parser::BinaryOp::NotEqual => bool_op(dynamics::BinaryOp::NotEqual),
                parser::BinaryOp::LessThan => bool_op(dynamics::BinaryOp::LessThan),
                parser::BinaryOp::GreaterThan => bool_op(dynamics::BinaryOp::GreaterThan),
                parser::BinaryOp::LessThanOrEqual => bool_op(dynamics::BinaryOp::LessThanOrEqual),
                parser::BinaryOp::GreaterThanOrEqual => {
                    bool_op(dynamics::BinaryOp::GreaterThanOrEqual)
                }
            }
        }
        parser::TermType::UnaryMinus(term) => {
            let inner_span = term.span;
            let (inner_ty, inner_term) = type_check_impl(*term, types)?;
            if !matches!(inner_ty, Type::Int) {
                let err = TypeError::TypeMismatch {
                    expected: Type::Int,
                    got: inner_ty,
                };
                return StaticError::new(inner_span, err).into();
            }
            Ok((
                Type::Int,
                dynamics::Term::UnaryMinus(UnaryMinus(inner_term.into())),
            ))
        }
        parser::TermType::Append(Append { list, item }) => {
            let (list_span, item_span) = (list.span, item.span);
            let (list_ty, list_term) = type_check_impl(*list, types.clone())?;
            let (item_ty, item_term) = type_check_impl(*item, types.clone())?;

            let Type::List(list_ty_inner) = list_ty.clone() else {
                let err = TypeError::NotAnArray { got: list_ty };
                return StaticError::new(list_span, err).into();
            };

            if *list_ty_inner != item_ty {
                let err = TypeError::TypeMismatch {
                    expected: (*list_ty_inner).clone(),
                    got: item_ty,
                };
                return StaticError::new(item_span, err).into();
            }

            Ok((
                list_ty,
                dynamics::Term::Append(crate::Append {
                    list: list_term.into(),
                    item: item_term.into(),
                }),
            ))
        }
        parser::TermType::Index(parser::Index { list, index }) => {
            let (list_span, index_span) = (list.span, index.span);
            let (list_ty, list_term) = type_check_impl(*list, types.clone())?;
            let (index_ty, index_term) = type_check_impl(*index, types.clone())?;

            let Type::List(list_ty_inner) = list_ty.clone() else {
                let err = TypeError::NotAnArray { got: list_ty };
                return StaticError::new(list_span, err).into();
            };

            if !matches!(index_ty, Type::Int) {
                let err = TypeError::TypeMismatch {
                    expected: Type::Int,
                    got: index_ty,
                };
                return StaticError::new(index_span, err).into();
            }

            Ok((
                (*list_ty_inner).clone(),
                dynamics::Term::Index(crate::Index {
                    list: list_term.into(),
                    index: index_term.into(),
                }),
            ))
        }
        parser::TermType::TupleIndex(tuple_index) => {
            let span = tuple_index.tup.span;
            let (ty, term) = type_check_impl(*tuple_index.tup, types)?;
            let ind: Result<u64, _> = (&tuple_index.index.0).try_into();
            let Ok(ind) = ind else {
                return StaticError::new(tuple_index.index.1, String::from("Index too large"))
                    .into();
            };
            let Type::Tuple(typs) = ty else {
                let err = TypeError::NotATuple { got: ty };
                return StaticError::new(span, err).into();
            };
            if ind as usize >= typs.len() {
                let err = TypeError::InvalidTupleIndex {
                    index: ind,
                    len: typs.len(),
                };
                return StaticError::new(tuple_index.index.1, err).into();
            }
            Ok((
                typs[ind as usize].clone(),
                dynamics::Term::IndexTuple(IndexTuple {
                    tuple: term.into(),
                    index: ind,
                }),
            ))
        }
        parser::TermType::Ascription(term, asc) => {
            let span = term.span;
            let (ty, inner) = type_check_impl(*term, types)?;
            if ty != asc {
                let err = TypeError::TypeMismatch {
                    expected: asc,
                    got: ty,
                };
                return StaticError::new(span, err).into();
            }
            Ok((ty, inner))
        }
        parser::TermType::Ctor(ctor) => {
            let Some((ty_name, inner_variant_type)) =
                types.sum_types.iter().find_map(|(name, variants)| {
                    variants.iter().find_map(|(find_ctor, ty)| {
                        (*find_ctor == ctor.ctor.ident).then_some((name, ty))
                    })
                })
            else {
                let err = TypeError::InvalidCtor {
                    ident: ctor.ctor.ident,
                };
                return StaticError::new(ctor.ctor.span, err).into();
            };
            let inner_variant_type = (*inner_variant_type).clone();

            let span = ctor.term.span;
            let (ty, term) = type_check_impl(*ctor.term, types.clone())?;

            if ty != inner_variant_type {
                let err = TypeError::TypeMismatch {
                    expected: inner_variant_type,
                    got: ty,
                };
                return StaticError::new(span, err).into();
            }

            Ok((
                Type::Sum(Rc::new(ty_name.clone())),
                dynamics::Term::Tag(dynamics::Tag {
                    tag: ctor.ctor.ident.into(),
                    body: term.into(),
                }),
            ))
        }
        parser::TermType::Match(expr, match_arms) => {
            let expr_span = expr.span;
            let (expr_ty, expr_term) = type_check_impl(*expr, types.clone())?;

            let Type::Sum(sum_ty_name) = expr_ty else {
                let err = TypeError::NotASumType { got: expr_ty };
                return StaticError::new(expr_span, err).into();
            };

            let sum_ty_variants = types.sum_types.get(&*sum_ty_name).unwrap();

            {
                let mut hs: HashSet<&str> = HashSet::new();
                for MatchArm { ctor, .. } in &match_arms {
                    if !hs.insert(&ctor.ident) {
                        let err = TypeError::DuplicatedBranch {
                            ident: ctor.ident.clone(),
                        };
                        let help = format!("Remove branch {}", ctor.ident);
                        return StaticError::new(ctor.span, err).with_help(help).into();
                    }
                }

                let needed: HashSet<&str> =
                    sum_ty_variants.iter().map(|(name, _)| &**name).collect();

                let missing = needed.difference(&hs);

                let errs: Vec<_> = missing
                    .into_iter()
                    .map(|arm| {
                        let err = TypeError::MissingBranch {
                            ident: arm.to_string(),
                        };
                        StaticError::new(expr_span, err)
                    })
                    .collect();

                if !errs.is_empty() {
                    return Err(errs);
                }
            }

            let mut match_arms_iter = match_arms.into_iter();

            let (fst_ty, fst_arm) = {
                let arm = match_arms_iter.next().unwrap();

                let Some(ty) = sum_ty_variants
                    .iter()
                    .find_map(|(nm, ty)| (*nm == arm.ctor.ident).then_some(ty))
                else {
                    let err = TypeError::CtorDoesNotBelongToType {
                        ctor_ident: arm.ctor.ident,
                        ty_ident: sum_ty_name.to_string(),
                    };
                    return StaticError::new(arm.ctor.span, err).into();
                };

                let mut types = types.clone();
                types
                    .local
                    .insert(arm.ident.ident.clone(), (ty.clone(), arm.ident.span));
                type_check_impl(arm.body, types).map(|(ty, term)| {
                    (
                        ty,
                        (arm.ctor.ident, dynamics::Arm {
                            bind: arm.ident.ident.into(),
                            body: term,
                        }),
                    )
                })?
            };

            let match_terms: Vec<_> = [Ok(fst_arm)]
                .into_iter()
                .chain(match_arms_iter.map(|arm| {
                    let Some(ty) = sum_ty_variants
                        .iter()
                        .find_map(|(nm, ty)| (*nm == arm.ctor.ident).then_some(ty))
                    else {
                        let err = TypeError::CtorDoesNotBelongToType {
                            ctor_ident: arm.ctor.ident,
                            ty_ident: sum_ty_name.to_string(),
                        };
                        return Err(vec![StaticError::new(arm.ctor.span, err)]);
                    };

                    let body_span = arm.body.span;
                    let mut types = types.clone();
                    types
                        .local
                        .insert(arm.ident.ident.clone(), (ty.clone(), arm.ident.span));
                    let (ty, term) = type_check_impl(arm.body, types)?;

                    if ty != fst_ty {
                        let err = TypeError::TypeMismatch {
                            expected: fst_ty.clone(),
                            got: ty,
                        };
                        return StaticError::new(body_span, err).into();
                    }

                    Ok((arm.ctor.ident, dynamics::Arm {
                        bind: arm.ident.ident.into(),
                        body: term,
                    }))
                }))
                .collect::<Result<Vec<_>, _>>()?;

            Ok((
                fst_ty,
                dynamics::Term::Match(Match {
                    expr: expr_term.into(),
                    arms: match_terms.into(),
                }),
            ))
        }
        parser::TermType::Slice(parser::Slice { list, lower, upper }) => {
            let list_span = list.span;
            let (list_ty, list_term) = type_check_impl(*list, types.clone())?;

            let Type::List(_) = list_ty.clone() else {
                return Err(vec![StaticError {
                    span: list_span.into(),
                    error: format!("Left side of index was not a list type. Got {list_ty}"),
                    help: None,
                    note: None,
                }]);
            };

            let lower = if let Some(term) = lower {
                let sp = term.span;
                let (ty, lower_term) = type_check_impl(*term, types.clone())?;
                if !matches!(ty, Type::Int) {
                    return Err(vec![StaticError {
                        span: sp.into(),
                        error: format!("Lower bound of Int. Got {ty}"),
                        help: None,
                        note: None,
                    }]);
                }
                Some(lower_term.into())
            } else {
                None
            };

            let upper = if let Some(term) = upper {
                let sp = term.span;
                let (ty, upper_term) = type_check_impl(*term, types.clone())?;
                if !matches!(ty, Type::Int) {
                    return Err(vec![StaticError {
                        span: sp.into(),
                        error: format!("Right side of index was not an Int. Got {ty}"),
                        help: None,
                        note: None,
                    }]);
                }
                Some(upper_term.into())
            } else {
                None
            };

            Ok((
                list_ty,
                dynamics::Term::Slice(crate::Slice {
                    list: list_term.into(),
                    lower,
                    upper,
                }),
            ))
        }
        parser::TermType::Length(list) => {
            let list_span = list.span;
            let (list_ty, list_term) = type_check_impl(*list, types.clone())?;

            let Type::List(_) = list_ty.clone() else {
                return Err(vec![StaticError {
                    span: list_span.into(),
                    error: format!("Left side of index was not a list type. Got {list_ty}"),
                    help: None,
                    note: None,
                }]);
            };

            Ok((Type::Int, dynamics::Term::Length(list_term.into())))
        }
    }
}

pub fn type_check(term: Term) -> Result<dynamics::Term, Vec<StaticError>> {
    type_check_impl(term, TypeEnvironment::default()).map(|(_, term)| term)
}
