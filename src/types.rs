use chumsky::span::SimpleSpan;

use crate::{
    StaticError, UnaryMinus,
    dynamics::{self, IndexTuple},
    parser::{self, Append, BinaryOp, Term},
};

#[derive(Clone, Debug)]
pub enum Type {
    Bool,
    Int,
    Tuple(im::Vector<Type>),
    Mobile(Box<Type>),
    List(Box<Type>),
    Func(Box<Type>, Box<Type>),
    Undetermined,
}

impl Type {
    pub fn unify(&mut self, other: &mut Self) -> bool {
        match (self, other) {
            (Type::Undetermined, Type::Undetermined) => {
                todo!()
            }
            (this @ Type::Undetermined, other) => {
                *this = other.clone();
                true
            }
            (this, other @ Type::Undetermined) => {
                *other = this.clone();
                true
            }
            (Type::Bool, Type::Bool) => true,
            (Type::Int, Type::Int) => true,
            (Type::Tuple(v1), Type::Tuple(v2)) => {
                (v1.len() == v2.len())
                    && v1
                        .iter_mut()
                        .zip(v2.iter_mut())
                        .all(|(t1, t2)| t1.unify(t2))
            }
            (Type::Mobile(t1), Type::Mobile(t2)) => t1.unify(t2),
            (Type::List(t1), Type::List(t2)) => t1.unify(t2),
            (Type::Func(a1, b1), Type::Func(a2, b2)) => a1.unify(a2) && b1.unify(b2),
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Tuple(inner) => write!(
                f,
                "<{}>",
                inner
                    .iter()
                    .map(|ty| format!("{ty}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Mobile(inner) => write!(f, "[{inner}]"),
            Type::List(inner) => write!(f, "{{{inner}}}"),
            Type::Func(arg, ret) => write!(f, "({arg} -> {ret})"),
            Type::Undetermined => write!(f, "???"),
        }
    }
}

#[derive(Clone, Default)]
struct TypeEnvironment {
    local: im::HashMap<String, (Type, SimpleSpan)>,
    global: im::HashMap<String, (Type, SimpleSpan)>,
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
        parser::TermType::NilLiteral => Ok((
            Type::List(Type::Undetermined.into()),
            dynamics::Term::NilLiteral,
        )),
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
                dynamics::Term::Box(dynamics::Bx { body: body.into() }),
            ))
        }
        parser::TermType::Variable(var) => {
            if let Some((ty, _)) = types.local.get(&var.ident) {
                Ok((ty.clone(), dynamics::Term::LocalVariable(var.ident)))
            } else if let Some((ty, _)) = types.global.get(&var.ident) {
                Ok((ty.clone(), dynamics::Term::GlobalVariable(var.ident)))
            } else {
                Err(vec![StaticError {
                    span: term.span.into(),
                    error: format!("Could not find variable {} in context", var.ident),
                    help: None,
                    note: None,
                }])
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
                dynamics::Term::Function(dynamics::Func {
                    binding: func.binding.ident,
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::Application(app) => {
            let (func_span, arg_span) = (app.func.span, app.arg.span);
            let (func_ty, func_term) = type_check_impl(*app.func, types.clone())?;
            let (mut arg_ty, arg_term) = type_check_impl(*app.arg, types.clone())?;

            let Type::Func(mut farg_ty, ret_ty) = func_ty else {
                return Err(vec![StaticError {
                    span: func_span.into(),
                    error: format!(
                        "Left side of application not evaluate to a function type. Got {func_ty}"
                    ),
                    help: None,
                    note: None,
                }]);
            };

            if !arg_ty.unify(&mut farg_ty) {
                return Err(vec![StaticError {
                    span: arg_span.into(),
                    error: format!(
                        "Right side of application did not match argument type. Expected {farg_ty}. Got {arg_ty}"
                    ),
                    help: None,
                    note: None,
                }]);
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
            let c_span = if_else.cond.span;
            let (t_span, f_span) = (if_else.if_true.span, if_else.if_false.span);
            let (cond_ty, cond_term) = type_check_impl(*if_else.cond, types.clone())?;
            let (mut true_ty, true_term) = type_check_impl(*if_else.if_true, types.clone())?;
            let (mut false_ty, false_term) = type_check_impl(*if_else.if_false, types.clone())?;

            let mut errs = vec![];
            if !matches!(cond_ty, Type::Bool) {
                errs.push(StaticError {
                    span: c_span.into(),
                    error: format!("Condition did not evaluate to a Bool. Got {cond_ty}"),
                    help: None,
                    note: None,
                });
            }
            if !true_ty.unify(&mut false_ty) {
                errs.push(StaticError {
                    span: t_span.into(),
                    error: format!("True case: {true_ty}"),
                    help: None,
                    note: None,
                });
                errs.push(StaticError {
                    span: f_span.into(),
                    error: format!("Branches do not match. False case is {false_ty}"),
                    help: None,
                    note: None,
                });
            }
            if !errs.is_empty() {
                return Err(errs);
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
                    StaticError {
                        span: let_binding.binding.span.into(),
                        error: String::from("Conflicting variable binding"),
                        help: None,
                        note: None,
                    },
                    StaticError {
                        span: (*prev_span).into(),
                        error: String::from("Previously defined here"),
                        help: None,
                        note: None,
                    },
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
                    binding: let_binding.binding.ident,
                    expr: ev_term.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::LetBoxBinding(let_binding) => {
            if let Some((_, prev_span)) = types.local.get(&let_binding.binding.ident) {
                return Err(vec![
                    StaticError {
                        span: let_binding.binding.span.into(),
                        error: String::from("Conflicting variable binding"),
                        help: None,
                        note: None,
                    },
                    StaticError {
                        span: (*prev_span).into(),
                        error: String::from("Previously defined here"),
                        help: None,
                        note: None,
                    },
                ]);
            }

            let ev_span = let_binding.expr.span;
            let (ev_ty, ev_term) = type_check_impl(*let_binding.expr, types.clone())?;
            let Type::Mobile(ev_ty) = ev_ty else {
                return Err(vec![StaticError {
                    span: ev_span.into(),
                    error: format!("LetBox binding did not evaluate to a box type. Got {ev_ty}"),
                    help: None,
                    note: None,
                }]);
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
                    binding: let_binding.binding.ident,
                    expr: ev_term.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::Fix(mut fix) => {
            let mut new_types = types.clone();
            let body_span = fix.body.span;
            new_types.local.insert(
                fix.binding.ident.clone(),
                (fix.arg_type.clone(), fix.binding.span),
            );
            let (mut body_ty, body_term) = type_check_impl(*fix.body, new_types)?;

            if !matches!(fix.arg_type, Type::Func(..)) {
                return Err(vec![StaticError {
                    span: term.span.into(),
                    error: "Fix is not supported for non-functions types".to_string(),
                    help: None,
                    note: None,
                }]);
            }

            if !body_ty.unify(&mut fix.arg_type) {
                return Err(vec![StaticError {
                    span: body_span.into(),
                    error: format!(
                        "Fix body does not match type signature. Expected {}. Got {}",
                        fix.arg_type, body_ty
                    ),
                    help: None,
                    note: None,
                }]);
            }

            Ok((
                fix.arg_type,
                dynamics::Term::Fix(crate::Fix {
                    binding: fix.binding.ident,
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
                errs.push(StaticError {
                    span: lhs_span.into(),
                    error: format!(
                        "Left-hand side of binary expression does not evaluate to Int. Got {lhs_ty}"
                    ),
                    help: None,
                    note: None,
                });
            }
            if !matches!(lhs_ty, Type::Int)
                && matches!(rhs_ty, Type::Int)
                && matches!(bin.op, BinaryOp::Subtract)
            {
                errs.push(StaticError {
                    span: (lhs_span.end..rhs_span.end),
                    error: String::from(
                        "If you intended to perform application with a negated argument, wrap this argument in brackets",
                    ),
                    help: None,
                    note: None,
                });
            }
            if !matches!(rhs_ty, Type::Int) {
                errs.push(StaticError {
                    span: rhs_span.into(),
                    error: format!("Right-hand side of binary expression does not evaluate to Int. Got {rhs_ty}"),
                    help: None,
                    note: None,
                });
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
                return Err(vec![StaticError {
                    span: inner_span.into(),
                    error: format!("Unary minus can only be applied to Int. Got {inner_ty}"),
                    help: None,
                    note: None,
                }]);
            }
            Ok((
                Type::Int,
                dynamics::Term::UnaryMinus(UnaryMinus(inner_term.into())),
            ))
        }
        parser::TermType::Append(Append { list, item }) => {
            let (list_span, item_span) = (list.span, item.span);
            let (mut list_ty, list_term) = type_check_impl(*list, types.clone())?;
            let (mut item_ty, item_term) = type_check_impl(*item, types.clone())?;

            let Type::List(list_ty_inner) = &mut list_ty else {
                return Err(vec![StaticError {
                    span: list_span.into(),
                    error: format!("Left side of append was not a list type. Got {list_ty}"),
                    help: None,
                    note: None,
                }]);
            };

            if !list_ty_inner.unify(&mut item_ty) {
                return Err(vec![StaticError {
                    span: item_span.into(),
                    error: format!(
                        "Right side of append does not match left side. Expected {list_ty_inner}. Got {item_ty}"
                    ),
                    help: None,
                    note: None,
                }]);
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
                return Err(vec![StaticError {
                    span: list_span.into(),
                    error: format!("Left side of index was not a list type. Got {list_ty}"),
                    help: None,
                    note: None,
                }]);
            };

            if !matches!(index_ty, Type::Int) {
                return Err(vec![StaticError {
                    span: index_span.into(),
                    error: format!("Right side of index was not an Int. Got {index_ty}"),
                    help: None,
                    note: None,
                }]);
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
                return Err(vec![StaticError {
                    span: tuple_index.index.1.into(),
                    error: String::from("Index too large"),
                    help: None,
                    note: None,
                }]);
            };
            let Type::Tuple(typs) = ty else {
                return Err(vec![StaticError {
                    span: span.into(),
                    error: format!("Expected tuple on left side of expression. Got {ty}"),
                    help: None,
                    note: None,
                }]);
            };
            if ind as usize >= typs.len() {
                return Err(vec![StaticError {
                    span: tuple_index.index.1.into(),
                    error: format!("Invalid index {ind}, length of tuple was {}", typs.len()),
                    help: None,
                    note: None,
                }]);
            }
            Ok((
                typs[ind as usize].clone(),
                dynamics::Term::IndexTuple(IndexTuple {
                    tuple: term.into(),
                    index: ind,
                }),
            ))
        }
        parser::TermType::Ascription(term, mut asc) => {
            let span = term.span;
            let (mut ty, inner) = type_check_impl(*term, types)?;
            if !ty.unify(&mut asc) {
                return Err(vec![StaticError {
                    span: span.into(),
                    error: format!("Term does not match ascription. Expected {asc} Got {ty}"),
                    help: None,
                    note: None,
                }]);
            }
            Ok((ty, inner))
        }
    }
}

pub fn type_check(term: Term) -> Result<dynamics::Term, Vec<StaticError>> {
    let (_, term) = type_check_impl(term, TypeEnvironment::default())?;
    Ok(term)
}
