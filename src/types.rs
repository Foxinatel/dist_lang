use std::rc::Rc;

use crate::{
    dynamics,
    parser::{self, Term},
    StaticError, UnaryMinus,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Mobile(Rc<Type>),
    Func(Rc<Type>, Rc<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "<>"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Mobile(inner) => write!(f, "[]{inner}"),
            Type::Func(arg, ret) => write!(f, "{arg} -> {ret}"),
        }
    }
}

#[derive(Clone, Default)]
struct TypeEnvironment {
    local: im::HashMap<String, Type>,
    global: im::HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn remove_local(mut self) -> Self {
        self.local = Default::default();
        self
    }
}

pub fn type_check_impl(
    term: Term,
    types: TypeEnvironment,
) -> Result<(Type, dynamics::Term), Vec<StaticError>> {
    match term.ty {
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
            if let Some(ty) = types.local.get(&var) {
                Ok((ty.clone(), dynamics::Term::LocalVariable(var)))
            } else if let Some(ty) = types.global.get(&var) {
                Ok((ty.clone(), dynamics::Term::GlobalVariable(var)))
            } else {
                Err(vec![StaticError {
                    span: term.span.into(),
                    error: format!("Could not find variable {var} in context"),
                    help: None,
                    note: None,
                }])
            }
        }
        parser::TermType::Function(func) => {
            let mut new_types = types.clone();
            new_types
                .local
                .insert(func.binding.clone(), func.arg_type.clone());
            let (body_ty, body_term) = type_check_impl(*func.body, new_types)?;

            Ok((
                Type::Func(func.arg_type.into(), body_ty.into()),
                dynamics::Term::Function(dynamics::Func {
                    binding: func.binding,
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::Application(app) => {
            let func_span = app.func.span;
            let (func_ty, func_term) = type_check_impl(*app.func, types.clone())?;
            let (arg_ty, arg_term) = type_check_impl(*app.arg, types.clone())?;

            let Type::Func(farg_ty, ret_ty) = func_ty else {
                return Err(vec![StaticError {
                    span: func_span.into(),
                    error: format!(
                        "Left side of application not evaluate to a function type. Got {func_ty}"
                    ),
                    help: None,
                    note: None,
                }]);
            };

            if arg_ty != *farg_ty {
                return Err(vec![StaticError {
                    span: func_span.into(),
                    error: format!("Right side of application did not match function type. Expected {farg_ty}. Got {arg_ty}"),
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
            let (true_ty, true_term) = type_check_impl(*if_else.if_true, types.clone())?;
            let (false_ty, false_term) = type_check_impl(*if_else.if_false, types.clone())?;

            let mut errs = vec![];
            if !matches!(cond_ty, Type::Bool) {
                errs.push(StaticError {
                    span: c_span.into(),
                    error: format!("Condition did not evaluate to a Bool. Got {cond_ty}"),
                    help: None,
                    note: None,
                });
            }
            if true_ty != false_ty {
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
            let (ev_ty, ev_term) = type_check_impl(*let_binding.expr, types.clone())?;
            let mut new_types = types.clone();
            new_types.local.insert(let_binding.binding.clone(), ev_ty);
            let (body_ty, body_term) = type_check_impl(*let_binding.body, new_types)?;
            Ok((
                body_ty,
                dynamics::Term::LetBinding(dynamics::LetBinding {
                    binding: let_binding.binding,
                    expr: ev_term.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::LetBoxBinding(let_binding) => {
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
            new_types
                .global
                .insert(let_binding.binding.clone(), (*ev_ty).clone());
            let (body_ty, body_term) = type_check_impl(*let_binding.body, new_types)?;

            Ok((
                body_ty,
                dynamics::Term::LetBoxBinding(dynamics::LetBinding {
                    binding: let_binding.binding,
                    expr: ev_term.into(),
                    body: body_term.into(),
                }),
            ))
        }
        parser::TermType::Fix(fix) => todo!(),
        parser::TermType::BinaryPrimitive(bin) => {
            let (lhs_span, rhs_span) = (bin.lhs.span, bin.rhs.span);
            let (lhs_ty, lhs_term) = type_check_impl(*bin.lhs, types.clone())?;
            let (rhs_ty, rhs_term) = type_check_impl(*bin.rhs, types.clone())?;

            let mut errs = vec![];
            if !matches!(lhs_ty, Type::Int) {
                errs.push(StaticError {
                    span: lhs_span.into(),
                    error: format!("Left-hand side of binary expression does not evaluate to Int. Got {lhs_ty}"),
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
    }
}

pub fn type_check(term: Term) -> Result<dynamics::Term, Vec<StaticError>> {
    type_check_impl(term, TypeEnvironment::default()).map(|(_, term)| term)
}
