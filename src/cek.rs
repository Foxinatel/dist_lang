use std::sync::Arc;

mod mobile;

use mobile::MobileValueBuilder as _;

use crate::dynamics::*;

type MobileValueBuilder = mobile::RayonMobileValueBuilder;

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Tuple(vals) => write!(
                f,
                "<{}>",
                vals.0
                    .iter()
                    .map(|val| format!("{val}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Int(val) => write!(f, "{val:?}"),
            Value::Bool(val) => write!(f, "{val:?}"),
            Value::Func(func) => write!(f, "{} -> (...)", func.binding),
            Value::Box(_) => write!(f, "box (...)"),
            Value::Fix(fix) => write!(f, "fix {} -> (...)", fix.binding),
            Value::List(vec) => write!(
                f,
                "{{{}}}",
                vec.0
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::SumType(sum) => write!(f, "{} {:?}", sum.ctor, sum.inner),
        }
    }
}

#[derive(Debug)]
enum Ctrl {
    Value(Value),
    Term(Term),
}

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{local: [{}], global: [{}]}}",
            self.local
                .iter()
                .map(|v| format!("({}: {})", v.0, v.1))
                .collect::<Vec<_>>()
                .join(", "),
            self.global
                .iter()
                .map(|v| format!("({}: {})", v.0, v.1))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug)]
enum Kont {
    Tuple {
        env: Env,
        done: im::Vector<Value>,
        todo: im::Vector<Term>,
    },
    Bind(Ident, Term, Env),
    BindBox(Ident, Term, Env),
    Arg(Term, Env),
    Func(Func),
    IfElse {
        if_true: Term,
        if_false: Term,
        env: Env,
    },
    UnaryMinus,
    BinaryPrimitive(BinaryOp, Term, Env),
    BinaryPrimitiveVal(BinaryOp, Value),
    Append(Term, Env),
    Push(List),
    ResolvList(Term, Env),
    Index(List),
    IndexTuple(u64),
    Match(im::HashMap<String, Arm>, Env),
    Tag(Arc<String>),
}

#[derive(Debug)]
pub struct Cek {
    ctrl: Ctrl,
    pub env: Env,
    cont: Vec<Kont>,
}

impl Cek {
    pub fn new(term: Term) -> Self {
        Self {
            ctrl: Ctrl::Term(term),
            env: Env::default(),
            cont: Vec::new(),
        }
    }

    fn from_box(Bx { body, env }: Bx) -> Self {
        Self {
            ctrl: Ctrl::Term((*body).clone()),
            env: Env {
                global: env,
                local: Default::default(),
            },
            cont: Vec::new(),
        }
    }

    pub fn step(self) -> Self {
        let mut cont = self.cont;
        match self.ctrl {
            // Dynamic: Fix-Step
            Ctrl::Value(Value::Fix(fix)) => {
                let mut nu_env = self.env.clone();
                let nu_fix = Value::Fix(fix.clone());
                nu_env.local.insert(fix.binding, nu_fix);
                Self {
                    ctrl: Ctrl::Term((*fix.body).clone()),
                    env: nu_env,
                    cont,
                }
            }
            Ctrl::Value(val) => {
                match cont.pop().unwrap() {
                    // Dynamic: Bind
                    Kont::Bind(ident, term, mut old_env) => {
                        old_env.local.insert(ident, val);
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: old_env,
                            cont,
                        }
                    }
                    // Dynamic: BindBox
                    Kont::BindBox(ident, term, mut old_env) => {
                        let bx = val.bx();

                        let mv = MobileValueBuilder::compute(Self::from_box(bx));

                        // Continue on our current thread
                        old_env.global.insert(ident, mv);
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: old_env,
                            cont,
                        }
                    }
                    // Dynamic: App-2
                    Kont::Arg(arg, orig_env) => {
                        cont.push(Kont::Func(val.func()));
                        Self {
                            ctrl: Ctrl::Term(arg),
                            env: orig_env,
                            cont,
                        }
                    }
                    // Dynamic: App
                    Kont::Func(Func {
                        binding,
                        body,
                        mut env,
                    }) => {
                        env.local.insert(binding, val);
                        Self {
                            ctrl: Ctrl::Term((*body).clone()),
                            env,
                            cont,
                        }
                    }
                    // Dynamic: IfTrue / IfFalse
                    Kont::IfElse {
                        if_true,
                        if_false,
                        env,
                    } => Self {
                        ctrl: Ctrl::Term(if val.bool() { if_true } else { if_false }),
                        env,
                        cont,
                    },
                    // Dynamic: BinOp-2
                    Kont::BinaryPrimitive(prim, term, env) => {
                        cont.push(Kont::BinaryPrimitiveVal(prim, val));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
                            cont,
                        }
                    }
                    // Dynamic: BinOp
                    Kont::BinaryPrimitiveVal(prim, lhs) => {
                        let (lhs, rhs) = (lhs.int(), val.int());
                        Self {
                            ctrl: Ctrl::Value(match prim {
                                BinaryOp::Add => Value::Int(lhs + rhs),
                                BinaryOp::Subtract => Value::Int(lhs - rhs),
                                BinaryOp::Multiply => Value::Int(lhs * rhs),
                                BinaryOp::Divide => Value::Int(lhs / rhs),
                                BinaryOp::Equal => Value::Bool(lhs == rhs),
                                BinaryOp::NotEqual => Value::Bool(lhs != rhs),
                                BinaryOp::LessThan => Value::Bool(lhs < rhs),
                                BinaryOp::GreaterThan => Value::Bool(lhs > rhs),
                                BinaryOp::LessThanOrEqual => Value::Bool(lhs <= rhs),
                                BinaryOp::GreaterThanOrEqual => Value::Bool(lhs >= rhs),
                            }),
                            env: Default::default(),
                            cont,
                        }
                    }
                    // Dynamic: Negate
                    Kont::UnaryMinus => {
                        let v = val.int();
                        Self {
                            ctrl: Ctrl::Value(Value::Int(-v)),
                            env: Default::default(),
                            cont,
                        }
                    }
                    // Dynamic: Append-2
                    Kont::Append(term, env) => {
                        cont.push(Kont::Push(val.list()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: env.clone(),
                            cont,
                        }
                    }
                    // Dynamic: Append
                    Kont::Push(mut list) => {
                        list.0.push_back(val);
                        Self {
                            ctrl: Ctrl::Value(Value::List(list)),
                            env: Default::default(),
                            cont,
                        }
                    }
                    // Dynamic: Index-2
                    Kont::ResolvList(term, env) => {
                        cont.push(Kont::Index(val.list()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: env.clone(),
                            cont,
                        }
                    }
                    // Dynamic: Index
                    Kont::Index(list) => {
                        let ind: usize = (&val.int()).try_into().unwrap();
                        let item = list.0[ind].clone();
                        Self {
                            ctrl: Ctrl::Value(item),
                            env: Default::default(),
                            cont,
                        }
                    }
                    Kont::Tuple {
                        env,
                        mut done,
                        mut todo,
                    } => {
                        done.push_back(val);
                        if let Some(nxt) = todo.pop_front() {
                            // Dynamic: Tup-2
                            cont.push(Kont::Tuple {
                                env: env.clone(),
                                done,
                                todo,
                            });
                            Self {
                                ctrl: Ctrl::Term(nxt),
                                env,
                                cont,
                            }
                        } else {
                            // Dynamic: Tup
                            Self {
                                ctrl: Ctrl::Value(Value::Tuple(Tuple(done))),
                                env: Default::default(),
                                cont,
                            }
                        }
                    }
                    // Dynamic: TupIndex
                    Kont::IndexTuple(ind) => {
                        let Tuple(vals) = val.tuple();
                        let val = vals[ind as usize].clone();
                        Self {
                            ctrl: Ctrl::Value(val),
                            env: Default::default(),
                            cont,
                        }
                    }
                    // Dynamic: Match
                    Kont::Match(mut arms, mut env) => {
                        let sum = val.sum_type();
                        let Arm { bind, body } = arms.remove(&*sum.ctor).unwrap();
                        env.local.insert(bind, (*sum.inner).clone());
                        Self {
                            ctrl: Ctrl::Term(body),
                            env,
                            cont,
                        }
                    }
                    // Dynamic: Tag
                    Kont::Tag(tag) => Self {
                        ctrl: Ctrl::Value(Value::SumType(SumType {
                            ctor: tag,
                            inner: val.into(),
                        })),
                        env: Default::default(),
                        cont,
                    },
                }
            }
            Ctrl::Term(Term::Tuple(mut terms)) => {
                if let Some(fst) = terms.pop_front() {
                    // Dynamic: Tup-1
                    cont.push(Kont::Tuple {
                        env: self.env.clone(),
                        done: Default::default(),
                        todo: terms,
                    });
                    Self {
                        ctrl: Ctrl::Term(fst),
                        env: self.env,
                        cont,
                    }
                } else {
                    // Dynamic: Unit
                    Self {
                        ctrl: Ctrl::Value(Value::Tuple(Tuple(im::Vector::new()))),
                        env: Default::default(),
                        cont,
                    }
                }
            }
            // Dynamic: Nil
            Ctrl::Term(Term::NilLiteral) => Self {
                ctrl: Ctrl::Value(Value::List(Default::default())),
                env: Default::default(),
                cont,
            },
            // Dynamic: IntLiteral
            Ctrl::Term(Term::IntLiteral(val)) => Self {
                ctrl: Ctrl::Value(Value::Int(val.into())),
                env: Default::default(),
                cont,
            },
            // Dynamic: TrueLiteral / FalseLiteral
            Ctrl::Term(Term::BoolLiteral(val)) => Self {
                ctrl: Ctrl::Value(Value::Bool(val)),
                env: Default::default(),
                cont,
            },
            // Dynamic: Box
            Ctrl::Term(Term::Box(BxTerm { body })) => Self {
                ctrl: Ctrl::Value(Value::Box(Bx {
                    body,
                    env: self.env.global,
                })),
                env: Default::default(),
                cont,
            },
            // Dynamic: Func
            Ctrl::Term(Term::Function(FuncTerm { binding, body })) => Self {
                ctrl: Ctrl::Value(Value::Func(Func {
                    binding,
                    body,
                    env: self.env,
                })),
                env: Default::default(),
                cont,
            },
            // Dynamic: Fix
            Ctrl::Term(Term::Fix(fix @ Fix { .. })) => Self {
                ctrl: Ctrl::Value(Value::Fix(fix)),
                env: Default::default(),
                cont,
            },
            // Dynamic: Local
            Ctrl::Term(Term::LocalVariable(ident)) => {
                let Some(val) = self.env.local.get(&ident).cloned() else {
                    panic!(
                        "Could not find local variable {ident} in scope {}",
                        self.env
                    )
                };
                Self {
                    ctrl: Ctrl::Value(val),
                    env: Default::default(),
                    cont,
                }
            }
            // Dynamic: Global
            Ctrl::Term(Term::GlobalVariable(ident)) => {
                let Some(mobile) = self.env.global.get(&ident).cloned() else {
                    panic!(
                        "Could not find global variable {ident} in scope {}",
                        self.env
                    )
                };
                let val = (*mobile).clone();
                Self {
                    ctrl: Ctrl::Value(val),
                    env: Default::default(),
                    cont,
                }
            }
            // Dynamic: App-1
            Ctrl::Term(Term::Application(Application { func, arg })) => {
                cont.push(Kont::Arg((*arg).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*func).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: IfElse
            Ctrl::Term(Term::IfElse(IfElse {
                cond,
                if_true,
                if_false,
            })) => {
                cont.push(Kont::IfElse {
                    if_true: (*if_true).clone(),
                    if_false: (*if_false).clone(),
                    env: self.env.clone(),
                });
                Self {
                    ctrl: Ctrl::Term((*cond).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: Let
            Ctrl::Term(Term::LetBinding(LetBinding {
                binding,
                expr,
                body,
            })) => {
                cont.push(Kont::Bind(binding, (*body).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*expr).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: LetBox
            Ctrl::Term(Term::LetBoxBinding(LetBinding {
                binding,
                expr,
                body,
            })) => {
                cont.push(Kont::BindBox(binding, (*body).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*expr).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: BinOp-1
            Ctrl::Term(Term::BinaryPrimitive(BinaryPrimitive { op, lhs, rhs })) => {
                cont.push(Kont::BinaryPrimitive(op, (*rhs).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*lhs).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: Negate-1
            Ctrl::Term(Term::UnaryMinus(UnaryMinus(inner))) => {
                cont.push(Kont::UnaryMinus);
                Self {
                    ctrl: Ctrl::Term((*inner).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: Append-1
            Ctrl::Term(Term::Append(Append { list, item })) => {
                cont.push(Kont::Append((*item).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*list).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: Index-1
            Ctrl::Term(Term::Index(Index { list, index })) => {
                cont.push(Kont::ResolvList((*index).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*list).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: TupIndex-1
            Ctrl::Term(Term::IndexTuple(IndexTuple { tuple, index })) => {
                cont.push(Kont::IndexTuple(index));
                Self {
                    ctrl: Ctrl::Term((*tuple).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: Tag-1
            Ctrl::Term(Term::Tag(Tag { tag, body })) => {
                cont.push(Kont::Tag(tag));
                Self {
                    ctrl: Ctrl::Term((*body).clone()),
                    env: self.env,
                    cont,
                }
            }
            // Dynamic: Match-1
            Ctrl::Term(Term::Match(Match { expr, arms })) => {
                cont.push(Kont::Match(
                    arms.iter().cloned().collect(),
                    self.env.clone(),
                ));
                Self {
                    ctrl: Ctrl::Term((*expr).clone()),
                    env: self.env,
                    cont,
                }
            }
        }
    }

    pub fn finish(&self) -> Option<Value> {
        if !self.cont.is_empty() {
            return None;
        }
        match &self.ctrl {
            Ctrl::Value(val) => Some(val.clone()),
            _ => None,
        }
    }
}
