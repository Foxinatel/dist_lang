use std::sync::Arc;

mod mobile;
use mobile::MobileValueBuilder as _;

use crate::dynamics::*;

// type MobileValueBuilder = mobile::SerialMobileValueBuilder;
// type MobileValueBuilder = mobile::LazyMobileValueBuilder;
// type MobileValueBuilder = mobile::OsThreadMobileValueBuilder;
type MobileValueBuilder = mobile::RayonMobileValueBuilder;

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(val) => write!(f, "{val:?}"),
            Value::Bool(val) => write!(f, "{val:?}"),
            Value::Box(_) => write!(f, "box (...)"),
            Value::Fix(fix) => write!(
                f,
                "fix {} {} -> (...)",
                if fix.fix_binding.is_empty() {
                    "!"
                } else {
                    &fix.fix_binding
                },
                fix.inner_binding
            ),
            Value::Arr(vec) => write!(
                f,
                "{{{}}}",
                vec.0
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::SumType(sum) => write!(f, "{} {:?}", sum.ctor, sum.inner),
            Value::MFix(fix) => write!(
                f,
                "mfix {} {} -> (...)",
                if fix.fix_binding.is_empty() {
                    "!"
                } else {
                    &fix.fix_binding
                },
                fix.inner_binding
            ),
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
    Bind(Ident, Term, Env),
    BindBox(Ident, Term, Env),
    Arg(Term, Env),
    Func(Fix),
    IfElse {
        if_true: Term,
        if_false: Term,
        env: Env,
    },
    UnaryMinus,
    BinaryPrimitive(BinaryOp, Term, Env),
    BinaryPrimitiveVal(BinaryOp, Value),
    Append(Term, Env),
    Push(Arr),
    ResolvList(Term, Env),
    Index(Arr),
    IndexTuple(u64),
    Match(imbl::HashMap<String, Arm>, Env),
    Tag(Arc<String>),
    Len,
    Slice1 {
        lower: Option<Term>,
        upper: Option<Term>,
        env: Env,
    },
    Slice2 {
        list: Arr,
        upper: Option<Term>,
        env: Env,
    },
    Slice3 {
        list: Arr,
        lower: u64,
    },
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
                    Kont::BindBox(ident, term, mut env) => {
                        match val {
                            Value::Box(bx) => {
                                let mv = MobileValueBuilder::compute(Self::from_box(bx));

                                // Continue on our current thread
                                env.global.insert(ident, mv);
                                Self {
                                    ctrl: Ctrl::Term(term),
                                    env,
                                    cont,
                                }
                            }
                            Value::MFix(fix) => {
                                let func = MobileValueBuilder::with_value(|this| {
                                    let mut env = fix.env;
                                    env.insert(fix.fix_binding, Arc::new(this));
                                    Value::Fix(Fix {
                                        fix_binding: "".into(),
                                        inner_binding: fix.inner_binding,
                                        body: fix.body,
                                        env: Env {
                                            global: env,
                                            ..Default::default()
                                        },
                                    })
                                });
                                env.global.insert(ident, func);
                                Self {
                                    ctrl: Ctrl::Term(term),
                                    env,
                                    cont,
                                }
                            }
                            v => todo!("Tried to bindbox with {}", v),
                        }
                    }
                    // Dynamic: App-2
                    Kont::Arg(arg, orig_env) => {
                        cont.push(Kont::Func(val.fix()));
                        Self {
                            ctrl: Ctrl::Term(arg),
                            env: orig_env,
                            cont,
                        }
                    }
                    // Dynamic: App
                    Kont::Func(fx) => {
                        let Fix {
                            fix_binding,
                            inner_binding,
                            body,
                            mut env,
                        } = fx.clone();
                        env.local.insert(fix_binding, Value::Fix(fx));
                        env.local.insert(inner_binding, val);
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
                        cont.push(Kont::Push(val.arr()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
                            cont,
                        }
                    }
                    // Dynamic: Append
                    Kont::Push(mut list) => {
                        list.0.push_back(val);
                        Self {
                            ctrl: Ctrl::Value(Value::Arr(list)),
                            env: Default::default(),
                            cont,
                        }
                    }
                    // Dynamic: Index-2
                    Kont::ResolvList(term, env) => {
                        cont.push(Kont::Index(val.arr()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
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
                    // Dynamic: TupIndex
                    Kont::IndexTuple(ind) => {
                        let Arr(vals) = val.arr();
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
                    Kont::Len => Self {
                        ctrl: Ctrl::Value(Value::Int(val.arr().0.len().into())),
                        env: Default::default(),
                        cont,
                    },
                    Kont::Slice1 { lower, upper, env } => {
                        if let Some(lower) = lower {
                            cont.push(Kont::Slice2 {
                                list: val.arr(),
                                upper,
                                env: env.clone(),
                            });
                            Self {
                                ctrl: Ctrl::Term(lower),
                                env,
                                cont,
                            }
                        } else if let Some(upper) = upper {
                            cont.push(Kont::Slice3 {
                                list: val.arr(),
                                lower: 0,
                            });
                            Self {
                                ctrl: Ctrl::Term(upper),
                                env,
                                cont,
                            }
                        } else {
                            let Arr(vals) = val.arr();
                            Self {
                                ctrl: Ctrl::Value(Value::Arr(Arr(vals.clone()))),
                                env,
                                cont,
                            }
                        }
                    }
                    Kont::Slice2 { list, upper, env } => {
                        if let Some(upper) = upper {
                            cont.push(Kont::Slice3 { list, lower: 0 });
                            Self {
                                ctrl: Ctrl::Term(upper),
                                env,
                                cont,
                            }
                        } else {
                            let Arr(mut vals) = list;
                            let ind: usize = (&val.int()).try_into().unwrap();
                            Self {
                                ctrl: Ctrl::Value(Value::Arr(Arr(vals.slice(ind..)))),
                                env,
                                cont,
                            }
                        }
                    }
                    Kont::Slice3 {
                        list: Arr(mut vals),
                        lower,
                    } => {
                        let upper: usize = (&val.int()).try_into().unwrap();
                        let slice = vals.slice((lower as usize)..upper);
                        Self {
                            ctrl: Ctrl::Value(Value::Arr(Arr(slice))),
                            env: Default::default(),
                            cont,
                        }
                    }
                }
            }
            Ctrl::Term(Term::NilLiteral) => {
                // Dynamic: Nil
                Self {
                    ctrl: Ctrl::Value(Value::Arr(Default::default())),
                    env: Default::default(),
                    cont,
                }
            }
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
            // Dynamic: Fix
            Ctrl::Term(Term::Fix(FixTerm {
                fix_binding,
                inner_binding,
                body,
            })) => Self {
                ctrl: Ctrl::Value(Value::Fix(Fix {
                    fix_binding,
                    inner_binding,
                    body,
                    env: self.env,
                })),
                env: Default::default(),
                cont,
            },
            // Dynamic: MFix
            Ctrl::Term(Term::MFix(FixTerm {
                fix_binding,
                inner_binding,
                body,
            })) => Self {
                ctrl: Ctrl::Value(Value::MFix(MFix {
                    fix_binding,
                    inner_binding,
                    body,
                    env: self.env.global,
                })),
                env: Default::default(),
                cont,
            },
            // Dynamic: Local
            Ctrl::Term(Term::LocalVariable(ref ident)) => {
                let Some(val) = self.env.local.get(ident).cloned() else {
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
            Ctrl::Term(Term::Slice(Slice { list, lower, upper })) => {
                cont.push(Kont::Slice1 {
                    lower: lower.as_deref().cloned(),
                    upper: upper.as_deref().cloned(),
                    env: self.env.clone(),
                });
                Self {
                    ctrl: Ctrl::Term((*list).clone()),
                    env: self.env,
                    cont,
                }
            }
            Ctrl::Term(Term::Length(list)) => {
                cont.push(Kont::Len);
                Self {
                    ctrl: Ctrl::Term((*list).clone()),
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
