use std::sync::{Arc, OnceLock};

use rayon::Yield;

use crate::dynamics::*;

#[derive(Clone, Debug)]
pub enum Value {
    Unit,
    Int(i64),
    Bool(bool),
    List(List),
    Func(Func),
    Box(Bx),
    Fix(Fix),
}

impl Value {
    fn int(self) -> i64 {
        if let Self::Int(val) = self {
            val
        } else {
            todo!()
        }
    }

    fn bool(self) -> bool {
        if let Self::Bool(val) = self {
            val
        } else {
            todo!()
        }
    }

    fn func(self) -> Func {
        if let Self::Func(val) = self {
            val
        } else {
            todo!()
        }
    }

    fn bx(self) -> Bx {
        if let Self::Box(val) = self {
            val
        } else {
            todo!()
        }
    }

    fn list(self) -> List {
        if let Self::List(val) = self {
            val
        } else {
            todo!()
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct List(im::Vector<(Value, Env)>);

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "<>"),
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
                    .map(|v| format!("{}", v.0))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MobileValue(Arc<OnceLock<(Value, Env)>>);

impl MobileValue {
    pub fn compute(mut term: Cek) -> Self {
        let val = Arc::new(OnceLock::new());

        let v = val.clone();
        rayon::spawn(move || {
            while term.finish().is_none() {
                term = term.step()
            }
            let fin = term.finish().unwrap();
            let env = term.env;
            v.set((fin, env)).unwrap();
        });

        Self(val)
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(val) = self.0.get() {
            write!(f, "{}", val.0)
        } else {
            write!(f, "Uncomputed")
        }
    }
}

impl std::ops::Deref for MobileValue {
    type Target = (Value, Env);

    fn deref(&self) -> &Self::Target {
        if let Some(v) = self.0.get() {
            return v;
        }
        while let Some(Yield::Executed) = rayon::yield_now() {
            if let Some(v) = self.0.get() {
                return v;
            }
        }
        self.0.wait()
    }
}

#[derive(Debug)]
enum Ctrl {
    Value(Value),
    Term(Term),
}

#[derive(Default, Clone, Debug)]
pub struct Env {
    global: im::HashMap<Ident, MobileValue>,
    local: im::HashMap<Ident, (Value, Env)>,
}

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{local: [{}], global: [{}]}}",
            self.local
                .iter()
                .map(|v| format!("({}: {})", v.0, v.1 .0))
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
    Func(Func, Env),
    IfElse {
        if_true: Term,
        if_false: Term,
        env: Env,
    },
    UnaryMinus,
    BinaryPrimitive(BinaryOp, Term, Env),
    BinaryPrimitiveVal(BinaryOp, Value),
    Append(Term, Env),
    Push {
        list: List,
        old_env: Env,
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

    fn with_global(term: Term, global: im::HashMap<Ident, MobileValue>) -> Self {
        Self {
            ctrl: Ctrl::Term(term),
            env: Env {
                global,
                local: Default::default(),
            },
            cont: Vec::new(),
        }
    }

    pub fn step(self) -> Self {
        match self.ctrl {
            Ctrl::Value(Value::Fix(fix)) => {
                let mut nu_env = self.env.clone();
                let nu_fix = Value::Fix(fix.clone());
                nu_env.local.insert(fix.binding, (nu_fix, self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*fix.body).clone()),
                    env: nu_env,
                    cont: self.cont,
                }
            }
            Ctrl::Value(val) => {
                let mut cont = self.cont;
                match cont.pop().unwrap() {
                    // The continuation binds the control term.
                    Kont::Bind(ident, term, mut old_env) => {
                        old_env.local.insert(ident, (val, self.env.clone()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: old_env,
                            cont,
                        }
                    }
                    Kont::BindBox(ident, term, mut old_env) => {
                        let bx = val.bx();

                        // Create a mobile value from the globals of the old environment
                        let global_only = Env {
                            global: self.env.global.clone(),
                            ..Env::default()
                        };
                        let mv = MobileValue::compute(Self::with_global(
                            (*bx.body).clone(),
                            global_only.global.clone(),
                        ));

                        // Continue on our current thread
                        println!("Inserting mobile value {ident}");
                        old_env.global.insert(ident, mv);
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: old_env,
                            cont,
                        }
                    }
                    Kont::Arg(arg, orig_env) => {
                        cont.push(Kont::Func(val.func(), self.env));
                        Self {
                            ctrl: Ctrl::Term(arg),
                            env: orig_env,
                            cont,
                        }
                    }
                    // We have finished evaluating a function. Push a binding to cont and start
                    // computing the argument
                    Kont::Func(Func { binding, body }, mut func_env) => {
                        func_env.local.insert(binding, (val, self.env));
                        Self {
                            ctrl: Ctrl::Term((*body).clone()),
                            env: func_env,
                            cont,
                        }
                    }
                    Kont::IfElse {
                        if_true,
                        if_false,
                        env,
                    } => Self {
                        ctrl: Ctrl::Term(if val.bool() { if_true } else { if_false }),
                        env,
                        cont,
                    },
                    Kont::BinaryPrimitive(prim, term, env) => {
                        cont.push(Kont::BinaryPrimitiveVal(prim, val));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
                            cont,
                        }
                    }
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
                            env: self.env,
                            cont,
                        }
                    }
                    Kont::UnaryMinus => {
                        let v = val.int();
                        Self {
                            ctrl: Ctrl::Value(Value::Int(-v)),
                            env: self.env,
                            cont,
                        }
                    }
                    Kont::Append(term, env) => {
                        cont.push(Kont::Push {
                            list: val.list(),
                            old_env: env.clone(),
                        });
                        Self {
                            ctrl: Ctrl::Term(term),
                            env: env.clone(),
                            cont,
                        }
                    }
                    Kont::Push { mut list, old_env } => {
                        list.0.push_back((val, self.env));
                        Self {
                            ctrl: Ctrl::Value(Value::List(list)),
                            env: old_env,
                            cont,
                        }
                    }
                }
            }
            Ctrl::Term(Term::UnitLiteral) => Self {
                ctrl: Ctrl::Value(Value::Unit),
                ..self
            },
            Ctrl::Term(Term::NilLiteral) => Self {
                ctrl: Ctrl::Value(Value::List(Default::default())),
                ..self
            },
            Ctrl::Term(Term::IntLiteral(val)) => Self {
                ctrl: Ctrl::Value(Value::Int(val)),
                ..self
            },
            Ctrl::Term(Term::BoolLiteral(val)) => Self {
                ctrl: Ctrl::Value(Value::Bool(val)),
                ..self
            },
            Ctrl::Term(Term::Function(val)) => Self {
                ctrl: Ctrl::Value(Value::Func(val)),
                ..self
            },
            Ctrl::Term(Term::Box(val)) => Self {
                ctrl: Ctrl::Value(Value::Box(val)),
                ..self
            },
            Ctrl::Term(Term::LocalVariable(ident)) => {
                let Some((val, env)) = self.env.local.get(&ident).cloned() else {
                    panic!(
                        "Could not find local variable {ident} in scope {}",
                        self.env
                    )
                };
                Self {
                    ctrl: Ctrl::Value(val),
                    env,
                    cont: self.cont,
                }
            }
            Ctrl::Term(Term::GlobalVariable(ident)) => {
                let Some(mobile) = self.env.global.get(&ident).cloned() else {
                    panic!(
                        "Could not find global variable {ident} in scope {}",
                        self.env
                    )
                };
                let (val, env) = (*mobile).clone();
                Self {
                    ctrl: Ctrl::Value(val),
                    env,
                    cont: self.cont,
                }
            }
            Ctrl::Term(Term::Application(Application { func, arg })) => {
                let mut cont = self.cont;
                cont.push(Kont::Arg((*arg).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*func).clone()),
                    env: self.env,
                    cont,
                }
            }
            Ctrl::Term(Term::IfElse(IfElse {
                cond,
                if_true,
                if_false,
            })) => {
                let mut cont = self.cont;
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
            Ctrl::Term(Term::LetBinding(LetBinding {
                binding,
                expr,
                body,
            })) => {
                let mut cont = self.cont;
                cont.push(Kont::Bind(binding, (*body).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*expr).clone()),
                    env: self.env,
                    cont,
                }
            }
            Ctrl::Term(Term::LetBoxBinding(LetBinding {
                binding,
                expr,
                body,
            })) => {
                let mut cont = self.cont;
                cont.push(Kont::BindBox(binding, (*body).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*expr).clone()),
                    env: self.env,
                    cont,
                }
            }
            Ctrl::Term(Term::Fix(fix @ Fix { .. })) => Self {
                ctrl: Ctrl::Value(Value::Fix(fix)),
                ..self
            },
            Ctrl::Term(Term::BinaryPrimitive(BinaryPrimitive { op, lhs, rhs })) => {
                let mut cont = self.cont;
                cont.push(Kont::BinaryPrimitive(op, (*rhs).clone(), self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*lhs).clone()),
                    env: self.env,
                    cont,
                }
            }
            Ctrl::Term(Term::UnaryMinus(UnaryMinus(inner))) => {
                let mut cont = self.cont;
                cont.push(Kont::UnaryMinus);
                Self {
                    ctrl: Ctrl::Term((*inner).clone()),
                    env: self.env,
                    cont,
                }
            }
            Ctrl::Term(Term::Append(Append { list, item })) => {
                let mut cont = self.cont;
                cont.push(Kont::Append((*item).clone(), self.env.clone()));
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
