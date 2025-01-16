use std::sync::{Arc, OnceLock};

use rayon::Yield;

use crate::dynamics::*;

#[derive(Clone, Debug)]
pub enum Value {
    Unit,
    Int(i64),
    Bool(bool),
    Func(Func),
    Box(Bx),
}

impl Value {
    fn unit(self) {
        let Self::Unit = self else { todo!() };
    }

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
}

#[derive(Clone, Debug)]
pub struct MobileValue(Arc<OnceLock<Value>>);

impl MobileValue {
    pub fn compute(mut term: CEK) -> Self {
        let val = Arc::new(OnceLock::new());

        let v = val.clone();
        rayon::spawn(move || {
            while term.finish().is_none() {
                term = term.step()
            }
            v.set(term.finish().unwrap()).unwrap();
        });

        Self(val)
    }
}

impl std::ops::Deref for MobileValue {
    type Target = Value;

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
struct Env {
    global: im::HashMap<Ident, (MobileValue, Env)>,
    local: im::HashMap<Ident, (Value, Env)>,
}

#[derive(Debug)]
enum Kont {
    Term(Term),
    Bind(Ident, Term, Env),
    BindBox(Ident, Term, Env),
    Arg(Term, Env),
    IfElse {
        if_true: Term,
        if_false: Term,
        env: Env,
    },
    UnaryMinus(Env),
    BinaryPrimitive(BinaryOp, Term, Env),
    BinaryPrimitiveVal(BinaryOp, Value, Env),
}

#[derive(Debug)]
pub struct CEK {
    ctrl: Ctrl,
    env: Env,
    cont: Vec<Kont>,
}

impl CEK {
    pub fn new(term: Term) -> Self {
        Self {
            ctrl: Ctrl::Term(term),
            env: Env::default(),
            cont: Vec::new(),
        }
    }

    fn with_global(term: Term, global: im::HashMap<Ident, (MobileValue, Env)>) -> Self {
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
            Ctrl::Value(val) => {
                let mut cont = self.cont;
                match cont.pop().unwrap() {
                    // The continuation has a new term. Run it.
                    Kont::Term(term) => Self {
                        ctrl: Ctrl::Term(term),
                        env: self.env,
                        cont,
                    },
                    // The continuation binds the control term.
                    Kont::Bind(ident, term, mut env) => {
                        env.local.insert(ident, (val, self.env.clone()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
                            cont,
                        }
                    }
                    Kont::BindBox(ident, term, mut env) => {
                        let Value::Box(bx) = val else { todo!() };
                        let mv = MobileValue::compute(Self::with_global(
                            (*bx.body).clone(),
                            self.env.clone().global,
                        ));
                        env.global.insert(ident, (mv, self.env.clone()));

                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
                            cont,
                        }
                    }
                    // We have finished evaluating a function. Push a binding to cont and start
                    // computing the argument
                    Kont::Arg(arg, arg_env) => {
                        let Func { binding, body } = val.func();
                        cont.push(Kont::Bind(binding, (*body).clone(), self.env));
                        Self {
                            ctrl: Ctrl::Term(arg),
                            env: arg_env,
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
                        cont.push(Kont::BinaryPrimitiveVal(prim, val, env.clone()));
                        Self {
                            ctrl: Ctrl::Term(term),
                            env,
                            cont,
                        }
                    }
                    Kont::BinaryPrimitiveVal(prim, lhs, env) => {
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
                            env,
                            cont,
                        }
                    }
                    Kont::UnaryMinus(env) => {
                        let v = val.int();
                        Self {
                            ctrl: Ctrl::Value(Value::Int(-v)),
                            env,
                            cont,
                        }
                    }
                }
            }
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
                let (val, env) = self.env.local.get(&ident).unwrap().clone();
                Self {
                    ctrl: Ctrl::Value(val),
                    env,
                    cont: self.cont,
                }
            }
            Ctrl::Term(Term::GlobalVariable(ident)) => {
                let (val, env) = self.env.global.get(&ident).unwrap().clone();
                Self {
                    ctrl: Ctrl::Value((*val).clone()),
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
            Ctrl::Term(Term::Fix(Fix { binding, body })) => {
                todo!()
            }
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
                cont.push(Kont::UnaryMinus(self.env.clone()));
                Self {
                    ctrl: Ctrl::Term((*inner).clone()),
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
