use std::rc::Rc;

use crate::dynamics::*;

#[derive(Clone, Debug)]
enum Value {
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
struct MobileValue;

impl MobileValue {
    pub fn compute(term: Bx) -> Self {
        todo!()
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
    Arg(Term, Env),
    IfElse {
        if_true: Term,
        if_false: Term,
        env: Env,
    },
}

#[derive(Debug)]
pub struct State {
    ctrl: Ctrl,
    env: Env,
    cont: Vec<Kont>,
}

impl State {
    pub fn new(term: Term) -> Self {
        Self {
            ctrl: Ctrl::Term(term),
            env: Env::default(),
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
                        if let Value::Box(bx) = val {
                            todo!()
                        } else {
                            env.local.insert(ident, (val, self.env.clone()));
                            Self {
                                ctrl: Ctrl::Term(term),
                                env,
                                cont,
                            }
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
                todo!()
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
            Ctrl::Term(Term::Fix(Fix { binding, body })) => {
                todo!()
            }
        }
    }

    pub fn is_done(&self) -> bool {
        matches!(self.ctrl, Ctrl::Value(_)) && self.cont.is_empty()
    }
}
