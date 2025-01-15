use std::rc::Rc;

pub type Ident = String;

#[derive(Clone, Debug)]
pub enum Term {
    BoolLiteral(bool),
    IntLiteral(i64),
    Box(Bx),
    LocalVariable(Ident),
    GlobalVariable(Ident),
    Function(Func),
    Application(Application),
    IfElse(IfElse),
    LetBinding(LetBinding),
    Fix(Fix),
}

#[derive(Clone, Debug)]
pub struct Func {
    pub binding: Ident,
    pub body: Rc<Term>,
}

#[derive(Clone, Debug)]
pub struct Application {
    pub func: Rc<Term>,
    pub arg: Rc<Term>,
}

#[derive(Clone, Debug)]
pub struct IfElse {
    pub cond: Rc<Term>,
    pub if_true: Rc<Term>,
    pub if_false: Rc<Term>,
}

#[derive(Clone, Debug)]
pub struct LetBinding {
    pub binding: Ident,
    pub expr: Rc<Term>,
    pub body: Rc<Term>,
}

#[derive(Clone, Debug)]
pub struct Fix {
    pub binding: Ident,
    pub body: Rc<Term>,
}

#[derive(Clone, Debug)]
pub struct Bx {
    pub body: Rc<Term>,
}
