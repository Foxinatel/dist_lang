use std::sync::Arc;

pub type Ident = String;

#[derive(Clone, Debug)]
pub enum Term {
    UnitLiteral,
    NilLiteral,
    BoolLiteral(bool),
    IntLiteral(i64),
    Box(Bx),
    LocalVariable(Ident),
    GlobalVariable(Ident),
    Function(Func),
    Application(Application),
    IfElse(IfElse),
    LetBinding(LetBinding),
    LetBoxBinding(LetBinding),
    Fix(Fix),
    UnaryMinus(UnaryMinus),
    BinaryPrimitive(BinaryPrimitive),
    Append(Append),
    Index(Index),
}

#[derive(Clone, Debug)]
pub struct Func {
    pub binding: Ident,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct Application {
    pub func: Arc<Term>,
    pub arg: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct IfElse {
    pub cond: Arc<Term>,
    pub if_true: Arc<Term>,
    pub if_false: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct LetBinding {
    pub binding: Ident,
    pub expr: Arc<Term>,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct Fix {
    pub binding: Ident,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct Bx {
    pub body: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct Append {
    pub list: Arc<Term>,
    pub item: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct Index {
    pub list: Arc<Term>,
    pub index: Arc<Term>,
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Debug)]
pub struct BinaryPrimitive {
    pub op: BinaryOp,
    pub lhs: Arc<Term>,
    pub rhs: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct UnaryMinus(pub Arc<Term>);
