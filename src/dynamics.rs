use std::sync::Arc;

use malachite::{Integer, Natural};

pub type Ident = Arc<str>;

pub trait Mobile:
    std::fmt::Display + std::fmt::Debug + std::ops::Deref<Target = Value> + Sync + Send
{
}

pub type MobileValue = Arc<dyn Mobile + 'static>;

#[derive(Clone, Debug)]
pub enum Value {
    Tuple(Tuple),
    Int(Integer),
    Bool(bool),
    List(List),
    SumType(SumType),
    Box(Bx),
    Func(Func),
    Fix(Func),
}

#[derive(Clone, Debug, Default)]
pub struct List(pub im::Vector<Value>);

#[derive(Clone, Debug)]
pub struct Tuple(pub im::Vector<Value>);

#[derive(Clone, Debug)]
pub struct SumType {
    pub ctor: Arc<String>,
    pub inner: Arc<Value>,
}

#[derive(Clone, Debug)]
pub struct Bx {
    pub body: Arc<Term>,
    pub env: GlobalEnv,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub binding: Ident,
    pub body: Arc<Term>,
    pub env: Env,
}

impl Value {
    pub fn int(self) -> Integer {
        if let Self::Int(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn bool(self) -> bool {
        if let Self::Bool(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn func(self) -> Func {
        if let Self::Func(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn bx(self) -> Bx {
        if let Self::Box(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn list(self) -> List {
        if let Self::List(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn tuple(self) -> Tuple {
        if let Self::Tuple(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn sum_type(self) -> SumType {
        if let Self::SumType(val) = self {
            val
        } else {
            todo!()
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    NilLiteral,
    BoolLiteral(bool),
    IntLiteral(Natural),
    Tuple(im::Vector<Term>),
    Box(BxTerm),
    LocalVariable(Ident),
    GlobalVariable(Ident),
    Function(FuncTerm),
    Application(Application),
    IfElse(IfElse),
    LetBinding(LetBinding),
    LetBoxBinding(LetBinding),
    Fix(FuncTerm),
    UnaryMinus(UnaryMinus),
    BinaryPrimitive(BinaryPrimitive),
    Append(Append),
    Index(Index),
    Slice(Slice),
    IndexTuple(IndexTuple),
    Tag(Tag),
    Match(Match),
    Length(Arc<Term>),
}

pub type GlobalEnv = im::HashMap<Ident, MobileValue>;

#[derive(Default, Clone, Debug)]
pub struct Env {
    pub global: GlobalEnv,
    pub local: im::HashMap<Ident, Value>,
}

#[derive(Clone, Debug)]
pub struct FuncTerm {
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
pub struct BxTerm {
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

#[derive(Clone, Debug)]
pub struct Slice {
    pub list: Arc<Term>,
    pub lower: Option<Arc<Term>>,
    pub upper: Option<Arc<Term>>,
}

#[derive(Clone, Debug)]
pub struct IndexTuple {
    pub tuple: Arc<Term>,
    pub index: u64,
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

#[derive(Clone, Debug)]
pub struct Tag {
    pub tag: Arc<String>,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug)]
pub struct Match {
    pub expr: Arc<Term>,
    pub arms: Arc<[(String, Arm)]>,
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub bind: Ident,
    pub body: Term,
}
