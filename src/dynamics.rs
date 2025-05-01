use std::sync::Arc;

use malachite::{Integer, Natural};
use serde::{Deserialize, Serialize};

pub type Ident = Arc<str>;

pub trait Mobile:
    std::fmt::Display + std::fmt::Debug + std::ops::Deref<Target = Value> + Sync + Send
{
}

impl<T> Mobile for T where
    T: std::fmt::Display + std::fmt::Debug + std::ops::Deref<Target = Value> + Sync + Send
{
}

pub type MobileValue = Arc<dyn Mobile + 'static>;

#[derive(Clone, Debug)]
pub enum Value {
    // Tuple(Tuple),
    Int(Integer),
    Bool(bool),
    Arr(Arr),
    SumType(SumType),
    Box(Bx),
    Fix(Fix),
    MFix(MFix),
}

#[derive(Clone, Debug, Default)]
pub struct Arr(pub imbl::Vector<Value>);

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
pub struct Fix {
    pub fix_binding: Ident,
    pub inner_binding: Ident,
    pub body: Arc<Term>,
    pub env: Env,
}

#[derive(Clone, Debug)]
pub struct MFix {
    pub fix_binding: Ident,
    pub inner_binding: Ident,
    pub body: Arc<Term>,
    pub env: GlobalEnv,
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

    pub fn fix(self) -> Fix {
        if let Self::Fix(val) = self {
            val
        } else {
            todo!()
        }
    }

    pub fn arr(self) -> Arr {
        if let Self::Arr(val) = self {
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Term {
    NilLiteral,
    BoolLiteral(bool),
    IntLiteral(Natural),
    Box(BxTerm),
    LocalVariable(Ident),
    GlobalVariable(Ident),
    Application(Application),
    IfElse(IfElse),
    LetBinding(LetBinding),
    LetBoxBinding(LetBinding),
    Fix(FixTerm),
    MFix(FixTerm),
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

pub type GlobalEnv = imbl::HashMap<Ident, MobileValue>;
pub type LocalEnv = imbl::HashMap<Ident, Value>;

#[derive(Default, Clone, Debug)]
pub struct Env {
    pub global: GlobalEnv,
    pub local: LocalEnv,
}

// #[derive(Clone, Debug, Serialize, Deserialize)]
// pub struct FuncTerm {
//     pub binding: Ident,
//     pub body: Arc<Term>,
// }

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FixTerm {
    pub fix_binding: Ident,
    pub inner_binding: Ident,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Application {
    pub func: Arc<Term>,
    pub arg: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IfElse {
    pub cond: Arc<Term>,
    pub if_true: Arc<Term>,
    pub if_false: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LetBinding {
    pub binding: Ident,
    pub expr: Arc<Term>,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BxTerm {
    pub body: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Append {
    pub list: Arc<Term>,
    pub item: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Index {
    pub list: Arc<Term>,
    pub index: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Slice {
    pub list: Arc<Term>,
    pub lower: Option<Arc<Term>>,
    pub upper: Option<Arc<Term>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexTuple {
    pub tuple: Arc<Term>,
    pub index: u64,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BinaryPrimitive {
    pub op: BinaryOp,
    pub lhs: Arc<Term>,
    pub rhs: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UnaryMinus(pub Arc<Term>);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Tag {
    pub tag: Arc<String>,
    pub body: Arc<Term>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Match {
    pub expr: Arc<Term>,
    pub arms: Arc<[(String, Arm)]>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Arm {
    pub bind: Ident,
    pub body: Term,
}
