use std::{
    ops::Deref,
    sync::{Arc, LazyLock, OnceLock},
};

use crate::cek::Value;

enum MaybeEvalCek {
    NeedsEval(crate::cek::Cek),
    JustValue(Value),
}

impl FnOnce<()> for MaybeEvalCek {
    type Output = Value;

    extern "rust-call" fn call_once(self, _: ()) -> Self::Output {
        match self {
            MaybeEvalCek::NeedsEval(mut cek) => {
                while cek.finish().is_none() {
                    cek = cek.step();
                }
                cek.finish().unwrap()
            }
            MaybeEvalCek::JustValue(value) => value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MobileValue(Arc<OnceLock<LazyLock<Value, MaybeEvalCek>>>);

impl super::Mobile for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(cek: crate::cek::Cek) -> Self {
        Self(Arc::new(OnceLock::from(LazyLock::new(
            MaybeEvalCek::NeedsEval(cek),
        ))))
    }

    fn with_value(val: impl FnOnce(Self) -> Value) -> Self {
        let mobile = Self(Arc::new(OnceLock::new()));
        mobile
            .0
            .set(LazyLock::new(MaybeEvalCek::JustValue(val(mobile.clone()))))
            .unwrap();
        mobile
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ll = self.0.get().unwrap();
        if let Some(val) = LazyLock::get(ll) {
            write!(f, "{}", val)
        } else {
            write!(f, "uninitialised")
        }
    }
}

impl Deref for MobileValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.0.get().unwrap()
    }
}
