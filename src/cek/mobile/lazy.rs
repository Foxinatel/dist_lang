use std::{ops::Deref, sync::LazyLock};

use crate::cek::Value;

struct EvalCek(crate::cek::Cek);

impl FnOnce<()> for EvalCek {
    type Output = Value;

    extern "rust-call" fn call_once(self, _: ()) -> Self::Output {
        let mut cek = self.0;
        while cek.finish().is_none() {
            cek = cek.step();
        }
        cek.finish().unwrap()
    }
}

#[derive(Debug)]
pub struct MobileValue(LazyLock<Value, EvalCek>);

impl super::Mobile for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(cek: crate::cek::Cek) -> Self {
        Self(LazyLock::new(EvalCek(cek)))
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", *self.0)
    }
}

impl Deref for MobileValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
