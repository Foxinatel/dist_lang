use std::{ops::Deref, sync::LazyLock};

use crate::cek::Value;

#[derive(Debug)]
pub struct MobileValue(LazyLock<Value, Box<dyn FnOnce() -> Value + Send>>);

impl super::Mobile for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(mut cek: crate::cek::Cek) -> Self {
        let f = move || {
            while cek.finish().is_none() {
                cek = cek.step();
            }
            cek.finish().unwrap()
        };

        Self(LazyLock::new(Box::new(f)))
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
