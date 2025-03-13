use std::{
    ops::Deref,
    sync::{Arc, OnceLock},
};

use crate::cek::Value;

#[derive(Debug, Clone)]
pub struct MobileValue(Arc<OnceLock<Value>>);

impl super::Mobile for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(mut cek: crate::cek::Cek) -> Self {
        while cek.finish().is_none() {
            cek = cek.step();
        }
        Self(Arc::new(cek.finish().unwrap().into()))
    }

    fn with_value(val: impl FnOnce(Self) -> Value) -> Self {
        let mobile = Self(Arc::new(OnceLock::new()));
        mobile.0.set(val(mobile.clone())).unwrap();
        mobile
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Deref for MobileValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.0.get().unwrap()
    }
}
