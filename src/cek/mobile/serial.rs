use std::{ops::Deref, sync::Arc};

use crate::cek::Value;

#[derive(Debug)]
pub struct MobileValue(Arc<Value>);

impl super::Mobile for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(mut cek: crate::cek::Cek) -> Self {
        while cek.finish().is_none() {
            cek = cek.step();
        }
        Self(Arc::new(cek.finish().unwrap()))
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for MobileValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
