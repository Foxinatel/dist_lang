use std::{ops::Deref, sync::Arc};

use crate::cek::{Env, Value};

#[derive(Debug)]
pub struct MobileValue(Arc<(Value, Env)>);

impl super::MobileValue for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(mut cek: crate::cek::Cek) -> Self {
        while cek.finish().is_none() {
            cek = cek.step();
        }
        Self(Arc::new((cek.finish().unwrap(), cek.env)))
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.0)
    }
}

impl Deref for MobileValue {
    type Target = (Value, Env);

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
