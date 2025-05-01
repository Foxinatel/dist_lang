use std::sync::{Arc, OnceLock};

use rayon::Yield;

use crate::cek::{Cek, Value};

#[derive(Clone, Debug)]
pub struct MobileValue(Arc<OnceLock<Value>>);

impl super::BuildableMobileValue for MobileValue {
    fn compute(mut term: Cek) -> Self {
        let val = Arc::new(OnceLock::new());

        let v = val.clone();
        rayon::spawn(move || {
            while term.finish().is_none() {
                term = term.step()
            }
            let fin = term.finish().unwrap();
            v.set(fin).unwrap();
        });

        Self(val)
    }

    fn with_value(val: impl FnOnce(Self) -> Value) -> Self {
        let mobile = Self(Arc::new(OnceLock::new()));
        mobile.0.set(val(mobile.clone())).unwrap();
        mobile
    }
}

impl std::fmt::Display for MobileValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(val) = self.0.get() {
            write!(f, "{}", val)
        } else {
            write!(f, "Uncomputed")
        }
    }
}

impl std::ops::Deref for MobileValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        if let Some(v) = self.0.get() {
            return v;
        }
        while let Some(Yield::Executed) = rayon::yield_now() {
            if let Some(v) = self.0.get() {
                return v;
            }
        }
        self.0.wait()
    }
}