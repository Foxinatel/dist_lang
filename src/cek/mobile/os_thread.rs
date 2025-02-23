use std::sync::{Arc, OnceLock};

use crate::cek::{Cek, Value};

#[derive(Clone, Debug)]
pub struct MobileValue(Arc<OnceLock<Value>>);

impl super::Mobile for MobileValue {}

impl super::BuildableMobileValue for MobileValue {
    fn compute(mut term: Cek) -> Self {
        let val = Arc::new(OnceLock::new());

        let v = val.clone();
        std::thread::spawn(move || {
            while term.finish().is_none() {
                term = term.step()
            }
            let fin = term.finish().unwrap();
            v.set(fin).unwrap();
        });

        Self(val)
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
        self.0.wait()
    }
}
