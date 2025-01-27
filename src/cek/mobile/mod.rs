use std::ops::Deref;

use super::{Env, Value};

mod os_thread;
mod rayon;
mod serial;

pub trait MobileValue:
    std::fmt::Display + std::fmt::Debug + Deref<Target = (Value, Env)> + Sync + Send
{
}

pub trait BuildableMobileValue: MobileValue {
    fn compute(cek: super::Cek) -> Self;
}

pub trait MobileValueBuilder<T: BuildableMobileValue + 'static> {
    fn compute(cek: super::Cek) -> std::sync::Arc<dyn MobileValue> {
        std::sync::Arc::new(T::compute(cek))
    }
}

pub struct RayonMobileValueBuilder;
impl MobileValueBuilder<rayon::MobileValue> for RayonMobileValueBuilder {}

pub struct SerialMobileValueBuilder;
impl MobileValueBuilder<serial::MobileValue> for SerialMobileValueBuilder {}

pub struct OsThreadMobileValueBuilder;
impl MobileValueBuilder<os_thread::MobileValue> for OsThreadMobileValueBuilder {}
