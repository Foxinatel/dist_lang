use crate::dynamics::{Mobile, Value};
use std::sync::Arc;

mod lazy;
mod os_thread;
mod rayon;
mod serial;

pub trait BuildableMobileValue: Mobile
where
    Self: std::marker::Sized,
{
    fn compute(cek: super::Cek) -> Self;
    fn with_value(val: impl FnOnce(Self) -> Value) -> Self;
}

pub trait MobileValueBuilder<T: BuildableMobileValue + 'static> {
    fn compute(cek: super::Cek) -> std::sync::Arc<dyn Mobile> {
        Arc::new(T::compute(cek))
    }

    fn with_value(val: impl FnOnce(T) -> Value) -> std::sync::Arc<dyn Mobile>
    where
        Self: std::marker::Sized,
    {
        Arc::new(T::with_value(val))
    }
}

pub struct RayonMobileValueBuilder;
impl MobileValueBuilder<rayon::MobileValue> for RayonMobileValueBuilder {}

pub struct SerialMobileValueBuilder;
impl MobileValueBuilder<serial::MobileValue> for SerialMobileValueBuilder {}

pub struct LazyMobileValueBuilder;
impl MobileValueBuilder<lazy::MobileValue> for LazyMobileValueBuilder {}

pub struct OsThreadMobileValueBuilder;
impl MobileValueBuilder<os_thread::MobileValue> for OsThreadMobileValueBuilder {}
