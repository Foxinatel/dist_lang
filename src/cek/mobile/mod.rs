use crate::dynamics::Mobile;

mod os_thread;
mod rayon;
mod serial;

pub trait BuildableMobileValue: Mobile {
    fn compute(cek: super::Cek) -> Self;
}

pub trait MobileValueBuilder<T: BuildableMobileValue + 'static> {
    fn compute(cek: super::Cek) -> std::sync::Arc<dyn Mobile> {
        std::sync::Arc::new(T::compute(cek))
    }
}

pub struct RayonMobileValueBuilder;
impl MobileValueBuilder<rayon::MobileValue> for RayonMobileValueBuilder {}

pub struct SerialMobileValueBuilder;
impl MobileValueBuilder<serial::MobileValue> for SerialMobileValueBuilder {}

pub struct OsThreadMobileValueBuilder;
impl MobileValueBuilder<os_thread::MobileValue> for OsThreadMobileValueBuilder {}
