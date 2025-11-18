use std::ffi::c_double;

unsafe extern "C" {
    fn tgamma(n: c_double) -> c_double;
}

// TODO: This will be part of the standard in the future [1] [2]
// [1] https://github.com/rust-lang/rfcs/issues/864
// [2] https://github.com/rust-lang/rust/pull/99747
pub fn gamma(x: f64) -> f64 {
    unsafe { tgamma(x) }
}
