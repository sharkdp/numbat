/// Calculates the factorial of (the floor of) the given `f64`.
///
/// It is the caller's responsibility to ensure that the given `f64` is a
/// non-negative integer.
pub fn factorial(mut x: f64) -> f64 {
    debug_assert!(x >= 0.0);
    x = x.floor();
    let mut result = 1f64;
    while x >= 1. && result != f64::INFINITY {
        result *= x;
        x -= 1.;
    }
    result
}
