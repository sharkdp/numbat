/// Calculates the factorial of (the floor of) the given `f64`.
///
/// It is the caller's responsibility to ensure that the given `f64` is a
/// non-negative integer.
pub fn factorial(mut x: f64, order: u16) -> f64 {
    debug_assert!(x >= 0.0);
    debug_assert!(order >= 1);
    x = x.floor();
    let mut result = 1f64;
    while x >= 1. && result != f64::INFINITY {
        result *= x;
        x -= order as f64;
    }
    result
}
