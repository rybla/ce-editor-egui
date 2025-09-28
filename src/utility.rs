/// The function's purpose is to calculate `x mod n` where `x` is a signed 8-bit
/// integer and `n` is a `usize`. A critical part of the logic is to ensure the
/// result is always non-negative, which aligns with the mathematical definition
/// of a modulus, as opposed to a simple remainder operation which can produce
/// negative results.
///
/// To accomplish this, the function first performs type casting. The input `x` (`i8`)
/// and `n` (`usize`) are both cast to `isize`. This provides a common, signed,
/// architecture-width integer type to perform the calculation without type conflicts.
/// The core of the implementation uses the `rem_euclid` method, which calculates the
/// Euclidean remainder. This method is specifically designed to produce a result that
/// is always non-negative, which is exactly the behavior required for a true modulus.
/// For instance, `(-5).rem_euclid(3)` correctly yields `1`.
///
/// After the non-negative `isize` result is obtained from `rem_euclid`, it is
/// safely cast to `usize` to match the function's specified return type. This
/// entire process is contained in a single, efficient line of code.
pub fn modulus_i8_to_usize(x: i8, n: usize) -> usize {
    (x as isize).rem_euclid(n as isize) as usize
}

pub fn split_vec_at_index<A>(xs: Vec<A>, i: usize) -> (Vec<A>, Vec<A>) {
    let mut left = xs;
    let right = left.split_off(i);
    (left, right)
}

pub fn get_owned_element_at_index<A>(xs: Vec<A>, i: usize) -> Option<A> {
    xs.into_iter().nth(i)
}
