use std::convert::{TryFrom, TryInto};
use toy_rsa_lib::{gcd, lcm, modexp, modinverse, rsa_prime};

/// Fixed RSA encryption exponent.
pub const EXP: u64 = 65_537;

fn lambda(p: u64, q: u64) -> u64 {
    lcm(p - 1, q - 1)
}

/// Generate a pair of primes in the range `2**30..2**31`
/// suitable for RSA encryption with exponent
/// `EXP`. Warning: this routine has unbounded runtime; it
/// works by generate-and-test, generating pairs of primes
/// `p` `q` and testing that they satisfy `λ(pq) <= EXP` and
/// that `λ(pq)` has no common factors with `EXP`.
pub fn genkey() -> (u32, u32) {
    let mut p = rsa_prime();
    let mut q = rsa_prime();
    let mut totient = lambda(p as u64, q as u64);

    while totient <= EXP && gcd(EXP, totient) != 1 {
        p = rsa_prime();
        q = rsa_prime();
        totient = lambda(p as u64, q as u64);
    }
    return (p, q);
}

/// Encrypt the plaintext `msg` using the RSA public `key`
/// and return the ciphertext.
pub fn encrypt(key: u64, msg: u32) -> u64 {
    modexp(msg as u64, EXP, key)
}

/// Decrypt the cipertext `msg` using the RSA private `key`
/// and return the resulting plaintext.
pub fn decrypt(key: (u32, u32), msg: u64) -> u32 {
    let d = modinverse(lambda(key.0 as u64, key.1 as u64), EXP);
    modexp(msg, d, u64::from(key.0) * u64::from(key.1)) as u32
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
