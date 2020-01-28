use std::convert::TryInto;
use toy_rsa_lib::{gcd, lcm, modexp, modinverse, rsa_prime};

/// Fixed RSA encryption exponent.
pub const EXP: u64 = 65_537;

/// Compute Carmichael's totient function.
/// λ(pq) = lcm(p − 1, q − 1)
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
    (p, q)
}

/// Encrypt the plaintext `msg` using the RSA public `key`
/// and return the ciphertext.
pub fn encrypt(key: u64, msg: u32) -> u64 {
    modexp(msg as u64, EXP, key)
}

/// Decrypt the ciphertext `msg` using the RSA private `key`
/// and return the resulting plaintext.
pub fn decrypt(key: (u32, u32), msg: u64) -> u32 {
    let p = key.0 as u64;
    let q = key.1 as u64;
    let d = modinverse(EXP, lambda(p, q));
    modexp(msg, d, p * q).try_into().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lambda() {
        // From https://en.wikipedia.org/wiki/RSA_%28cryptosystem%29#Example
        let (p, q): (u64, u64) = (61, 53);
        assert_eq!(780, lambda(p, q));
    }
    #[test]
    fn test_encrypt() {
        let p: u64 = 0xed23e6cd;
        let q: u64 = 0xf050a04d;
        let msg: u32 = 12345;

        assert_eq!(0x164e44b86776d497, encrypt(p * q, msg))
    }
    #[test]
    fn test_decrypt() {
        let p: u32 = 0xed23e6cd;
        let q: u32 = 0xf050a04d;
        let encrypted_msg: u64 = 0x164e44b86776d497;
        assert_eq!(12345, decrypt((p, q), encrypted_msg));
    }

    #[test]
    fn test_genkey() {
        let (p, q) = genkey();
        let p = p as u64;
        let q = q as u64;
        assert!(lambda(p, q) > EXP);
        assert!(gcd(EXP, lambda(p, q)) == 1)
    }

    #[test]
    fn test_encrypt_decrypt() {
        let (p, q) = genkey();
        let key: u64 = p as u64 * q as u64;
        let msg: u64 = 54321;
        assert_eq!(
            msg,
            decrypt((p, q), encrypt(key, msg.try_into().unwrap())) as u64
        )
    }
}
