fn error() -> ! {
    eprintln!("modexp: usage: modexp <x> <y> <m>\n\tNote: <x>, <y>, and <m> must be positive integers less than 4,294,967,295");
    std::process::exit(1);
}

fn modexp(x: u64, y: u64, m: u64) -> u64 {
    if x == 0 {
        return 0;
    } else if y == 0 {
        return 1;
    }
    let mut z = modexp(x, y / 2, m);
    z = (z * z) % m;
    if y % 2 == 1 {
        z = (z * x) % m;
    }
    // Return z implicitly
    z
}

fn main() {
    let x: u64 = match std::env::args().nth(1) {
        Some(x) => x.parse().expect("Expected x to be a number"),
        _ => error(),
    };

    let y: u64 = match std::env::args().nth(2) {
        Some(y) => y.parse().expect("Expected y to be a number"),
        _ => error(),
    };

    let m: u64 = match std::env::args().nth(2) {
        Some(m) => m.parse().expect("Expected m to be a number"),
        _ => error(),
    };

    let max = u64::from(u32::max_value());
    if x > max || y > max || m > max {
        error();
    }

    let expmod = modexp(x, y, m);

    println!("{}", expmod);
}

#[test]
fn test_modexp_simple_x() {
    assert_eq!(modexp(0, 20, 17), 0);
}

#[test]
fn test_modexp_simple_y() {
    assert_eq!(modexp(2, 0, 17), 1);
}

#[test]
fn test_modexp_complex() {
    assert_eq!(modexp(2, 20, 17), 16);
}
