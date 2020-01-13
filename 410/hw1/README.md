This assignment went well. I started by adding error and modexp functions and then building out the main function to leverage them when ran.

The only programming issue I ran into was dealing with the command line argument parsing. I thought it was returning a Result instead of an Option and had a hard time figuring out why it wasn't working. As well, when running `cargo fmt` and `cargo clippy` I found out the the rust toolchain was incorrectly installed and had the run `rustup toolchain remove stable && rustup toolchain add stable` to fix the issue.

I tested the modexp function with three tests, testing that the value of x equal to 0 returned 0, the value of y equal to 0 returned 1 and the values given in the assignment definition (2\*\*20 mod 17) were equal to the result given in the assignment definition (16). All tests pass after running `cargo test`.
