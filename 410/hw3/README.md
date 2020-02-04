## KWIndex

A small Keyword Index.

Example

```rust
let index = kwindex::KWIndex::new()
    .extend_from_text("Hello world.");
assert_eq!(2, index.len());
assert_eq!(1, index.count_matches("world"));
```

### Building and running

```sh
# To build
cargo build
```

### Testing

```sh
cargo test
```
