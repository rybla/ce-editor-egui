build:
  cargo build

run:
  RUST_BACKTRACE=1 cargo run

test:
  cargo test -- --nocapture
