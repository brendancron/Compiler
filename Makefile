RUST_DIR := rust_comp

TEST_TARGETS := rust cronyx
REQUESTED := $(filter $(TEST_TARGETS),$(MAKECMDGOALS))
RUN := $(if $(REQUESTED),$(REQUESTED),$(TEST_TARGETS))

OUT := out

.PHONY: all rust test test-rust test-cronyx run clean

all: rust

rust:
	cargo build --release --manifest-path $(RUST_DIR)/Cargo.toml

test: $(addprefix test-,$(RUN))

test-rust: rust
	cargo test --manifest-path $(RUST_DIR)/Cargo.toml

test-cronyx:
	@echo "cronyx tests TBD"

run: rust
	cargo run --manifest-path $(RUST_DIR)/Cargo.toml -- --out out < $(FILE)

clean:
	cargo clean --manifest-path $(RUST_DIR)/Cargo.toml
	rm -rf $(OUT)
