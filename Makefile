MAKEFLAGS = -rR
NAME = sqfs
CARGO = cargo
#CARGO_BUILD = $(CARGO) build
CARGO_BUILD = rustup run nightly cargo clippy

ifdef V
E=@#
Q=
else
E=@echo
Q=@
endif

.PHONY: run release

run: target/debug/$(NAME)
	$Q$< list image.sqfs /etc
	$Q$< sb image.sqfs

release: target/release/$(NAME)
	$Q$< list image.sqfs
	$Q$< sb image.sqfs

target/debug/$(NAME): $(wildcard src/*.rs)
	$E $(CARGO_BUILD)
	$Q$(CARGO_BUILD)

target/release/$(NAME): $(wildcard src/*.rs)
	$E $(CARGO) build --release
	$Q$(CARGO) build --release

%:
	$E $(CARGO) $@
	$Q$(CARGO) $@
