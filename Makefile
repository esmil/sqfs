MAKEFLAGS = -rR
NAME = sqfs
STRIP = strip
CARGO = cargo
#CARGO_BUILD = $(CARGO) build
CARGO_BUILD = rustup run nightly cargo clippy

sources = $(wildcard src/*.rs)

ifdef V
E=@#
Q=
else
E=@echo
Q=@
endif

.PHONY: run release

run: target/debug/$(NAME)
	$Q$< plan test.yml
	$Q$< sb test.sqfs
	$Q$< ls -r test.sqfs

release: target/release/$(NAME)
	$Q$< list image.sqfs
	$Q$< sb image.sqfs

static: target/x86_64-unknown-linux-musl/release/$(NAME)
	$E $(STRIP) $@
	$Q$(STRIP) -s --strip-unneeded -o '$@' '$<'

target/debug/$(NAME): $(sources)
	$E $(CARGO_BUILD)
	$Q$(CARGO_BUILD)

target/release/$(NAME): $(sources)
	$E $(CARGO) build --release
	$Q$(CARGO) build --release

target/x86_64-unknown-linux-musl/release/$(NAME): $(sources)
	$E $(CARGO) build --target x86_64-unknown-linux-musl --release
	$Q$(CARGO) build --target x86_64-unknown-linux-musl --release

%:
	$E $(CARGO) $@
	$Q$(CARGO) $@
