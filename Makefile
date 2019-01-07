MAKEFLAGS = -rR
NAME = sqfs
STRIP = strip
CARGO = cargo

ifdef V
E=@#
Q=
else
E=@echo
Q=@
endif

.PHONY: run release static

run: target/debug/$(NAME) | build
	$Q$< plan test.yml
	$Q$< sb test.sqfs
	@#$Q$< ls -r test.sqfs

release: target/release/$(NAME)
	$Q$< plan test.yml
	$Q$< sb test.sqfs
	@#$Q$< ls -r test.sqfs

static: target/x86_64-unknown-linux-musl/release/$(NAME)
	$E $(STRIP) $@
	$Q$(STRIP) -s --strip-unneeded -o '$@' '$<'

target/debug/$(NAME):
	$Q:

target/release/$(NAME):
	$E $(CARGO) build --release
	$Q$(CARGO) build --release

target/x86_64-unknown-linux-musl/release/$(NAME):
	$E $(CARGO) build --target x86_64-unknown-linux-musl --release
	$Q$(CARGO) build --target x86_64-unknown-linux-musl --release

%:
	$E $(CARGO) $@
	$Q$(CARGO) $@
