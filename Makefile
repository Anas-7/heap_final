
UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
endif

test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

test/%.run: test/%.s runtime/start.rs
	nasm -f $(ARCH) test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc -g -L runtime/ runtime/start.rs -o test/$*.run

test/%.alls: test/%.s runtime/start.rs
	rustc --emit asm -L runtime/ runtime/start.rs -o test/$*.alls

.PRECIOUS: test/%.s
