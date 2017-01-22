all: generate link compile run

generate:
	csi -s compile.scm > body.ll

link:
	cat prelude.ll primitives/*.ll body.ll > output.ll

compile:
	llc output.ll -o output.s -O1 -x86-asm-syntax=intel
	gcc -m64 -masm=intel -o output print_ptr.c output.s entry.s runner.c

run:
	./output

tests:
	csi -s test/runner.scm
