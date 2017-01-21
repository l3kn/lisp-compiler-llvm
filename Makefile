all: generate link compile run

generate:
	csi -s compile.scm > body.ll

link:
	cat primitives/*.ll body.ll > output.ll

compile:
	llc output.ll -o output.s
	gcc -m64 -masm=intel -o output output.s entry.s runner.c

run:
	./output

tests:
	csi -s test/runner.scm
