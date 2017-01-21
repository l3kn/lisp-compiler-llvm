all: generate link compile run

generate:
	csi -s compile.scm > body.ll

link:
	cat primitives/*.ll body.ll > output.ll

compile:
	llc output.ll -o output.s

run:
	gcc -m64 -masm=intel -o output output.s entry.s runner.c
	echo "Program output:"
	./output
