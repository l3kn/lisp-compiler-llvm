all: generate body

body: link compile run

generate:
	csi -s compile.scm > body.ll

link:
	cat stdlib-ll/*.ll stdlib.ll body.ll > output.ll

compile:
	llc output.ll -o output.s -O1
	# -x86-asm-syntax=intel
	gcc -m64 -o output output.s
	# -masm=intel 

run:
	./output

tests:
	csi -s test/runner.scm

clean:
	rm body.ll output.ll output.s output
