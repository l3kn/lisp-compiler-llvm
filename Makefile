combine-chicken:
	cat compatibility.scm syntax.scm helper.scm llvm.scm preprocessing/*.scm reader.scm compile.scm > full-chicken.scm

combine:
	cat syntax.scm helper.scm llvm.scm preprocessing/*.scm reader.scm compile.scm > full.scm

bootstrap:
	make combine-chicken
	make combine
	make build-chicken infile=full.scm name=compiler-body
	make build-chicken infile=programs/stdlib.scm name=stdlib
	make compile name=compiler

build-chicken:
	csi -s full-chicken.scm < $(infile) > $(name).ll

build:
	./compiler < $(infile) > $(name).ll

compile:
	cat stdlib-ll/*.ll stdlib.ll $(name)-body.ll > $(name).ll
	llc $(name).ll -o $(name).s -O3
	gcc -m64 -o $(name) $(name).s
