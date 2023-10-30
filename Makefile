BNFC_COMMAND=bnfc-linux


all: grammar latc_llvm

grammar: Latte.cf
	mkdir -p buildGrammar
	mkdir -p src/grammar
	rm -rf src/grammar/*
	$(BNFC_COMMAND) -m --functor -o buildGrammar Latte.cf  
	make -C buildGrammar
	cp buildGrammar/*.hs src/grammar/
	rm -rf src/grammar/TestLatte.hs

lib: lib-sources/runtime.ll
	llvm-asm lib-sources/runtime.ll -o lib/runtime.bc


insc_llvm: src/*.hs src/llvm/*.hs
	ghc -Wall -Wno-unused-do-bind -Wno-unused-imports -isrc/grammar/ -isrc/ -isrc/llvm -outputdir buildLLVM -o insc_llvm src/llvm/MainLLVM.hs


clean:
	rm -rf buildJVM insc_jvm
	rm -rf buildLLVM insc_llvm
	rm -rf buildGrammar
	rm -rf mn418323.tgz
.PHONY:
	clean

create_archieve:
	rm -rf mn418323.tgz
	tar cf mn418323.tgz --exclude='src/grammar' src lib Makefile Instant.cf README
	mkdir -p testing-archieve
	rm -rf testing-archieve/*
	cp mn418323.tgz testing-archieve/
