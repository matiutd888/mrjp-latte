BNFC_COMMAND=$(or $(BNFC_HOME),bnfc-linux)

all: Grammar latc


Grammar: src/Grammar/AbsLatte.hs
	@echo "Alias for 'src/Grammar/AbsLatte.hs' target."


src/Grammar/AbsLatte.hs: Latte.cf 
	mkdir -p buildGrammar
	mkdir -p src/Grammar
	rm -rf src/Grammar/*
	$(BNFC_COMMAND) -m --functor -p Grammar -o buildGrammar Latte.cf
	make -C buildGrammar
	cp buildGrammar/Grammar/*.hs src/Grammar/
	rm -rf src/Grammar/TestLatte.hs

lib/runtime.bc: lib-sources/runtime.ll
	llvm-asm lib-sources/runtime.ll -o lib/runtime.bc	

latc: src/*.hs Grammar
		ghc -Wall -Wno-unused-do-bind -Wno-unused-imports -isrc/Grammar/ -isrc/ -outputdir buildCompiler -o latc src/Main.hs

clean:
	rm -rf buildCompiler latc
	rm -rf buildGrammar
	rm -rf mn418323.tgz
.PHONY:
	clean
	Grammar

create_archieve:
	rm -rf mn418323.tgz
	tar cf mn418323.tgz --exclude='src/Grammar' src lib Makefile Latte.cf
	mkdir -p testing-archieve
	rm -rf testing-archieve/*
	cp mn418323.tgz testing-archieve/
