BNFC_COMMAND=$(or $(BNFC_HOME),bnfc-linux)

all: Grammar latc_x86


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

lib/runtime.o: lib-sources/runtime.h lib-sources/runtime.c
	gcc -c lib-sources/runtime.c -o lib/runtime.o
	

latc_x86: src/*.hs Grammar lib/runtime.o
#		ghc -Wall -Wno-unused-do-bind -Wno-unused-imports -isrc/Grammar/ -isrc/ -outputdir buildCompiler -o latc_x86 src/Main.hs
		cabal build
		ln -sf $$(cabal exec which latc_x86) latc_x86

clean:
	rm -rf lib/runtime.o
	rm -rf buildCompiler latc_x86
	rm -rf buildGrammar
	rm -rf mn418323.tgz
	cabal clean
.PHONY:
	clean
	Grammar


create_archieve:
	rm -rf mn418323.tgz
	tar cf mn418323.tgz --exclude='src/Grammar' src lib lib-sources Makefile Latte.cf
	mkdir -p testing-archieve
	rm -rf testing-archieve/*
	cp mn418323.tgz testing-archieve/
