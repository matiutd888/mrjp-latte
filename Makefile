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

latc_x86: src/*.hs Grammar
#		ghc -Wall -Wno-unused-do-bind -Wno-unused-imports -isrc/Grammar/ -isrc/ -outputdir buildCompiler -o latc_x86 src/Main.hs
		cabal build
		ln -sf $$(cabal exec which latc_x86) latc_x86

clean:
	rm -rf buildCompiler latc_x86
	rm -rf buildGrammar
	rm -rf mn418323.tgz
	cabal clean
.PHONY:
	clean
	Grammar

create_archieve:
	rm -rf mn418323.tgz
	tar cf mn418323.tgz --exclude='src/Grammar' src lib Makefile Latte.cf
	mkdir -p testing-archieve
	rm -rf testing-archieve/*
	cp mn418323.tgz testing-archieve/
