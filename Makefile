BNFC_COMMAND=bnfc-linux

all: grammar latc


grammar: src/grammar/AbsLatte.hs
	@echo "Alias for 'src/grammar/AbsLatte.hs' target."


src/grammar/AbsLatte.hs: Latte.cf 
	mkdir -p buildGrammar
	mkdir -p src/grammar
	rm -rf src/grammar/*
	$(BNFC_COMMAND) -m --functor -o buildGrammar Latte.cf  
	make -C buildGrammar
	cp buildGrammar/*.hs src/grammar/
	rm -rf src/grammar/TestLatte.hs

latc: src/*.hs grammar
		ghc -Wall -Wno-unused-do-bind -Wno-unused-imports -isrc/grammar/ -isrc/ -outputdir buildCompiler -o latc src/Main.hs

clean:
	rm -rf buildCompiler latc
	rm -rf buildGrammar
	rm -rf mn418323.tgz
.PHONY:
	clean
	grammar

create_archieve:
	rm -rf mn418323.tgz
	tar cf mn418323.tgz --exclude='src/grammar' src lib Makefile Latte.cf README
	mkdir -p testing-archieve
	rm -rf testing-archieve/*
	cp mn418323.tgz testing-archieve/
