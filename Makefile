COMMON=src/ast.sml target/mips.sml src/temp.sml target/translate.sml src/graph.sml 

src/%.lex.sml: src/%.lex
	mllex $<

src/%.grm.sml: src/%.grm
	mlyacc $<

all: ec

.PHONY: all clean test

clean:
	rm -rf src/*.lex.sml *.out *.asm
	rm -rf src/*.grm.sml src/*.grm.desc src/*.grm.sig ec

ec: ec.sml ec.mlb src/expr.grm.sml src/expr.lex.sml ${COMMON}
	mlton ec.mlb

test: all
	./ec test.inp > test.out

