COMMON=src/ast.sml target/mips.sml src/temp.sml src/ir.sml  target/translate.sml src/graph.sml src/treeir.sml


src/%.lex.sml: src/%.lex
	mllex $<

src/%.grm.sml: src/%.grm
	mlyacc $<


all: ec

.PHONY: all clean test

clean:
	rm -f *.lex.sml *.out *.asm
	rm -f *.grm.sml *.grm.desc *.grm.sig ec


ec: ec.sml ec.mlb expr.grm.sml expr.lex.sml ${COMMON} translate.sml
	mlton ec.mlb

test: all
	./ec test.inp

