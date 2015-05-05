ML = main.ml check.ml dict.ml lexer.ml parser.ml keiko.ml peephole.ml tree.ml print.ml gen.ml

.PHONY: depend test clean cloc

all: language

LANGUAGE = config.cmo keiko.cmo print.cmo tree.cmo dict.cmo peephole.cmo lexer.cmo parser.cmo check.cmo gen.cmo main.cmo

language: $(LANGUAGE)
	ocamlc -o language str.cma $(LANGUAGE)

parser.mli parser.ml: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	rm -f language
	rm -f *.cma *.cmo *.cmi
	rm -f parser.ml lexer.ml parser.output

%.cmi : %.mli
	ocamlc $(MLFLAGS) -c $<

%.cmo : %.ml
	ocamlc $(MLFLAGS) -c $<

depend : $(ML)
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

test:
	ruby test_runner.rb

cloc:
	cloc --exclude-dir=keiko .


###

main.cmo : tree.cmo print.cmo parser.cmi lexer.cmo gen.cmo config.cmo \
    check.cmo
main.cmx : tree.cmx print.cmx parser.cmx lexer.cmx gen.cmx config.cmx \
    check.cmx
check.cmo : tree.cmo keiko.cmo dict.cmi
check.cmx : tree.cmx keiko.cmx dict.cmx
dict.cmo : dict.cmi
dict.cmx : dict.cmi
lexer.cmo : tree.cmo parser.cmi keiko.cmo
lexer.cmx : tree.cmx parser.cmx keiko.cmx
parser.cmo : tree.cmo keiko.cmo dict.cmi parser.cmi
parser.cmx : tree.cmx keiko.cmx dict.cmx parser.cmi
keiko.cmo :
keiko.cmx :
peephole.cmo : print.cmo keiko.cmo
peephole.cmx : print.cmx keiko.cmx
tree.cmo : keiko.cmo dict.cmi
tree.cmx : keiko.cmx dict.cmx
print.cmo : tree.cmo
print.cmx : tree.cmx
gen.cmo : tree.cmo peephole.cmo keiko.cmo dict.cmi
gen.cmx : tree.cmx peephole.cmx keiko.cmx dict.cmx
