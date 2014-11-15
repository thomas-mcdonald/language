ML = main.ml lexer.ml parser.ml keiko.ml tree.ml

all: language

LANGUAGE = keiko.cmo parser.cmo lexer.cmo tree.cmo main.cmo

language: $(LANGUAGE)
	ocamlc -o language $(LANGUAGE)

parser.mli parser.ml: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean: force
	rm -f language
	rm -f *.cma *.cmo *.cmi
	rm -f parser.mli parser.ml lexer.ml parser.output

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

%.cmi : %.mli
	ocamlc $(MLFLAGS) -c $<

%.cmo : %.ml
	ocamlc $(MLFLAGS) -c $<

force: 

###

main.cmo : tree.cmo parser.cmi lexer.cmo
main.cmx : tree.cmx parser.cmx lexer.cmx
lexer.cmo : tree.cmo parser.cmi keiko.cmi
lexer.cmx : tree.cmx parser.cmx keiko.cmx
parser.cmo : tree.cmo parser.cmi
parser.cmx : tree.cmx parser.cmi
keiko.cmo : keiko.cmi
keiko.cmx : keiko.cmi
tree.cmo :
tree.cmx :
