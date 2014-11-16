ML = main.ml check.ml lexer.ml parser.ml keiko.ml tree.ml print.ml

all: language

LANGUAGE = keiko.cmo print.cmo tree.cmo lexer.cmo parser.cmo check.cmo main.cmo

language: $(LANGUAGE)
	ocamlc -o language str.cma $(LANGUAGE)

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

main.cmo : tree.cmo parser.cmi lexer.cmo check.cmo
main.cmx : tree.cmx parser.cmx lexer.cmx check.cmx
check.cmo : tree.cmo
check.cmx : tree.cmx
lexer.cmo : tree.cmo parser.cmi keiko.cmi
lexer.cmx : tree.cmx parser.cmx keiko.cmx
parser.cmo : tree.cmo parser.cmi
parser.cmx : tree.cmx parser.cmi
keiko.cmo : keiko.cmi
keiko.cmx : keiko.cmi
tree.cmo :
tree.cmx :
print.cmo : tree.cmo
print.cmx : tree.cmx
