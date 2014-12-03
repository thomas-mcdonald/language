ML = main.ml check.ml dict.ml lexer.ml parser.ml keiko.ml tree.ml print.ml gen.ml

all: language

LANGUAGE = keiko.cmo print.cmo tree.cmo dict.cmo lexer.cmo parser.cmo check.cmo gen.cmo main.cmo

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

main.cmo : tree.cmo print.cmo parser.cmi lexer.cmo gen.cmo check.cmo
main.cmx : tree.cmx print.cmx parser.cmx lexer.cmx gen.cmx check.cmx
check.cmo : tree.cmo dict.cmi
check.cmx : tree.cmx dict.cmx
dict.cmo : dict.cmi
dict.cmx : dict.cmi
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
gen.cmo : tree.cmo
gen.cmx : tree.cmx
