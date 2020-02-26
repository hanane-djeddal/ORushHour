CAMLC=ocamlc
COMPFLAGS= -w -A
FILES= port.cmo moves.cmo solver.cmo main.cmo
FILEEXE=main.exe

all: $(FILES)
	$(CAMLC) -o $(FILEEXE) $(FILES)
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<
.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<
include .depend
