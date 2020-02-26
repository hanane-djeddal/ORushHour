export OCAMLFIND_CONF=/Vrac/3I008-1819/opam-js_of_ocaml/4.06.1/lib/findlib.conf

GEN_JS_API_DIR=/Vrac/3I008-1819/opam-js_of_ocaml/4.06.1/lib/gen_js_api
JS_OF_OCAML_DIR=/Vrac/3I008-1819/opam-js_of_ocaml/4.06.1/bin/
JS_OF_OCAML_LIBDIR=/Vrac/3I008-1819/opam-js_of_ocaml/4.06.1/lib/js_of_ocaml-compiler/

CAMLC=ocamlc
COMPFLAGS=-I $(GEN_JS_API_DIR)

GEN_JS_API=ocamlrun $(GEN_JS_API_DIR)/gen_js_api
OJSFLAGS=gen_js_api.cma

JS_OF_OCAML=$(JS_OF_OCAML_DIR)/js_of_ocaml
JS_OF_OCAML_FLAGS=--no-runtime $(JS_OF_OCAML_LIBDIR)/runtime.js $(GEN_JS_API_DIR)/ojs_runtime.js

FILES= port.cmo moves.cmo solver.cmo
ENGINE_OBJS=orush.cmo
JS_DEPS=dom.cmi
JS_OBJS=dom.cmo  $(FILES) $(ENGINE_OBJS) 



all : ocaml-orush.js 

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<


dom.ml: dom.mli
	$(GEN_JS_API) dom.mli

ocaml-orush.byte: $(JS_DEPS) $(JS_OBJS)
	$(CAMLC) $(COMPFLAGS) -no-check-prims -o $@ $(OJSFLAGS) $(JS_OBJS)

ocaml-orush.js: ocaml-orush.byte
	$(JS_OF_OCAML) $(JS_OF_OCAML_FLAGS) $^

include .depend
clean:
	rm -rf *.cmo *.cmi *.cmx *.cma *.o *.byte
	rm -f dom.ml ocaml-orush.js

