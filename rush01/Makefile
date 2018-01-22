RESULT = tama
SOURCES = \
		  tama.ml \
		  window.ml \
		  main.ml \

PACKS = tsdl tsdl_ttf
THREADS = "yes"
OCAMLMAKEFILE = OCamlMakefile

all: .install byte-code

.install:
	opam install tsdl
	opam install tsdl-ttf
	touch .install
include $(OCAMLMAKEFILE)
