#Copyright (C) 2014 Denis Berthod

all:
	ocaml setup.ml -build
	ocaml setup.ml -test

build:
	ocaml setup.ml -build

test:
	ocaml setup.ml -test

clean:
	ocaml setup.ml -clean

distclean:
	ocaml setup.ml -distclean

install:
	ocaml setup.ml -install
