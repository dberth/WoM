#Copyright (C) 2014 Denis Berthod

OCAML=ocaml -w -3

.PHONY: all, build, test, clean, distclean, install, configure

all:
	$(OCAML) setup.ml -build
	$(OCAML) setup.ml -test

build:
	$(OCAML) setup.ml -build

test:
	$(OCAML) setup.ml -test

clean:
	$(OCAML) setup.ml -clean

distclean:
	$(OCAML) setup.ml -distclean

install:
	$(OCAML) setup.ml -install

configure:
	$(OCAML) setup.ml -configure --enable-tests
