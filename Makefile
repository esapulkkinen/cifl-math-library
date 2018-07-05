#!/usr/bin/make
SHELL=/bin/sh
CABAL_CONFIG ?= ${HOME}/.cabal-8.4.2/config
CABALOPTS ?= --config-file=$(CABAL_CONFIG)

CABAL=cabal

unit : configure build test

all : configure build test dependencegraph document hscolor

all_with_install : all install


install_dependencies :
	$(CABAL) $(CABALOPTS) install HUnit

configure :
	$(CABAL) $(CABALOPTS) configure --enable-tests
#--enable-shared --enable-executable-dynamic


build : 
	$(CABAL) $(CABALOPTS) build -j4 

build-llvm:
	$(CABAL) $(CABALOPTS) build -j4 --ghc-options="-fllvm"

interpreter :
	cabal $(CABALOPTS) configure -flibrary-only
	cabal $(CABALOPTS) repl --ghc-option="-fobject-code"

test : 
	cabal $(CABALOPTS) test -j4 --show-details=failures --ghc-options="+RTS -M786M -RTS"
	@grep -h Counts dist/test/*.log

dependencegraph :
	mkdir -p dist/doc/html/cifl-math-library
	dependencies/draw_graph.sh Tools
	dependencies/draw_graph.sh Graph
	dependencies/draw_graph.sh Matrix
	dependencies/draw_graph.sh Number
	mv dependencies-*.ps dependencies-*.pdf dist/doc/html/cifl-math-library/

document : 
	cabal $(CABALOPTS) haddock --haddock-options="--title=cifl-math-library" --ghc-options="+RTS -M786M -RTS" --verbose=0 --all --hyperlink-source > dist/haddock-output

publish_document : dependencegraph document hscolor
	cp -r dist/doc/html/cifl-math-library/* docs/
	sed -i 's+COPYRIGHT">COPYRIGHT+COPYRIGHT" rel="license">COPYRIGHT+g' docs/index.html
	sed -i 's+<head><meta+<head><link href="https://github.com/esapulkkinen/cifl-math-library/blob/master/COPYRIGHT" rel="license"/><link href="https://github.com/esapulkkinen/cifl-math-library/blob/master/AUTHORS" rel="author"/><meta+g' docs/*.html

hscolor :
	cabal $(CABALOPTS) hscolour --all 

install :
	cabal $(CABALOPTS) install -j2 --enable-documentation 

clean :
	cabal clean
	rm -f dependencies-*.ps dependencies-*.pdf
