#!/usr/bin/make
SHELL=/bin/sh
#CABAL_CONFIG ?= ${HOME}/.cabal-8.4.2/config
GHC_VERSION ?= 8.4.2
CABALOPTS ?=
#CABALOPTS ?= --with-compiler ghc-$(GHC_VERSION)
#CABALOPTS ?= --config-file=$(CABAL_CONFIG) --with-compiler ghc-$(GHC_VERSION)

CABAL=cabal
STACK=stack

unit : build test

.PHONY: unit all build test all_with_install install_dependencies configure force-configure document hscolor dependencegraph build-llvm test-llvm interpreter latex-document publish_document install clean


all : configure build test dependencegraph document hscolor

all_with_install : all install


install_dependencies :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) install HUnit

configure : configure-stack

configure-stack :

configure-cabal :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-configure --enable-tests

force-configure :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-configure --enable-tests --force-reinstall --upgrade-dependencies --reinstall --only-dependencies

#--enable-shared --enable-executable-dynamic

build : build-stack

build-cabal : 
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-build -j4 

build-stack :
	$(STACK) build -j 3

build-stack-llvm :
	$(STACK) build --ghc-options="-fllvm"

build-llvm:
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-build -j4 --ghc-options="-fllvm"

test-stack-llvm:
	$(STACK) test --ghc-options="-fllvm"

test-llvm:
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-test -j4 --ghc-options="-fllvm"

interpreter :
	cabal $(CABALOPTS) $(CABALFLAGS) new-configure -flibrary-only
	cabal $(CABALOPTS) $(CABALFLAGS) new-repl --ghc-option="-dynamic" --ghc-option="-fobject-code"

test : test-stack

test-stack :
	$(STACK) test


test-cabal : 
	cabal $(CABALOPTS) $(CABALFLAGS) new-test -j4 --ghc-options="+RTS -M786M -RTS"
	@grep -h Counts dist-newstyle/build/*/*/*/*/unit-tests/test/*.log

externaldepgraph :
	$(STACK) dot --external | dot -Tpdf > external-deps.pdf
	mv external-deps.pdf dist-newstyle/doc/html/cifl-math-library/

dependencegraph :
	mkdir -p dist-newstyle/doc/html/cifl-math-library
	dependencies/draw_graph.sh Tools
	dependencies/draw_graph.sh Graph
	dependencies/draw_graph.sh Matrix
	dependencies/draw_graph.sh Number
	mv dependencies-*.pdf dist-newstyle/doc/html/cifl-math-library/

document : document-cabal

document-stack :
	$(STACK) haddock --haddock-arguments "--title=cifl-math-library --html --source-base=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/ --source-module=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F --source-entity=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F#%N --no-print-missing-docs" --ghc-options="+RTS -M786M -RTS" --no-haddock-deps

document-cabal : 
	cabal $(CABALOPTS) $(CABALFLAGS) new-haddock --offline --enable-documentation --haddock-options="--title=cifl-math-library --html --source-base=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/ --source-module=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F --source-entity=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F#%N --no-print-missing-docs" --ghc-options="+RTS -M786M -RTS"  > dist-newstyle/haddock-output

latex-document :
	cabal $(CABALOPTS) $(CABALFLAGS) new-haddock --enable-documentation --haddock-options="--title=cifl-math-library --latex" --haddock-all

publish_document : dependencegraph externaldepgraph document hscolor
	cp -r dist-newstyle/build/*/ghc-*/cifl-math-library-*/doc/html/cifl-math-library/* docs/
	cp dist-newstyle/doc/html/cifl-math-library/*.pdf docs/
	sed -i 's+COPYRIGHT">COPYRIGHT+COPYRIGHT" rel="license">COPYRIGHT+g' docs/index.html
	sed -i "s+<head><meta+<head profile=\"http://dublincore.org/specifications/dublin-core/dc-html/2008-08-04/\" prefix=\"og: http://ogp.me/ns#\">$$(cat metadata.txt | tr '\n' ' ')<meta+g" docs/*.html

hscolor :


#cabal $(CABALOPTS) hscolour --all 

install :
	cabal $(CABALOPTS) $(CABALFLAGS) new-install -j2 --enable-documentation 

clean :
	stack clean
	rm -f dependencies-*.ps dependencies-*.pdf

old-clean :
	cabal $(CABALOPTS) $(CABALFLAGS) new-clean
	rm -f dependencies-*.ps dependencies-*.pdf
