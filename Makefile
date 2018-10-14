#!/usr/bin/make
SHELL=/bin/sh
#CABAL_CONFIG ?= ${HOME}/.cabal-8.4.2/config
GHC_VERSION ?= 8.4.2
CABALOPTS ?=
#CABALOPTS ?= --with-compiler ghc-$(GHC_VERSION)
#CABALOPTS ?= --config-file=$(CABAL_CONFIG) --with-compiler ghc-$(GHC_VERSION)

CABAL=cabal

unit : build test

all : configure build test dependencegraph document hscolor

all_with_install : all install


install_dependencies :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) install HUnit

configure :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-configure --enable-tests --enable-documentation

force-configure :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-configure --enable-tests --force-reinstall --upgrade-dependencies --reinstall --only-dependencies

#--enable-shared --enable-executable-dynamic


build : 
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-build -j4 --enable-documentation 

build-llvm:
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-build -j4 --ghc-options="-fllvm"

test-llvm:
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-test -j4 --ghc-options="-fllvm"

interpreter :
	cabal $(CABALOPTS) $(CABALFLAGS) new-configure -flibrary-only
	cabal $(CABALOPTS) $(CABALFLAGS) new-repl --ghc-option="-dynamic" --ghc-option="-fobject-code"

test : 
	cabal $(CABALOPTS) $(CABALFLAGS) new-test -j4 --ghc-options="+RTS -M786M -RTS"
	@grep -h Counts dist-newstyle/build/*/*/*/*/unit-tests/test/*.log

dependencegraph :
	mkdir -p dist-newstyle/doc/html/cifl-math-library
	dependencies/draw_graph.sh Tools
	dependencies/draw_graph.sh Graph
	dependencies/draw_graph.sh Matrix
	dependencies/draw_graph.sh Number
	mv dependencies-*.ps dependencies-*.pdf dist-newstyle/doc/html/cifl-math-library/

document : 
	cabal $(CABALOPTS) $(CABALFLAGS) new-haddock --offline --enable-documentation --haddock-options="--title=cifl-math-library --html --haddock-hyperlink-source --source-base=https://github.com/esapulkkinen/cifl-math-library/blob/master/ --source-module=https://github.com/esapulkkinen/cifl-math-library/blob/master/%M.html --source-entity=https://github.com/esapulkkinen/cifl-math-library/blob/master/%M.html#%N --no-print-missing-docs" --ghc-options="+RTS -M786M -RTS" --haddock-internal  > dist-newstyle/haddock-output

latex-document :
	cabal $(CABALOPTS) $(CABALFLAGS) new-haddock --enable-documentation --haddock-options="--title=cifl-math-library --latex" --haddock-all

publish_document : dependencegraph document hscolor
	cp -r dist-newstyle/build/*/ghc-*/cifl-math-library-*/doc/html/cifl-math-library/* docs/
	sed -i 's+COPYRIGHT">COPYRIGHT+COPYRIGHT" rel="license">COPYRIGHT+g' docs/index.html
	sed -i 's+<head><meta+<head><link href="https://github.com/esapulkkinen/cifl-math-library/blob/master/COPYRIGHT" rel="license"/><link href="https://www.gnu.org/licenses/lgpl-3.0.txt" rel="license"/><link href="https://github.com/esapulkkinen/cifl-math-library/blob/master/AUTHORS" rel="author"/><meta+g' docs/*.html

hscolor :


#cabal $(CABALOPTS) hscolour --all 

install :
	cabal $(CABALOPTS) $(CABALFLAGS) new-install -j2 --enable-documentation 

clean :
	cabal $(CABALOPTS) $(CABALFLAGS) new-clean
	rm -f dependencies-*.ps dependencies-*.pdf
