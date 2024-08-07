#!/usr/bin/make
SHELL=/bin/sh
#CABAL_CONFIG ?= ${HOME}/.cabal-8.4.2/config
GHC_VERSION ?= 8.4.2
CABALOPTS ?= 
#CABALOPTS ?= --with-compiler ghc-$(GHC_VERSION)
#CABALOPTS ?= --config-file=$(CABAL_CONFIG) --with-compiler ghc-$(GHC_VERSION)

CABAL=cabal
STACK=stack

unit : test

.PHONY: unit all build test all_with_install install_dependencies configure force-configure document hscolor dependencegraph build-stack-llvm test-stack-llvm build-llvm test-llvm interpreter latex-document publish_document install clean


all : configure build test publish_document

all_with_install : all install

llvm : build-stack-llvm test-stack-llvm

install_dependencies :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) install HUnit

configure : configure-stack

configure-stack :

configure-cabal :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-configure --enable-tests

force-configure :
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-configure --enable-tests --force-reinstall --upgrade-dependencies --reinstall --only-dependencies

#--enable-shared --enable-executable-dynamic

watch :
	$(STACK) build --file-watch --fast --ghc-options "-j3 +RTS -A128m -n2m -qg -RTS"

build : build-stack

build-cabal : 
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-build -j4 

build-stack :
	$(STACK) build --fast cifl-math-library:lib

build-prof :
	$(STACK) build --fast cifl-math-library:lib --library-profiling --ghc-options "-rtsopts"

build-gui-prof :
	$(STACK) build --fast cifl-math-library:mathgui --executable-profiling 

build-stack-llvm :
	$(STACK) build --ghc-options="-fllvm -O3 -Wno-error=unsupported-llvm-version" --flag '*:LLVM'

build-llvm:
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-build -j4 --ghc-options="-fllvm -Wno-error=unsupported-llvm-version"

test-stack-llvm:
	$(STACK) test --ghc-options="-fllvm -Wno-error=unsupported-llvm-version" --flag '*:LLVM'

test-llvm:
	$(CABAL) $(CABALOPTS) $(CABALFLAGS) new-test -j4 --ghc-options="-fllvm -Wno-error=unsupported-llvm-version"

interpreter :
	$(STACK) repl --no-build --ghc-options -fobject-code

cabal-interpreter :
	cabal $(CABALOPTS) $(CABALFLAGS) new-configure -flibrary-only
	cabal $(CABALOPTS) $(CABALFLAGS) new-repl --ghc-option="-dynamic" --ghc-option="-fobject-code" 

test : test-stack

test-stack :
	$(STACK) test --fast cifl-math-library:unit-tests


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
	$(STACK) haddock --haddock-arguments "--title=cifl-math-library -v --html --source-base=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/ --html-location=https://esapulkkinen.github.io/cifl-math-library/ --source-module=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F --source-entity=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F#%N --no-print-missing-docs" --ghc-options="+RTS -M786M -RTS" --no-haddock-deps

document-cabal : 
	cabal $(CABALOPTS) $(CABALFLAGS) new-haddock --offline -v --enable-documentation --haddock-options="--title=cifl-math-library --html --html-location=https://esapulkkinen.github.io/cifl-math-library/ --source-base=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/ --source-module=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F --source-entity=https://raw.githubusercontent.com/esapulkkinen/cifl-math-library/master/%F#%N --no-print-missing-docs" --ghc-options="+RTS -M786M -RTS"  > dist-newstyle/haddock-output

document-standalone :
	standalone-haddock -o docs --compiler-exe=$$(stack path --compiler-exe) --dist-dir=$$(stack path --dist-dir) --package-db=$$(stack path --snapshot-pkg-db) --package-db=$$(stack path --local-pkg-db) --hyperlink-source .

latex-document :
	cabal $(CABALOPTS) $(CABALFLAGS) new-haddock --enable-documentation --haddock-options="--title=cifl-math-library --latex" --haddock-all

publish_document : dependencegraph externaldepgraph document-standalone hscolor
	#cp -r dist-newstyle/build/*/ghc-*/cifl-math-library-*/doc/html/cifl-math-library/* docs/
	cp dist-newstyle/doc/html/cifl-math-library/*.pdf docs/
	cd html ; $(MAKE) all ; cd ..
	cp html/bibliography.html docs/bibliography.html
	sed -i 's%<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">%<!DOCTYPE html>%g' docs/*.html docs/cifl-math-library/*.html
	sed -i 's+COPYRIGHT">COPYRIGHT+COPYRIGHT" rel="license">COPYRIGHT+g' docs/index.html docs/cifl-math-library/index.html
	sed -i 's+<html xmlns+<html itemscope itemtype="http://schema.org/APIReference" lang="en" xmlns+' docs/*.html docs/cifl-math-library/*.html
	sed -i "s+<head>+<head profile=\"http://dublincore.org/specifications/dublin-core/dc-html/2008-08-04/\" prefix=\"og: http://ogp.me/ns#\">$$(cat metadata.txt | tr '\n' ' ')+g" docs/*.html docs/cifl-math-library/*.html

hscolor :


#cabal $(CABALOPTS) hscolour --all 

install :
	cabal $(CABALOPTS) $(CABALFLAGS) new-install -j2 --enable-documentation 

clean :
	stack clean --full
	rm -f dependencies-*.ps dependencies-*.pdf

old-clean :
	cabal $(CABALOPTS) $(CABALFLAGS) new-clean
	rm -f dependencies-*.ps dependencies-*.pdf
