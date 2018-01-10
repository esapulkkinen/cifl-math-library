
all : configure build test dependencegraph document hscolor

all_with_install : all install

configure :
	cabal configure --enable-tests 
#--enable-shared --enable-executable-dynamic

build : 
	cabal build -j

interpreter :
	cabal repl

test : 
	cabal test -j --show-details=failures

dependencegraph :
	mkdir -p dist/doc/html/cifl-math-library
	dependencies/draw_graph.sh Tools
	dependencies/draw_graph.sh Graph
	dependencies/draw_graph.sh Matrix
	dependencies/draw_graph.sh Number
	mv dependencies-*.ps dependencies-*.pdf dist/doc/html/cifl-math-library/

document : 
	cabal haddock --haddock-options="--title=cifl-math-library" --verbose=0 --all --hyperlink-source > dist/haddock-output

hscolor :
	cabal hscolour --all

install :
	cabal install -j --enable-documentation 

clean :
	cabal clean
	rm -f dependencies-*.ps dependencies-*.pdf
