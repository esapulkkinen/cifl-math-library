
all : bibliography.html bibliography.cff

bibliography.html : bibliography.xslt bibliography.xml
	xsltproc bibliography.xslt bibliography.xml > bibliography.html

bibliography.cff : bibliography-to-cff.xslt bibliography.xml CITATION.cff.xslt
	xsltproc bibliography-to-cff.xslt bibliography.xml > bibliography.cff
	cp bibliography.cff ../CITATION.cff

