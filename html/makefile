
all : bibliography.html bibliography.cff

bibliography.html : bibliography.xslt bibliography.xml
	xsltproc bibliography.xslt bibliography.xml > bibliography.html

bibliography.cff : bibliography-to-cff.xslt bibliography.xml CITATION.cff.xslt
	xsltproc bibliography-to-cff.xslt bibliography.xml | tail -n +2 > bibliography.cff
	cp bibliography.cff ../CITATION.cff

