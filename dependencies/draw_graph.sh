#!/bin/sh
part="${1:-Tools}"
mydir=$(cd $(dirname $0) ; pwd)
#export GS_FONTPATH=/usr/share/fonts/type1/gsfonts:$(echo $(ls -d /usr/share/fonts/truetype/*) | sed 's/ /:/g')
#export GDFONTPATH=/usr/share/fonts/type1/gsfonts:$(echo $(ls -d /usr/share/fonts/truetype/*) | sed 's/ /:/g')
#export GS_LIB=/usr/share/fonts/type1/gsfonts:$(echo $(ls -d /usr/share/fonts/truetype/*) | sed 's/ /:/g')
${mydir}/find_dependencies.sh ${part} | gxl2gv -d | fdp -v -Tpdf -Gratio=auto -GK=100 -Gsize=10,10 -Gnodesep=1 -Granksep=1 -Nfixedsize=true -Nstyle=solid -Ncolor=black -Nfontcolor=black -Nfontname=Times-Roman -Nfontsize=1000 -Nheight=30 -Nwidth=180 -Nshape=oval -s10 -Edir=forward -Earrowtail=normal -Earrowsize=30 > dependencies-${part}.pdf
#${mydir}/find_dependencies.sh ${part} | gxl2gv -d | fdp -v -Tps2 -Gratio=auto -GK=100 -Gsize=10,10 -Gnodesep=1 -Granksep=1 -Nfixedsize=true -Nstyle=solid -Ncolor=black -Nfontcolor=black -Nfontname=Times-Roman -Nfontsize=1000 -Nheight=30 -Nwidth=180 -Nshape=oval -s10 -Edir=forward -Earrowtail=normal -Earrowsize=30 > dependencies-${part}.ps
#ps2pdf14 -dPDFSETTINGS=/prepress -dEmbedAllFonts=false dependencies-${part}.ps dependencies-${part}.pdf
