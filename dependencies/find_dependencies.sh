#!/bin/bash
part="${1:-Tools}"
echo "<gxl><graph id=\"dependencies-${part}\" edgeids=\"true\" edgemode=\"directed\">"
cd Math
find . -name \*.lhs | grep -v Test | egrep -v '(^Matrix.lhs|^Tools.lhs|^Model.lhs)' | grep ${part} | sed -e 's/.lhs//g' -e 's+/+.+g' -e 's+\.\.++g' -e 's%^\(.*\)%<node id="\1"><attr name="label"><string>\1</string></attr></node>%g'
find . -name \*.lhs | grep -v Test | grep -v '^(Matrix.lhs|Tools.lhs|Model.lhs)' | grep ${part} | xargs egrep -o '^>import (safe )?(qualified )? ?([a-zA-Z0-9\.]+)' | grep -v Prelude | grep -v oolib | grep -v Data | grep -v Control | sed -e 's/>import //g' -e 's/safe //g' -e 's/qualified //g' -e 's/.lhs//g' -e 's+/+.+g' -e 's+\.\.++g' | sed 's%\(.*\):\(.*\)%<edge from="\1" to="\2"><attr name="label"><string>\E</string></attr></edge>%g'
echo "</graph></gxl>"
