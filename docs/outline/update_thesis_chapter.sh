#!/bin/bash

SOURCE="$HOME/github/coringtreespotters/docs/manuscript/msCoringtreespotters.tex"
DEST="$HOME/github/MSthesis/docs/outline/chapter1.tex"

awk '
/\\begin{document}/ {inside=1; next}
/\\end{document}/ {inside=0}

/\\begin{knitrout}/ {skip=1; next}
/\\end{knitrout}/ {skip=0; next}

# DROP bibliography lines if they exist
/^\\bibliography/ {next}
/^\\bibliographystyle/ {next}

inside && !skip &&
!/^\\title{/ &&
!/^\\author{/ &&
!/^\\date{/ &&
!/^\\maketitle/
' "$SOURCE" > "$DEST"

echo "Updated $DEST"
