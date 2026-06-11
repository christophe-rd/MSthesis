#!/bin/bash

SOURCE="$HOME/github/wildchrokie/docs/manuscript/manuscriptOutline.tex"
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


SOURCE="$HOME/github/coringtreespotters/docs/manuscript/TSmanuscriptOutline.tex"
DEST="$HOME/github/MSthesis/docs/outline/chapter2.tex"

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

rsync -av --delete \
  "$HOME/github/wildchrokie/analyses/figures/" \
  "$HOME/github/MSthesis/docs/figures/wildchrokie/"

echo "Updated figures"
