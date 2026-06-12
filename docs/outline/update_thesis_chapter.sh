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
sed -i '' '1s/^/\\chapter{Wildchrokie}\n\\label{ch:Wildchrokie}\n/' "$DEST"
sed -i '' 's|/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/|../figures/wildchrokie/|g' "$DEST"
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
sed -i '' '1s/^/\\chapter{CoringTreespotters}\n\\label{ch:CoringTreespotters}\n/' "$DEST"
# Remove the Ball command since it's already defined in chapter 1
sed -i '' '/^\\newcommand{\\Ball}/d' "$DEST"
echo "Updated $DEST"
# Copy figures and delete what's currently in that directory
rsync -av --delete \
  "$HOME/github/wildchrokie/analyses/figures/" \
  "$HOME/github/MSthesis/docs/figures/wildchrokie/"
rsync -av --delete \
  "$HOME/github/coringtreespotters/analyses/figures/" \
  "$HOME/github/MSthesis/docs/figures/coringtreespotters/"
echo "Updated figures"