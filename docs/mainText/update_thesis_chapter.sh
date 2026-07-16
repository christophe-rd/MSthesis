###################
### WILDCHROKIE ###
###################
SOURCE="$HOME/github/wildchrokie/docs/manuscript/msWildchrokie.tex"
DEST="$HOME/github/MSthesis/docs/mainText/chapter1.tex"
TITLE=$(grep -o '{\\Large [^}]*}' "$SOURCE" | sed -E 's/\{\\Large (.*)\}/\1/')
awk '
/\\begin{document}/ {inside=1; next}
/\\end{document}/ {inside=0}
/\\begin{knitrout}/ {skip=1; next}
/\\end{knitrout}/ {skip=0; next}
/\\begin{center}/ {titleblock=1; next}
/\\end{center}/ {titleblock=0; next}
/^\$\^[0-9]/ {next}
/^\\bibliography/ {next}
/^\\bibliographystyle/ {next}
inside && !skip && !titleblock &&
!/^\\title{/ &&
!/^\\author{/ &&
!/^\\date{/ &&
!/^\\maketitle/
' "$SOURCE" > "$DEST"
sed -i '' "1s/^/\\\\chapter{$TITLE}\n\\\\label{ch:Wildchrokie}\n/" "$DEST"
sed -i '' 's|\.\./\.\./analyses/figures/|../figures/wildchrokie/|g' "$DEST"
echo "Updated $DEST"

##########################
### Coringtreespotters ###
##########################
SOURCE="$HOME/github/coringtreespotters/docs/manuscript/msCoringtreespotters.tex"
DEST="$HOME/github/MSthesis/docs/mainText/chapter2.tex"
TITLE=$(grep -o '{\\Large [^}]*}' "$SOURCE" | sed -E 's/\{\\Large (.*)\}/\1/')
awk '
/\\begin{document}/ {inside=1; next}
/\\end{document}/ {inside=0}
/\\begin{knitrout}/ {skip=1; next}
/\\end{knitrout}/ {skip=0; next}
/\\begin{center}/ {titleblock=1; next}
/\\end{center}/ {titleblock=0; next}
/^\$\^[0-9]/ {next}
# DROP bibliography lines if they exist
/^\\bibliography/ {next}
/^\\bibliographystyle/ {next}
inside && !skip && !titleblock &&
!/^\\title{/ &&
!/^\\author{/ &&
!/^\\date{/ &&
!/^\\maketitle/
' "$SOURCE" > "$DEST"
sed -i '' "1s/^/\\\\chapter{$TITLE}\n\\\\label{ch:CoringTreespotters}\n/" "$DEST"
# Remove the Ball command since it's already defined in chapter 1
sed -i '' '/^\\newcommand{\\Ball}/d' "$DEST"
# Rewrite figure paths to match the rsync destination below
sed -i '' 's|\.\./\.\./analyses/figures/|../figures/coringtreespotters/|g' "$DEST"
echo "Updated $DEST"


# Copy figures and delete what's currently in that directory
rsync -av --delete \
  "$HOME/github/wildchrokie/analyses/figures/" \
  "$HOME/github/MSthesis/docs/figures/wildchrokie/"
rsync -av --delete \
  "$HOME/github/coringtreespotters/analyses/figures/" \
  "$HOME/github/MSthesis/docs/figures/coringtreespotters/"
echo "Updated figures"