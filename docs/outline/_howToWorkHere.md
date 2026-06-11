# how to work here
## 2026-06-11

I created a shell script with chatGPT so I could more robustly copy and paste the content of my two chapters into my thesis repo. for this I wrote:
`update_thesis_chapter.sh`

For now, it copies and paste the content of my second chapter into the thesis outline repo, by removing the preambule because it won't render here with it. It also removes the bibliography block, because it runs somewhere else, and copies the bibliography from Wildchrokie. 

To edit the shell script, run the following in the terminal. 
`nano update_thesis_chapter.sh`
Then save

Then run: 
`chmod +x update_thesis_chapter.sh`
And:
`./update_thesis_chapter.sh`

Then render `diss.tex`