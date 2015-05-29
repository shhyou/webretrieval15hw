#!/bin/bash
mkdir -p B00902107
pdflatex report.tex && rm -f report.aux report.log report.synctex.gz
mv report.pdf ./B00902107/REPORT.pdf
cp h_csst.rank ./B00902107/B00902107.pagerank1
cp h_standford.rank ./B00902107/B00902107.pagerank2
cp h_0326.rank ./B00902107/B00902107.pagerank3
cp compilePR.sh ./B00902107/
cp executePR.sh ./B00902107/
cp LICENSE ./B00902107/
cp pagerank.cabal ./B00902107/
cp Main.hs ./B00902107/
cp Setup.hs ./B00902107/
zip -9 B00902107.zip B00902107/
