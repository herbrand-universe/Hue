all : doc.tex
	pdflatex doc.tex
	bibtex doc
	pdflatex doc.tex
	pdflatex doc.tex

clean :
	rm -f doc.aux
	rm -f doc.bbl
	rm -f doc.blg
	rm -f doc.log
	rm -f pts.aux
	rm -f coc.aux


