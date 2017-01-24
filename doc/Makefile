all : doc.tex
	pdflatex doc.tex
	bibtex doc
	pdflatex doc.tex
	pdflatex doc.tex

clean :
	rm doc.aux
	rm doc.bbl
	rm doc.blg
	rm doc.log


