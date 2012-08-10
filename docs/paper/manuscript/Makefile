all:
	pdflatex paper
	-bibtex paper
	pdflatex paper
	pdflatex paper

clean:
	rm -f *.aux *.blg *.pdf *.bbl *.log

push:
	git commit -a
	git push
