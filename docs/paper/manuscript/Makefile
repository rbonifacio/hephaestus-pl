ssh-user = softlang@linux.uni-koblenz.de
www-dir = /home/softlang/www/hpl

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

upload: all
	ssh ${ssh-user} mkdir -p ${www-dir}
	scp -r -p paper.pdf index.html ${ssh-user}:${www-dir}

upload-html:
	scp -r -p index.html ${ssh-user}:${www-dir}
