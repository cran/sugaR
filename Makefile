
all:
	@echo "This makefile only serves as a wrapper for the 'install', 'build' and 'check' R commands."

build:	clean
	R CMD build .

look inst/doc/sugaR.pdf:
	R CMD build .
	kpdf inst/doc/sugaR.pdf
	

install: clean inst/doc/sugaR.pdf
	R CMD INSTALL .

check: clean
	R CMD check .

commit:
	svn commit

clean:
	rm -rf ..Rcheck `find inst/doc -name "*.pdf" -o -name "*.eps" -o -name "*.out" -o -name "*.log" -o -name "*.aux" -o -name "*.tex" -o -name "*.ps" -o -name "*.dvi" |grep -v sugaR.pdf` sugaR*.tar.gz

.PHONY: clean check install vignette build look
