##
# Project Title
#
# @file
# @version 0.1
.PHONY: snippets

sprawko/sprawozdanie.pdf: sprawko/sprawozdanie.tex snippets
	cd sprawko; pdflatex -shell-escape sprawozdanie.tex
	cd sprawko; pdflatex -shell-escape sprawozdanie.tex

snippets:
	-rm -r sprawko/snippets
	mkdir sprawko/snippets
	./snippets sprawko/snippets/ src/mh1/alg.clj
# end
