##
# Project Title
#
# @file
# @version 0.1

sprawko/sprawozdanie.pdf: sprawko/sprawozdanie.tex
	cd sprawko; pdflatex -shell-escape sprawozdanie.tex

# end
