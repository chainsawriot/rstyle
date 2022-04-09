all: latex

.FORCE:

latex: .FORCE
	Rscript -e "tools::texi2pdf('RJwrapper.tex')"

zip: latex
	zip yenetal.zip yenetal.tex yenetal.R yenetal.bib tab1.csv RJwrapper.tex RJwrapper.pdf Makefile fig1.pdf fig2.pdf fig3.pdf fig4.pdf fig5.pdf fig6.pdf fig7.pdf
