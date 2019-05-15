eis:
	pandoc README.md --toc --template=eisvogel --variable urlcolor=blue --variable geometry:margin=1in -o README.pdf -N --pdf-engine=xelatex

pdf:
	pandoc README.md --toc --variable urlcolor=blue --variable geometry:margin=1in -o README.pdf -N --pdf-engine=xelatex

.DEFAULT: pdf
