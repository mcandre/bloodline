# bloodline - Trace the lineages of your favorite programming languages

## HOMEPAGE

[http://www.yellosoft.us/bloodline](http://www.yellosoft.us/bloodline)

# EXAMPLE

	$ make
	runhaskell bloodline.hs --depth 3 --lang Haskell > bloodline.dot
	dot -Tpdf bloodline.dot > bloodline-intermediate.pdf
	pdflatex bloodline
	$(OPEN) bloodline.pdf

# REQUIREMENTS

- [Haskell](http://haskell.org/)
- [hsparql](http://hackage.haskell.org/package/hsparql)
- [GraphViz](http://www.graphviz.org/)
- [LaTeX](http://latex.org/)
- [Cabal graphviz](http://hackage.haskell.org/packages/archive/graphviz/latest/doc/html/Data-GraphViz.html)

# CREDITS

Copyright 2012 Andrew Pennebaker

Inspiration: O'Reilly's [The History of Programming Languages](http://oreilly.com/pub/a/oreilly/news/languageposter_0504.html)