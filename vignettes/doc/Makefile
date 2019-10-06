LATEX = pdflatex 

refs = ref.bib
srcrnw = paper.Rnw
src = $(srcrnw)
cachedir = cache
figdir = Figures
srctex = $(subst Rnw,tex,$(srcrnw))
srcaux = $(subst Rnw,aux,$(srcrnw))

outpdf = $(subst Rnw,pdf,$(srcrnw))
outr = $(subst Rnw,R,$(srcrnw))

.PHONY: pdf
pdf: $(outpdf)

.PHONY: all
all: r pdf

$(outpdf): $(srctex) $(refs)
	ERROR=0; \
	for i in 1 2 3; do \
		echo "Round $$i...................."; \
		$(LATEX) -halt-on-error -interaction=nonstopmode $(srctex); \
		ERROR=$$?; \
		if [[ $${ERROR} != 0 ]];then \
			echo "Error code $${ERROR}."; \
			exit $${ERROR}; \
		fi; \
		bibtex $(srcaux); \
	done

$(srctex): $(src)
	R -e 'knitr::knit("$<")'

$(figdir): $(srctex)

.PHONY:	vignette
vignette: $(srctex) $(figdir)
	make
	cp paper.tex ../paper.Rnw
	cp -r $(figdir) ..
	cp ref.bib ..
	rm -f $(outpdf)
	make clean

.PHONY: r
r: $(src)
	R -e 'knitr::purl("$<")'

.PHONY: compress
compress: $(src) $(refs) $(outpdf) $(outr)
	zip svverse.zip $^ Makefile

.PHONY: clean
clean:
	rm -fv ./*.{aux,bbl,blg,fdb_latexmk,fls,log,out,tex} ./*-concordance.tex

.PHONY: distclean
distclean: clean
	rm -rf $(cachedir)
	rm -rf $(figdir)
	rm -f $(outpdf)
	rm -f $(srctex)
	rm -f $(outr)

.PHONY: show
show: $(outpdf)
	xdg-open $< &

