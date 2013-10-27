# Mathew McLean
# October 23, 2013
# biblatex temp work





# useful stack overflow stuff on biblatex
# http://tex.stackexchange.com/questions/37095/compatibility-of-bibtex-and-biblatex-bibliography-files
# http://tex.stackexchange.com/questions/25701/bibtex-vs-biber-and-biblatex-vs-natbib
# http://tex.stackexchange.com/questions/96958/switching-to-biblatex-how-to-load-custom-elsarticle-harv-bst-style
# http://tex.stackexchange.com/questions/111846/biblatex-2-custom-fields-only-one-is-working

# biblatex + knitr: http://texblog.org/2013/08/20/rknitr-automatic-bibliography-generation-with-biblatex-in-rstudio/
# biblatex + knitr 2 : http://support.rstudio.org/help/discussions/problems/2930-rstudio-knitr-biber

# useful source code to borrow from
# bibtex:::write.bib
# bibtex:::read.bib
# bibtex:::make.citation.list
# bibtex:::make.bib.entry  # calls bibentry, broken by non-bibtex supported stuff
#
# utils:::bibentry
# utils:::print.bibentry
# utils:::format.bibentry  # broken by non-bibtex supported stuff
# utils:::.bibentry_expand_crossrefs
# utils:::toBibtex.bibentry # some code for formatting author and editor
# utils:::citation


test <- readPDF(elem=list(uri='~/biblatex.pdf'))
test(elem=list(uri='~/biblatex.pdf'))

GetBibtexGS(title='wand variational bayes')

regexpr("(?<=').*?(?=')", "hello \'blah blah\' asdf ")
regexpr('(?<=\")[^"]+(?=\")',"hello \'blah blah\' asdf ")
regexpr("\\s/^.\*\"(.*)".*\"$", "hello \'blah blah\' asdf ")
regexpr("/\"(.+?)/\"", "hello \"blah blah\" asdf ", perl=TRUE)
regexpr("(\\?\")(.*?)", "hello \"blah blah\" asdf ")
regexpr("[:alnum:]\"+", "hello \"blah blah\" asdf ")
test <- "hello \"blah blah\" asdf "
m <- regexpr("\"(.*?)\"", "hello \"blah blah\" asdf ")
#m <- regexpr("\"([^\\^\"]+)\"\3", "hello \"blah blah\" asdf ")
res <- regmatches("hello \"blah blah\" asdf ", m)
res <- substring(res, 2, nchar(res)-1)
strsplit("hello \"blah blah\" asdf ","?\".*\"")

