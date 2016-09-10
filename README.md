RefManageR
========
[![](https://travis-ci.org/mwmclean/RefManageR.svg?branch=master)](https://travis-ci.org/mwmclean/RefManageR/)
[![Coverage Status](https://coveralls.io/repos/mwmclean/RefManageR/badge.svg?branch=master)](https://coveralls.io/r/mwmclean/RefManageR?branch=master)

`RefManageR` provides tools for importing and working with
bibliographic references.  It greatly enhances the `bibentry` class by
providing a class `BibEntry` which stores `BibTeX` and `BibLaTeX` references,
supports `UTF-8` encoding, and can be easily searched by any field, by date
ranges, and by various formats for name lists (author by last names,
translator by full names, etc.). Entries can be updated, combined, sorted,
printed in a number of styles, and exported. `BibTeX` and `BibLaTeX` `.bib` files
can be read into `R` and converted to `BibEntry` objects.  Interfaces to
`NCBI Entrez`, `CrossRef`, and `Zotero` are provided for importing references and
references can be created from locally stored `PDF` files using `Poppler`.  Includes
functions for citing and generating a bibliography with hyperlinks for
documents prepared with `RMarkdown` or `RHTML`.

Please see the [vignette](https://cran.r-project.org/web/packages/RefManageR/vignettes/manual.pdf)
for an introduction and [NEWS](https://github.com/mwmclean/RefManageR/blob/master/inst/NEWS)
for the latest changes.

Using the latest version from `Github` requires first installing the newest version of the `bibtex` package
from `GitHub`:

```
install.packages("devtools")
devtools::install_github("romainfrancois/bibtex")
devtools::install_github("mwmclean/RefManageR")
```
