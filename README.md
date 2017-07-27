RefManageR
========
[![](https://travis-ci.org/ropensci/RefManageR.svg?branch=master)](https://travis-ci.org/ropensci/RefManageR/)
[![AppVeyor Build Status](http://ci.appveyor.com/api/projects/status/github/ropensci/RefManageR?branch=master&svg=true)](http://ci.appveyor.com/project/ropensci/RefManageR)
[![Coverage Status](https://coveralls.io/repos/ropensci/RefManageR/badge.svg?branch=master)](https://coveralls.io/r/ropensci/RefManageR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RefManageR)](https://cran.r-project.org/package=RefManageR)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/RefManageR)](https://cran.r-project.org/package=RefManageR)
[![](https://ropensci.org/badges/120_status.svg)](https://github.com/ropensci/onboarding/issues/120)
[![](https://zenodo.org/badge/DOI/10.5281/zenodo.835547.svg)](https://github.com/openjournals/joss-reviews/issues/269)

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

Please see the [vignette](https://arxiv.org/pdf/1403.2036v1)
for an introduction and [NEWS](https://github.com/ropensci/RefManageR/blob/master/inst/NEWS.md)
for the latest changes.

To install the latest version from `GitHub`:

```
install.packages("devtools")
devtools::install_github("ropensci/RefManageR")
```
[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
