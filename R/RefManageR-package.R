#' Import and Manage BibTeX and BibLaTeX references with RefManageR
#' 
#' RefManageR provides tools for importing and working with
#' bibliographic references.  It greatly enhances the bibentry class by
#' providing a class BibEntry which stores BibTeX and BibLaTeX references,
#' supports UTF-8 encoding, and can be easily searched by any field, by date
#' ranges, and by various formats for name lists (author by last names,
#' translator by full names, etc.). Entries can be updated, combined, sorted, printed
#' in a number of styles, and exported. BibTeX and BibLaTeX .bib files can be
#' read into R and converted to BibEntry objects.  Interfaces to NCBI's
#' Entrez, CrossRef, and Zotero are provided for importing references and
#' references can be created from locally stored PDFs using Poppler.  Includes
#' functions for citing and generating a bibliography with hyperlinks for
#' documents prepared with RMarkdown or RHTML.
#' @name RefManageR-package
#' @aliases RefManageR refmanager
#' @docType package
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}  
#' @details 
#' \bold{Importing and Creating References}
#' 
#' BibEntry objects can be created directly using the \code{\link{BibEntry}} function.  \code{.bib} files can be read into R
#' using the \code{\link{ReadBib}} function.
#' Tools are provided for importing references from Crossref, Zotero, Google Scholar, 
#' and PDFs and looking up PubMed ID's and DOIs.  See \code{\link{ReadPDFs}}, \code{\link{ReadZotero}}, \code{\link{ReadCrossRef}}, \code{\link{ReadGS}},
#' \code{\link{ReadPubMed}}, \code{\link{GetPubMedByID}}, \code{\link{GetPubMedRelated}}.
#' 
#' \bold{Manipulating BibEntry objects}
#' 
#' BibEntry objects may be searched and indexed by field values, name lists, keys, dates, date ranges, etc.  
#' See \code{\link{[.BibEntry}}, \code{\link{[<-.BibEntry}}, \code{\link{[[.BibEntry}}, \code{\link{$.BibEntry}}.
#' 
#' \bold{Printing and Exporting Bibliographies}
#' 
#' The \code{\link{print.BibEntry}} function can print in a number of formats (e.g. text, html) and most of the 
#' base bibliography styles available with BibLaTeX (e.g. alphabetic, numeric, authortitle, and authoryear).  
#' \code{\link{toBibtex.BibEntry}} will convert a BibEntry object to a character vector containing lines of 
#' a BibTeX file, converting fields, entry types and expanding crossreferences as needed to coerce BibLaTeX entries to
#' BibTeX.  \code{\link{toBiblatex}} converts the BibEntry object to a character vector containing lines of 
#' the corresponding BibLaTeX file.  The results can be written to a file using \code{\link{WriteBib}}.
#' 
#' Citations can be gerenated in a number of styles using one of the available functions for 
#' citations.  A list of references can be printed based on the works the user has cited thus far
#' in their document.  See \code{\link{Cite}}.  The citations and bibliography can be printed 
#' including hyperlinks using either the R Markdown or R HTML formats.
#' 
#' \bold{Additional features}
#' 
#' All sorting methods for bibliographies available in the BibLaTeX LaTeX package have been implemented see 
#' \code{\link{sort.BibEntry}} and the references.
#' 
#' Using \code{\link{open.BibEntry}} electronic copies of references can be opened in a PDF viewer or web browser.
#' 
#' The convenience function \code{\link{BibOptions}} is provided for setting defaults for commonly used
#' functions such as \code{\link{print.BibEntry}}, \code{\link{[.BibEntry}}, and 
#' \code{\link{Cite}}.  Its interface is similar to \code{\link{options}}.
#' @keywords package
#' @references McLean, M. W. (2014). Straightforward Bibliography Management in R Using the RefManageR Package.
#' \href{http://arxiv.org/abs/1403.2036}{arXiv: 1403.2036 [cs.DL]}. Submitted.
#' @references Lehman, P., P. Kime, A. Boruvka, and J. Wright (2013). The biblatex Package.
#' \url{http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @references Hornik, K., D. Murdoch, and A. Zeileis (2012). 
#' Who Did What? The Roles of R Package Authors and How to Refer to Them. The R Journal \bold{4}, 1.
#' \url{http://journal.r-project.org/archive/2012-1/RJournal_2012-1_Hornik~et~al.pdf}
#' @references Patashnik, O (1988). Bibtexing. \url{http://ctan.sharelatex.com/tex-archive/biblio/bibtex/base/btxdoc.pdf}.
NULL
