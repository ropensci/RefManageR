#' Import and Manage BibTeX and BibLaTeX references with RefManageR
#' 
#' RefManageR provides tools for importing and working with bibliographic references.  It greatly enhances the 
#' \code{bibentry} class by providing a class BibEntry which stores BibTeX and BibLaTeX references, supports UTF-8, 
#' and can be easily searched, combined, printed in a number of styles, and exported.  Interfaces to NCBI's Entrez, CrossRef,
#' and Zotero are provided for importing references and references can be created from locally stored PDFs using Poppler.  
#' Citations can be generated and a list of references printed for inclusion in e.g. RMarkdown documents.
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
#' in their document.  See \code{\link{Cite}}.
#' \bold{Additional features}
#' 
#' All sorting methods for bibliographies available in the BibLaTeX LaTeX package have been implemented see 
#' \code{\link{sort.BibEntry}} and the references.
#' 
#' Using \code{\link{open.BibEntry}} electronic copies of references can be opened in a PDF viewer or web browser.
#' @keywords package
#' @references McLean, M.W. and R.J. Carroll (2014). Biblatex Bibliography Managament in R Using the RefManager Package.
#' \url{http://stat.tamu.edu/~mmclean}.
#' @references Lehman, P., P. Kime, A. Boruvka, and J. Wright (2013). The biblatex Package.
#' \url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @references Hornik, K., D. Murdoch, and A. Zeileis (2012). 
#' Who Did What? The Roles of R Package Authors and How to Refer to Them. The R Journal \bold{4}, 1.
#' \url{http://journal.r-project.org/archive/2012-1/RJournal_2012-1_Hornik~et~al.pdf}
#' @references Patashnik, O (1988). Bibtexing. \url{http://ctan.sharelatex.com/tex-archive/biblio/bibtex/base/btxdoc.pdf}.
NULL