#' Import and Manage BibTeX and BibLaTeX references with RefManager
#' 
#' RefManageR provides tools for importing and working with bibliographic references.  It introduces a class \code{BibEntry}
#' which inherits from the \code{\link{bibentry}} class which stores BibTeX and BibLaTeX references and can be easily searched,
#' combined, printed, and exported.  
#' @details 
#' 
#' \bold{Importing and Creating References}
#' 
#' BibEntry objects can be created directly using the \code{\link{BibEntry}} function.  \code{.bib} files can be read into R
#' using the \code{\link{ReadBib}} function.
#' Tools are provided for importing references from Crossref, Zotero, Google Scholar, 
#' and PDfs.
#' 
#' \bold{Manipulating BibEntry objects}
#' 
#' BibEntry objects may be searched by field, name
#' 
#' \bold{Printing and Exporting Bibliographies}
#' 
#' The \code{\link{print.BibEntry}} function can print in a number of formats (e.g. text, html) and most of the 
#' base bibliography styles available with BibLaTeX (e.g. alphabetic, numeric, authortitle, and authoryear).  
#' \code{\link{toBibtex.BibEntry}} will convert a BibEntry object to a character vector containing lines of 
#' a BibTeX file, converting fields, entry types and expanding crossreferences as needed to coerce BibLaTeX entries to
#' BibTeX.  \code{\link{toBiblatex}} converts the BibEntry object to a character vector containing lines of 
#' the corresponding BibLaTeX file.
#' 
#' \bold{Sorting BibEntries}
#' 
#' All sorting methods for bibliographies available in the BibLaTeX LaTeX package have been implemented see 
#' \code{\link{sort.BibEntry}} and the references.
#' 
#' @references McLean, M.W. and R.J. Carroll (2014). Biblatex Bibliography Managament in R Using the RefManager Package.
#' \url{http://stat.tamu.edu/~mmclean}.
#' @references Lehman, P., P. Kime, A. Boruvka, and J. Wright (2013). The biblatex Package.
#' \url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @references Hornik, K., D. Murdoch, and A. Zeileis (2012). 
#' Who Did What? The Roles of R Package Authors and How to Refer to Them. The R Journal \bold{4}, 1.
#' \url{http://journal.r-project.org/archive/2012-1/RJournal_2012-1_Hornik~et~al.pdf}
#' @references Patashnik, O (1988). Bibtexing. \url{http://ctan.sharelatex.com/tex-archive/biblio/bibtex/base/btxdoc.pdf}.
