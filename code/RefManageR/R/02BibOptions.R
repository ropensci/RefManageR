#' Set options/hooks for RefManageR
#' 
#' This function is used to access and set package options for RefManageR, similar to \code{\link{options}}.  
#' The options are listed in the details
#' @param ... a character vector or strings specifying option names to access; or to set options values, 
#' a named list or vector of option values or options specified in name=value pairs.
#' @param restore.defaults logical; if TRUE, \code{...}'s are ignored and all package options are restored to their
#' defaults.
#' @export
#' @details The following are valid package options. 
#' 
#' \bold{Options for searching/indexing a BibEntry object.  See \code{\link{[.BibEntry}} and 
#' \code{\link{[<-.BibEntry}}}
#' \enumerate{
#' \item \code{match.author} - string; controls how name list fields (author, editor, translator, etc.) are matched 
#' when searching for names.
#' \dQuote{family.with.initials} require family names and given name initials to match, \dQuote{exact} requires names to match
#' exactly, and any other value results in only family names being compared (the default).
#' \item \code{match.date} - string; controls how date fields are matched when searching.  If \dQuote{year.only} (the default),
#' only years are checked for equality when comparing dates, otherwise months and days will also be compared,
#' if they are available.
#' \item \code{use.regex} - logical; if \code{TRUE}, regular expressions are used when searching non-date fields; otherwise, exact
#' matching is used.
#' \item \code{ignore.case} - logical; if \code{TRUE}, case is ignored when searching.
#' \item \code{return.ind} - logical; if \code{TRUE} the return value of \code{\link{SearchBib}} and the operators 
#' \code{\link{[.BibEntry}}, will be the indices of any matches; otherwise, a \code{BibEntry}
#' object is returned.
#' }
#' 
#' \bold{Options for Printing with \code{\link{print.BibEntry}} and \code{\link{PrintBibliography}}}
#' \enumerate{
#' \item \code{bib.style} - string; Biblatex bibliography style to use when printing and formatting a BibEntry object.  Possible
#' values are \dQuote{numeric} (default), \dQuote{authoryear}, \dQuote{authortitle}, \dQuote{alphabetic}, \dQuote{draft}.
#' \item \code{first.inits} - logical; if \code{TRUE}, only given name initials are displayed when printing; otherwise, full names
#' are used.
#' \item \code{dashed} - logical; if \code{TRUE} and \code{bib.style = "authoryear"} or \code{bib.style = "authortitle"},
#' recurring author and editor names are replaced with \dQuote{---} when printing.
#' \item \code{sorting} - string; controls how BibEntry objects are sorted.  Possible values are \dQuote{nty}, \dQuote{nyt}, 
#' \dQuote{nyvt}, \dQuote{anyt}, \dQuote{anyvt}, \dQuote{ynt}, \dQuote{ydnt}, \dQuote{none}, \dQuote{debug};  see 
#' \code{\link{sort.BibEntry}}
#' \item \code{max.names} - numeric; maximum number of names to display before using \dQuote{et al.} when formatting and printing name
#' list fields.  This is also the minimum number of names that will be displayed if \dQuote{et al.} is used 
#' (minnames package option in Biblatex) 
#' \item \code{no.print.fields} character vector; fields that should not be printed, 
#' e.g., doi, url, isbn, etc.
#' \item \code{style} - character string naming the printing style.  Possible values are 
#' plain text (style \dQuote{text}), BibTeX (\dQuote{Bibtex}), BibLaTeX (\dQuote{Biblatex}),
#' a mixture of plain text and BibTeX as 
#' traditionally used for citations (\dQuote{citation}), HTML (\dQuote{html}), 
#' LaTeX (\dQuote{latex}), \dQuote{markdown}, 
#' R code (\dQuote{R}), and a simple copy of the textVersion elements 
#' (style \dQuote{textVersion}, see \code{\link{BibEntry}})
#' }
#' 
#' \bold{Options for the \code{\link{Cite}} functions}
#' \enumerate{
#' \item \code{cite.style} - character string; bibliography style to use to generate citations.  
#' \item \code{style} - as above, but used to format the citations.  
#' \item \code{hyperlink} - character string or logical; for use with \code{style = "markdown"}
#' and \code{style = "html"} (ignored otherwise).  If \code{FALSE}, no hyperlink
#' will be generated for the citation or in the bibliography when printing.  If set equal to \code{"to.bib"}, then hyperlinks will be
#' generated pointing connecting the citation and bibliography.  The default value, \code{"to.doc"},
#' will try to create the hyperlink using the \code{url}, \code{doi}, or \code{eprint} fields of 
#' entry.  If these fields are not available, the hyperlink will point to the bibliography.  See
#' also \code{\link{open.BibEntry}}.
#' \item \code{super} - logical; should superscripts be used for numeric citations?  Ignored if
#'  \code{cite.style != "numeric"}.
#' \item \code{max.names} - numeric; same as above, except for citations.
#' \item \code{longnamesfirst} logical; should the first time a citation appears in the text
#' not be truncated at \code{max.names}?
#' \item \code{bibpunct} - character vector; punctuation to use in a citation.  The entries in \code{bibpunct} are as follows
#' \enumerate{
#' \item The left delimiter for non-alphabetic and non-numeric citation styles
#' \item The right delimiter for non-alphabetic and non-numeric citation styles
#' \item The left delimiter for alphabetic and numeric citation styles
#' \item The right delimiter for alphabetic and numeric citation styles 
#' \item The separator between references in a citation.
#' \item Punctuation to go between the author and year.
#' }
#' }
#' 
#' \bold{Other}
#' \enumerate{
#' \item \code{check.entries} - string or \code{FALSE}; if \code{FALSE} entries are not checked to ensure that they have all the 
#' required fields for the type of entry; if \dQuote{warn} then entries are checked, but only a warning is issued and the 
#' entry is processed anyway; otherwise an error is produced if an entry does not have the required fields (default).  Note that
#' the majority of fields listed as required for a particular entry type in the Biblatex manual are not actually required for
#' Biblatex to produce an entry.
#' \item \code{merge.fields.to.check} - character vector; for \code{\link{merge.BibEntry}} and the operator \code{\link{+.BibEntry}},
#' the fields that should be checked when comparing entries for equality when merging BibEntry objects.  Specifying 
#' \dQuote{all} results in all fields be checked with \code{\link{duplicated}}.  The default is \dQuote{key} to only check for
#' duplicated keys.
#' }
#' @note If \code{...} is missing and \code{restore.defaults = FALSE}, all options and their current values will be returned
#' as a list.
#' @return if a vector of option names is supplied, the current value of the requested options, or if \code{...} is missing,
#' all current option values; otherwise, when setting options the old values of the changed options are (invisibly) 
#' returned as a list.
#' @seealso \code{\link{print.BibEntry}}, \code{\link{BibEntry}}, \code{\link{options}}
#' @examples
#' BibOptions()
#' BibOptions("first.inits", "bib.style")
#' 
#' oldopts <- BibOptions(first.inits = FALSE, bib.style = "authoryear")
#' oldopts
#' BibOptions(oldopts)
#' 
#' BibOptions(restore.defaults = TRUE)
BibOptions <- function(..., restore.defaults = FALSE){
  if (restore.defaults)
    return(invisible(mapply(assign, .BibOptNames, .Defaults, MoreArgs = list(envir=.BibOptions))))
    
  if (missing(...))
    return(mget(.BibOptNames, envir = .BibOptions))
  opts <- list(...)
  nom <- names(opts)
  
  if (is.null(nom)  && !is.list(opts[[1L]])){
    opts <- unlist(opts)
    return(mget(opts[opts %in% .BibOptNames], envir = .BibOptions))
  }else{
    if (is.list(opts[[1L]])){
      opts <- opts[[1L]]
      nom <- names(opts)
    }
      
    if (any(!nom %in% .BibOptNames))
      stop('Invalid name specified, see ?BibOptions')
    ind <- nom %in% .LogicalBibOptNames
    if (any(ind)){
      opts[ind] <- as.logical(opts[ind])
      if (any(is.na(opts[ind])))
        stop("One of the specified option values should be logical and is not, see ?BibOptions")
      names(opts[ind]) <- nom[ind]
    }
      
    oldopts <- mget(nom, envir=.BibOptions)
    mapply(assign, nom, opts, MoreArgs = list(envir=.BibOptions))
    invisible(oldopts)
  }
}

.Defaults <- list(match.author='family.name', match.date='year.only', return.ind=FALSE, 
              merge.fields.to.check = 'key', bib.style = 'numeric', first.inits = TRUE, 
              dashed = TRUE, sorting = NULL, check.entries = 'error', use.regex = TRUE, 
              ignore.case = TRUE, max.names = 3, cite.style = "authoryear",
              longnamesfirst = TRUE, hyperlink = "to.doc", style = "text",
              super = FALSE, bibpunct = c("(", ")", "[", "]",  ";", ","),
              no.print.fields = character(0))  
.BibOptions <- list2env(.Defaults)
.BibOptNames <- names(.Defaults)
.LogicalBibOptNames <- c("return.ind", "first.inits", "dashed", "use.regex", "ignore.case", 
                         "longnamesfirst", "super")
.cites <- new.env()
assign("indices", logical(0), .cites)
assign("labs", character(0), .cites)
assign("sty", "authoryear", .cites)
globalVariables("return.labs")