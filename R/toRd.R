#' Convert BibEntry object to a fragment of Rd code.
#'
#' Renders references in a BibEntry object as a fragment of Rd code, which can
#' then be rendered into text, HTML, or LaTex.
#' @param obj - An object of class BibEntry
#' @param style - The bibstyle to be used for converting \code{obj}; see
#' \code{\link{print.BibEntry}}
#' @param .sorting - the BibLaTeX sorting method to use; see
#' \code{\link{sort.BibEntry}}
#' @param ... - ignored
#' @S3method toRd BibEntry
#' @return Returns a character vector containing a fragment of Rd code that
#' could be parsed and rendered.
#' @seealso \code{\link{print.BibEntry}}, \code{\link{sort.BibEntry}},
#' \code{\link{BibEntry}}, \code{\link{bibstyle}}
#' @keywords internal
#' @noRd
#' @importFrom tools getBibstyle bibstyle toRd
toRd.BibEntry <- function(obj, ...) {
  .style <- .BibOptions$bib.style
  doc.style <- .BibOptions$style

  if (is.null(.style)){
    .style <- .BibOptions$bib.style <- 'numeric'
    style.env <- MakeBibLaTeX(docstyle = doc.style)
  }else if (.style %in% tools::getBibstyle(TRUE)){
    .BibOptions$bib.style <- .style
    style.env <- tools::bibstyle(.style)
  }else if (.style == "authoryear"){
    style.env <- MakeAuthorYear(docstyle = doc.style)
  }else if (.style %in% c('authortitle', 'alphabetic', 'numeric', 'draft')){
    #.style <- 'BibLaTeX'
    style.env <- MakeBibLaTeX(docstyle = doc.style, .style == "authortitle")
  }

  env <- new.env(hash = FALSE, parent = style.env)

  if (!(.style == 'authoryear' || .style == 'authortitle') ||
      !.BibOptions$dashed || is.null(obj$.duplicated))
    obj$.duplicated <- FALSE
  assign("bibstyle", .style, style.env)
#   maxnames <- .BibOptions$max.names  # for R CMD Check
#   assign("max.n", maxnames, style.env)

  bib <- unclass(obj)
  result <- character(length(bib))
  #browser()
  for (i in seq_along(bib)) {
    assign('paper', bib[[i]], env)

  	result[i] <- with(env,
  	  switch(attr(paper, "bibtype"),
  	    Article = formatArticle(paper),
  	    Book = formatBook(paper),
        MVBook = formatBook(paper, collection = FALSE),
  	    InBook = formatInBook(paper),
        BookInBook = formatInBook(paper, TRUE),
        SuppBook = formatInBook(paper),
        Booklet = formatBooklet(paper),
        Collection = formatBook(paper, TRUE),
        MVCollection = formatBook(paper, collection = TRUE),
  	    InCollection = formatInCollection(paper),
        SuppCollection = formatInCollection(paper),
        Manual = formatManual(paper),
  	    Misc = formatMisc(paper),
        Online = formatOnline(paper),
        Patent = formatPatent(paper),
        Periodical = formatPeriodical(paper),
        SuppPeriodical = formatArticle(paper),
        Proceedings = formatProceedings(paper),
        MVProceedings = formatProceedings(paper),
        InProceedings = formatInProceedings(paper),
        Reference = formatBook(paper, TRUE),  # alias for collection
        MVReference = formatBook(paper, collection = TRUE),
        InReference = formatInCollection(paper),
        Report = formatReport(paper),
        Thesis = formatThesis(paper),
  	    Unpublished = formatUnpublished(paper),
        Set = paste0('Set: ', attr(paper, 'key')),
        XData = paste0('XData: ', attr(paper, 'key')),
# Aliases
        TechReport = {
                      typ <-  if (is.null(paper$type))
                               "techreport"
                              else NULL
                      formatReport(paper, typ)
                      },
        PhdThesis = {
                     typ <-  if (is.null(paper$type))
                               "phdthesis"
                             else NULL
                     formatThesis(paper, typ)
                    },
        MastersThesis = {
                         typ <-  if (is.null(paper$type))
                                   "mathesis"
                                 else NULL
                         formatThesis(paper, typ)
                        },
        Www = formatOnline(paper),
        Electronic = formatOnline(paper),
        Conference = formatInProceedings(paper),
  	    paste("bibtype", attr(paper, "bibtype"), "not implemented") ))
  }
  result
}
