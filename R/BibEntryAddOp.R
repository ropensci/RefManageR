#' Merge two BibEntry objects while discarding duplicates
#'
#' Merges two BibEntry objects comparing only the specified fields to detect duplicates, thus it is can be made less strict
#' than using \code{duplicated}, \code{unique}, etc.  Attributes are also merged and keys are ensured to be unique.
#' \code{merge} and \code{+} simply provide different interfaces for merging.
#'
#' @param e1 BibEntry object
#' @param e2 BibEntry object to be merged with e1
#' @param fields.to.check character vector; which BibLaTeX fields should be checked to determine if an entry
#' is a duplicate?  Can include \code{"bibtype"} to check entry type and \code{"key"} to check entry keys.
#' Specifying \code{"all"} checks all fields using \code{\link{duplicated}}.
#' @return an object of class BibEntry
#' @family operators
#' @seealso \code{\link{duplicated}}, \code{\link{unique}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @rdname merge.BibEntry
#' @method + BibEntry
#' @export
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' bib1 <- bib[seq_len(44)]
#' bib2 <- bib[45:length(bib)]
#'
#' ## The following is FALSE because the parent entry of one entry in bib1 is in bib2,
#' ##   so the child entry in is expanded in the BibEntry object returned by `[`
#' ##   to include the fields inherited from the dropped parent
#' identical(merge(bib1, bib2, 'all'), bib)
#' toBiblatex(bib1[[1L]])
#' toBiblatex(bib[[1L]])
#'
#' ## Alternatively, the operator `[[` for BibEntry objects does not expand
#' ##   cross references
#' bib1 <- bib[[seq_len(44)]]
#' bib2 <- bib[[45:length(bib)]]
#' identical(merge(bib1, bib2, 'all'), bib)
#'
#' ## Not strict enough
#' invisible(merge(bib1, bib2, c('title', 'date')))
#'
#' ## New publications of R.J. Carroll from Google Scholar and Crossref
#' \dontrun{
#' bib1 <- ReadGS(scholar.id = "CJOHNoQAAAAJ", limit = '10', sort.by.date = TRUE)
#' bib2 <- ReadCrossRef(query = "rj carroll", limit = 10, sort = "relevance",
#'   min.relevance = 80)
#' oldopt <- BibOptions(merge.fields.to.check = "title")
#' rjc.new.pubs <- bib1 + bib2
#' BibOptions(oldopt)
#' }
#' @keywords methods
`+.BibEntry` <- function(e1, e2){
  fields.to.check = .BibOptions$merge.fields.to.check
  awl <- "all" %in% fields.to.check
  att1 <- attributes(e1)[bibentry_list_attribute_names]
  att2 <- attributes(e2)[bibentry_list_attribute_names]

  if (length(fields.to.check) && !awl){
    possible.dup <- seq_along(e2)
    temp <- c("key", "bibtype") %in% fields.to.check
    if (all(temp)){
      possible.dup <- which(sapply(e2,
                                    function(x, y){
                                      ykeys <- unlist(y$key)
                                      ytypes <- unlist(y$bibtype)
                                      xkey <- x$key
                                      xtype <- x$bibtype
                                      for (i in seq_along(y)){
                                        if (ykeys[i] == xkey && ytypes[i] == xtype)
                                          return(TRUE)
                                      }
                                      return(FALSE)
                                    }, y = e1))
    }else if (temp[1L]){
      possible.dup <- which(names(e2) %in% names(e1))
    }else if (temp[2L]){
      possible.dup <- unlist(e2$bibtype) %in% unlist(e1$bibtype)
    }
    remain.dup.f <- setdiff(fields.to.check, c('bibtype', 'key'))
    if (length(remain.dup.f)){
      dup.ind <- sapply(unclass(e2[possible.dup]),
                      function(x, y, flds){
                        x <- x[flds]
                        for (i in seq_along(y)){
                          if (identical(y[[i]][flds], x))
                            return(TRUE)
                        }
                        return(FALSE)
                    }, y = unclass(e1), flds = remain.dup.f)
      if (length(dup.ind))
          dup.ind <- which(dup.ind)
    }else{
      dup.ind <- possible.dup
    }
    if (length(dup.ind) == length(e2)){
      message('Only duplicates in second BibEntry object')
      return(e1)
    }
    if (length(dup.ind)){
      e2 <- e2[[-dup.ind]]
      message(paste0('Duplicate entries found in second BibEntry object in position(s): ',
                     paste0(dup.ind, collapse = ', ')))
    }
  }
  x <- c(e1, e2)
  if (awl){
    x <- x[[!duplicated(x)]]
  }
  x <- MakeKeysUnique(x)
  attributes(x)[bibentry_list_attribute_names] <- mapply(function(x, y){
    res <- c(x, y)
    res[!duplicated(res)]
  } , att1, att2)

  return(x)
}

#' @param x BibEntry object
#' @param y BibEntry object
#' @param ... ignored
#' @aliases +.BibEntry
#' @method merge BibEntry
#' @export
#' @rdname merge.BibEntry
merge.BibEntry <- function(x, y, fields.to.check = BibOptions()$merge.fields.to.check, ...){
  oldfields <- BibOptions(merge.fields.to.check = fields.to.check)
  on.exit(BibOptions(oldfields))
  `+.BibEntry`(x, y)
}

