# Mathew McLean
# January 15, 2014
# Define addition operator for objects of BibEntry class
# Note: will not remove duplicates entries already present in e1 or e2

#' Merge two BibEntry objects while discarding duplicates
#' 
#' Merges two BibEntry objects comparing only the specified fields to detect duplicates, thus it is can be made less strict
#' than using \code{duplicated}, \code{unique}, etc.  Attributes are also merged and keys are ensured to be unique.#' 
#' 
#' @param e1 - BibEntry object
#' @param e2 - BibEntry object to be merged with e1
#' @param fields.to.check - character vector; which BibLaTeX fields should be checked to determine if an entry 
#' is a duplicate?  Can include \code{\dQuote{bibtype}} to check entry type and \code{\dQuote{key}} to check entry keys.  
#' Specifying \code{\dQuote{all}} checks all fields.
#' @return an object of class BibEntry
#' @seealso \code{\link{BibEntry}}, \code{\link{duplicated}}, \code{\link{unique}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @aliases merge.BibEntry
#' @S3method BibEntry
#' @note 
#' @examples
#' file.name <- system.file("sampleData", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' bib1 <- bib[seq_len(44)]
#' bib2 <- bib[seq_len(45:length(bib))]
#' 
#' ## The following is FALSE because the parent entry of one bib1 entry is in bib2, 
#' ##   so the child entry in is expanded in the BibEntry object returned by \code{`[`} 
#' ##   to include the fields inherited from the dropped parent
#' identical(merge(bib1, bib2, 'all'), bib)
#' toBiblatex(bib1[[1L]])
#' toBiblatex(bib[[1L]])
#' 
#' ## Not strict enough
#' length(merge(bib1, bib2, c('title', 'date'))
#'
#' ## New publications of R.J. Carroll from Google Scholar and Crossref 
#' bib1 <- ReadGS(scholar.id = "CJOHNoQAAAAJ", limit = '10', sort.by.date = TRUE)
#' bib2 <- ReadCrossRef(query = "rj carroll", limit = 10, sort = "relevance", min.relevance = 80)
#' .BibOptions$merge.fields.to.check <- "title"
#' rjc.new.pubs <- bib1 + bib2
#' @keywords methods
`+.BibEntry` <- function(e1, e2, fields.to.check = .BibOptions()$merge.fields.to.check){
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
      dup.ind <- which(sapply(unclass(e2[possible.dup]), 
                      function(x, y, flds){
                        x <- x[flds]
                        for (i in seq_along(y)){
                          if (identical(y[[i]][flds], x))
                            return(TRUE)
                        }
                        return(FALSE)
                      }, y = unclass(e1), flds = remain.dup.f))
    }else{
      dup.ind <- possible.dup
    }
    if (length(dup.ind) == length(e2)){
      message('Only duplicates in second BibEntry object')
      return(e1)
    }
    if (length(dup.ind)){
      e2 <- e2[-dup.ind]
      message(paste0('Duplicate entries found in second BibEntry object in position(s): ', 
                     paste0(dup.ind, collapse = ', ')))
    }
  }
  x <- c(e1, e2)
  if (awl){
    x <- x[!duplicated(x)]  
  }
  names(x) <- make.unique(names(x), sep = ":")
  attributes(x)[bibentry_list_attribute_names] <- mapply(function(x, y){
    res <- c(x, y)
    res[!duplicated(res)]
  } , att1, att2)

  return(x)
}

#' Merge two BibEntry objects while discarding duplicates
#' 
#' Merges two BibEntry objects comparing only the specified fields to detect duplicates, thus it is can be made less strict
#' than using \code{duplicated}, \code{unique}, etc.  Attributes are also merged and keys are ensured to be unique.
#' 
#' @inheritParams [.BibEntry
#' @aliases [.BibEntry
#' @S3method BibEntry
merge.BibEntry <- function(x, y, fields.to.check = .BibOptions$merge.fields.to.check, ...)
  UseMethod('+')