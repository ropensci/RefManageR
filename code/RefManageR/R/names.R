#' Names (keys) of a BibEntry object
#' 
#' Functions to get and set the keys of an object of class BibEntry
#' @aliases names `names<-` names.BibEntry `names<-.BibEntry`
#' @method names BibEntry
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @param x an object of class BibEntry
#' @return vector containing the keys on all entries in \code{x}.
#' @keywords attribute
#' @export
#' @examples
#' bib <- ReadBib(system.file("sampleData", "test.bib", package = "RefManageR"))
#' names(bib)
#' names(bib)[1] <- 'newkey'
names.BibEntry <- function(x){
  return(unlist(x$key))  # return(sapply(unclass(x), function(x) return(attr(x, 'key'))))
}

#`names<-.BibEntry` <- function(x, value){
#    res <- `names<-`(unclass(x), value) 
#    class(res) <- c('BibEntry', 'bibentry')
#}

