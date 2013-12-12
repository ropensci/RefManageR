#' Names (keys) of a BibEntry object
#' 
#' Functions to get and set the keys of an object of class BibEntry
#' @aliases names `names<-` names.BibEntry `names<-.BibEntry`
#' @method names BibEntry
#' @author McLean, M. W. \url{mathew.w.mclean@gmail.com}
#' @param x an object of class BibEntry
names.BibEntry <- function(x){
  return(unlist(x$key))  # return(sapply(unclass(x), function(x) return(attr(x, 'key'))))
}

#`names<-.BibEntry` <- function(x, value){
#    res <- `names<-`(unclass(x), value) 
#    class(res) <- c('BibEntry', 'bibentry')
#}

