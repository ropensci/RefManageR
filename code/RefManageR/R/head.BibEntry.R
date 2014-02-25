#' Return the first or last part of a BibEntry object
#' 
#' Prints the first or last entries of a BibEntry object (via \code{\link{message}}) and returns them \emph{invisibly} 
#'   (via \code{\link{invisible}}).
#'  
#' @param x an object of class BibEntry.
#' @param n a single integer. If positive, size for the resulting object: number of elements for a vector 
#'   (including lists), rows for a matrix or data frame or lines for a function. If negative, all but the 
#'   n last/first number of elements of x.
#' @param suppress.messages boolean; should the head/tail entries be printed via \code{\link{message}}?   
#' @param ... arguments to be passed to or from other methods.   
#' @details If \code{suppress.messages} is \code{FALSE}, the head/tail entries are output to the console along
#'   with some additional formatting for the \sQuote{bibtype} and \sQuote{key}, in addition to invisibling
#'   returning the entries.
#' @return an object of class BibEntry.  
#' @importFrom utils head tail
#' @rdname head.BibEntry
#' @aliases tail.BibEntry
#' @method head BibEntry
#' @export 
head.BibEntry <- function (x, n = 6L, suppress.messages = TRUE, ...){
    stopifnot(length(n) == 1L)
    
    n <- if (n < 0L) 
        max(length(x) + n, 0L)
    else min(n, length(x))

    ind <- seq_len(n)
    bibtype <- unlist(x$bibtype)
    key <- unlist(x$key)

    if (!suppress.messages)
    message(paste0(sapply(ind, function(i){
                    paste(paste0('[[', i, ']] ', bibtype[i], ': ', key[i]),
                      format(x[[i]]), sep = '\n')
                      }), collapse='\n\n'))

    invisible(x[ind])
}

#' @rdname head.BibEntry
#' @method tail BibEntry
#' @export
tail.BibEntry <- function (x, n = 6L, suppress.messages = TRUE, ...){
    stopifnot(length(n) == 1L)
    
    lenx <- length(x)
    n <- if (n < 0L) 
        max(lenx + n, 0L)
    else min(n, lenx)
    
    ind <- seq.int(to = lenx, length.out = n)
    bibtype <- unlist(x$bibtype)
    key <- unlist(x$key)
    if (!suppress.messages)
      message(paste0(sapply(ind, function(i){
                    paste(paste0('[[', i, ']] ', bibtype[i], ': ', key[i]),
                      format(x[[i]]), sep = '\n')
                      }), collapse='\n\n'))

    invisible(x[ind])
}