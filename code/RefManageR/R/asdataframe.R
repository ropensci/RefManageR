#' Coerce to a Data Frame
#' 
#' Coerces a BibEntry object to a data.frame, with each row of the data frame being a field present in at least one
#' entry in the BibEntry object being coerced.
#' @param x - a BibEntry object
#' @param row.names - ignored
#' @param optional - ignored
#' @param ... - ignored
#' @method as.data.frame BibEntry
#' @export
#' @return a data.frame object with row names giving the keys, and first column giving entry type.
#' @seealso \code{\link{BibEntry}}, \code{\link{as.BibEntry}}
#' @examples 
#' bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article", 
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"), 
#'   c(bibtype = "article", key = "mclean2014b", volume = 10, title = "My Newer Article", 
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))       
#' bib <- as.BibEntry(bib)   
#' as.data.frame(bib)
as.data.frame.BibEntry <- function(x, row.names = NULL, optional = FALSE, ...){
  col.names <- unique(unlist(fields(x)))
  n.fields <- length(col.names)
  y <- matrix(nrow = length(x), ncol = n.fields + 1L)
  colnames(y) <- c('bibtype', col.names)
  rownames(y) <- names(x)
#   browser()
#   authors <- x$author
#   names(authors) <- names(x)
#   authors <- sapply(authors, paste0, collapse =', ')
  #authors <- setNames(sapply(x, function(z) paste0(z$author, collapse = ', '), USE.NAMES = TRUE), rownames(y))
       
  y[, 1L] <- unlist(x$bibtype)
  for (i in seq_len(n.fields)){
    nom <- col.names[i]
    temp <- do.call(`$.BibEntry`, list(x = x, name = nom))
    not.nulls <- !sapply(temp, is.null)
    noms <- names(temp[not.nulls])
    if (nom %in% .BibEntryNameList){
      temp <- sapply(temp[not.nulls], format_author)
    }else{
      temp <- unlist(temp)
    }
#     if (nom != .BibEntryNameList){
#       temp <- x[nom]
#     }else{
#       temp <- do.call(`$`, list(x = x, name = nom))  # eval(parse(text=paste0('x$', nom)))
#       temp <- sapply(setNames(temp, rownames(y)), function(z) paste0(z, collapse = ', '))
#     }
    y[noms, nom] <- temp
  }
  
  y <- as.data.frame(y, stringsAsFactors = FALSE)
  return(y)
}