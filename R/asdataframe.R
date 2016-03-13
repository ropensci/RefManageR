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
  n.entries <- length(x)
  y <- as.data.frame(matrix(nrow = n.entries, ncol = n.fields + 1L),
                     stringsAsFactors = FALSE,
                     row.names = make.unique(names(x), sep = "-"))
  colnames(y) <- c('bibtype', col.names)
       
  y[, 1L] <- unlist(x$bibtype)
  not.nulls <- 1L
  for (i in seq_len(n.fields)){
    nom <- col.names[i]
    temp <- do.call(`$.BibEntry`, list(x = x, name = nom))
    if (n.entries > 1L)
      not.nulls <- !sapply(temp, is.null)

    if (nom %in% .BibEntryNameList){
      temp <- sapply(temp[not.nulls], format_author)
    }else{
      temp <- unlist(temp)
    }

    y[not.nulls, nom] <- temp
  }
  
  return(y)
}
