#' Sort a BibEntry Object
#' 
#' Sorts a \code{BibEntry} object by specified fields.  The possible fields used for sorting and
#' the order they are used in correspond with the options avaiable in BibLaTeX.
#' 
#' @param x - an object of class BibEntry
#' @param decreasing - logical; should the sort be increasing or decreasing?
#' @param sorting - sort method to use, see \bold{Details}.
#' @param .bibstyle - bibliography style; used when \code{sort} is called by \code{\link{print.BibEntry}} 
#' @param ... - internal use only
#' @return the sorted BibEntry object
#' @method sort BibEntry
#' @export
#' @keywords manip methods
#' @details The possible values for argument \code{sorting} are
#' \itemize{
#' \item nty - sort by name, then by title, then by year
#' \item nyt - sort by name, then by year, then title
#' \item nyvt - sort by name, year, volume, title
#' \item anyt - sort by alphabetic label, name, year, title
#' \item anyvt - sort by alphabetic label, name, year, volume, title
#' \item ynt - sort by year, name, title
#' \item ydnt - sort by year (descending), name, title
#' \item debug - sort by keys
#' \item none - no sorting is performed
#' }
#' 
#' All sorting methods first consider the field presort, if available.  Entries with no presort field are assigned presort 
#' value \dQuote{mm}. Next the sortkey field is used.
#' 
#' When sorting by name, the sortname field is used first.  If it is not present, the author field is used, 
#' if that is not present editor is used, and if that is not present translator is used.  All of these fields are affected
#' by the value of \code{max.names} in .BibOptions()$max.names.
#' 
#' When sorting by title, first the field sorttitle is considered.  Similarly, when sorting by year, the field sortyear is 
#' first considered.
#' 
#' When sorting by volume, if the field is present it is padded to four digits with leading zeros; otherwise, 
#' the string \dQuote{0000} is used.
#' 
#' When sorting by alphabetic label, the labels that would be generating with the \dQuote{alphabetic} bibstyle are used.
#' First the shorthand field is considered, then label, then shortauthor, shorteditor, author, editor, and translator.
#' Refer to the BibLaTeX manual Sections 3.1.2.1 and 3.5 and Appendix C.2 for more information.
#' @references Lehman, Philipp and Kime, Philip and Boruvka, Audrey and Wright, J. (2013). The biblatex Package. \url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @seealso \code{\link{BibEntry}}, \code{\link{print.BibEntry}}, \code{\link{order}}
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name)[[70:73]])
#' BibOptions(sorting = "none")
#' bib
#' sort(bib, sorting = "nyt")
#' sort(bib, sorting = "ynt")
#' BibOptions(restore.defaults = TRUE)
sort.BibEntry <- function (x, decreasing = FALSE, .bibstyle = BibOptions()$bib.style, 
                           sorting = BibOptions()$sorting, ...){
  # if (sorting == 'none' && .bibstyle != "alphabetic" c("numeric", ""))
  #  return(x)
  if (sorting == 'debug' || .bibstyle == 'draft')
    return(x[order(names(x))])
  #if (tolower(.bibstyle) %in% c('biblatex', 'alphabetic', 'numeric', 'authoryear', 'authortitle')){    
  if (sorting != "none"  || .bibstyle == "alphabetic"){
    aut <- tools::bibstyle('BibLaTeX')$sortKeys(x)
    yr <- tools::bibstyle('BibLaTeX')$sortKeysY(x)    
    ps <- tools::bibstyle('BibLaTeX')$sortKeysPS(x)
    ttl <- tools::bibstyle('BibLaTeX')$sortKeysT(x)
    if (sorting %in% c('nyvt', 'anyvt'))
      vol <- tools::bibstyle('BibLaTeX')$sortKeysV(x)
  }
  if (.bibstyle == 'alphabetic' || sorting == 'anyt' || sorting == 'anyvt'){
    #browser()
    alabs <- tools::bibstyle('BibLaTeX')$sortKeysLA(x, yr)
    alabs <- paste0(alabs, unlist(lapply(rle(alabs[rank(alabs, ties.method = 'min')])$len, 
                     function(x){
                       if (x == 1)
                         ''
                       else letters[seq_len(x)]
                     }))) 
  }  
  if (sorting != "none"){
    ord <- switch(sorting, nyt = order(ps, aut, yr, ttl, decreasing = decreasing),
              nyvt = order(ps, aut, yr, vol, ttl, decreasing = decreasing),
              anyt = order(ps, alabs, aut, yr, ttl, decreasing = decreasing),   
              anyvt = order(ps, alabs, aut, yr, vol, ttl, decreasing = decreasing),    
              ynt = order(ps, yr, aut, ttl, decreasing = decreasing),
              ydnt = order(ps, rev(yr), aut, ttl, decreasing = decreasing),
              order(ps, aut, ttl, yr, decreasing = decreasing))  # DEFAULT = nty
    x <- x[ord]
    aut <- aut[ord]
    if (.bibstyle == "alphabetic")
      alabs <- alabs[ord]
  }
  # create labels if needed
  if (hasArg(return.ind)){  
    if (.bibstyle %in% c("authoryear", "authortitle")){
    #  browser()
      if (sorting == "none")
        aut <- tools::bibstyle('BibLaTeX')$sortKeys(x)
      x$.duplicated <- duplicated(aut)
      if (.bibstyle == "authoryear"){
        tmp <- tools::bibstyle('authoryear')$GetLastNames(x)
        if (sorting == "none"){
          yr <- tools::bibstyle('BibLaTeX')$sortKeysY(x)      
        }else{
          yr <- yr[ord]
        }
        tmp <- paste0(tmp, yr)
        alabs <- unlist(lapply(rle(tmp[rank(tmp, ties.method = 'min')])$len, 
                               function(x){
                                 if (x == 1)
                                   ''
                                 else letters[seq_len(x)]
                               })) 
      }
    }
    x$.index <- switch(.bibstyle, numeric = {
      ind <- which(!unlist(x$bibtype) %in% c('Set', 'XData'))
      index <- numeric(length(x))
      index[ind] <- seq_along(ind)
      index
      }, alphabetic = alabs, authoryear = alabs, NULL)  
  }
  x
#   }else{
#     x[order(tools::bibstyle(.bibstyle)$sortKeys(x), decreasing = decreasing), 
#       drop = drop]
#   }
}
  