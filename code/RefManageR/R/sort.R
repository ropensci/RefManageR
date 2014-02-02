#' @param sorting - sort method to use, see \bold{Details}.
#' @param ... - internal use only
#' @details Refer the BibLaTeX manual Sections 3.1.2.1 and 3.5 and Appendix C.2 for more information.
#' @references Lehman, Philipp and Kime, Philip and Boruvka, Audrey and Wright, J. (2013). The biblatex Package}.
#' \url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @seealso \code{\link{BibEntry}}, \code{\link{print.BibEntry}}, \code{\link{order}}
sort.BibEntry <- function (x, decreasing = FALSE, .bibstyle = .BibOptions$bib.style, 
                           sorting = .BibOptions$sorting, drop = FALSE, ...){
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
  