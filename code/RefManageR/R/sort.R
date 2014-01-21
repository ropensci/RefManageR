sort.BibEntry <- function (x, decreasing = FALSE, .bibstyle = .BibOptions$bib.style, 
                           sorting = .BibOptions$sorting, drop = FALSE, ...){
  if (sorting == 'none')
    return(x)
  if (sorting == 'debug' || .bibstyle == 'draft')
    return(x[order(names(x))])
  if (tolower(.bibstyle) %in% c('biblatex', 'alphabetic', 'numeric', 'authoryear', 'authortitle')){    
    aut <- tools::bibstyle('BibLaTeX')$sortKeys(x)
    yr <- tools::bibstyle('BibLaTeX')$sortKeysY(x)    
    ps <- tools::bibstyle('BibLaTeX')$sortKeysPS(x)
    ttl <- tools::bibstyle('BibLaTeX')$sortKeysT(x)
    if (sorting %in% c('nyvt', 'anyvt'))
      vol <- tools::bibstyle('BibLaTeX')$sortKeysV(x)
    if (.bibstyle == 'alphabetic' || sorting == 'anyt' || sorting == 'anyvt'){
      browser()
      alabs <- tools::bibstyle('BibLaTeX')$sortKeysLA(x, yr)
      alabs <- paste0(alabs, unlist(lapply(rle(alabs[rank(alabs, ties.method = 'min')])$len, 
                       function(x){
                         if (x == 1)
                           ''
                         else letters[seq_len(x)]
                       }))) 
    }  
    ord <- switch(sorting, nyt = order(ps, aut, yr, ttl, decreasing = decreasing),
              nyvt = order(ps, aut, yr, vol, ttl, decreasing = decreasing),
              anyt = order(ps, alabs, aut, yr, ttl, decreasing = decreasing),   
              anyvt = order(ps, alabs, aut, yr, vol, ttl, decreasing = decreasing),    
              ynt = order(ps, yr, aut, ttl, decreasing = decreasing),
              ydnt = order(ps, rev(yr), aut, ttl, decreasing = decreasing),
              order(ps, aut, ttl, yr, decreasing = decreasing))  # DEFAULT = nty
    x <- x[ord]

    if (hasArg(return.ind)){  
      if (.bibstyle == 'alphabetic'){
        alabs <- alabs[ord]
      }else if (.bibstyle == 'authoryear'){
      #  browser()
        tmp <- tools::bibstyle('authoryear')$GetLastNames(x)
        tmp <- paste0(tmp, yr[ord])
        alabs <- unlist(lapply(rle(tmp[rank(tmp, ties.method = 'min')])$len, 
                               function(x){
                                 if (x == 1)
                                   ''
                                 else letters[seq_len(x)]
                               })) 
      }
      x$.index <- switch(.bibstyle, numeric = seq_along(x), alphabetic = alabs, authoryear = alabs, NULL)  
      x$.duplicated <- duplicated(aut[ord])
    }
    x
  }else{
    x[order(tools::bibstyle(.bibstyle)$sortKeys(x), decreasing = decreasing), 
      drop = drop]
  }
}
  