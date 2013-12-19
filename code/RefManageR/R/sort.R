sort.BibEntry <- function (x, decreasing = FALSE, .bibstyle = .BibOptions$bib.style, 
                           sorting = .BibOptions$sorting, drop = FALSE, ...){
  if (sorting == 'none')
    return(x)
  if (sorting == 'debug' || .bibstyle == 'draft')
    return(x[order(names(x))])
 # browser()
  if (tolower(.bibstyle) %in% c('biblatex', 'alphabetic', 'numeric', 'authoryear', 'authortitle')){    
    aut <- tools::bibstyle('BibLaTeX')$sortKeys(x)
    yr <- tools::bibstyle('BibLaTeX')$sortKeysY(x)    
    ps <- tools::bibstyle('BibLaTeX')$sortKeysPS(x)
    ttl <- tools::bibstyle('BibLaTeX')$sortKeysT(x)
    if (sorting %in% c('nyvt', 'anyvt'))
      vol <- tools::bibstyle('BibLaTeX')$sortKeysV(x)
    if (.bibstyle =='alphabetic' || sorting == 'anyt' || sorting == 'anyvt'){
      alabs <- tools::bibstyle('BibLaTeX')$sortKeysLA(x, aut, yr)
      alabs <- paste0(alabs, unlist(lapply(rle(rank(alabs, ties.method = 'min'))$len, 
                       function(x){
                         if (x == 1)
                           ''
                         else letters[seq_len(x)]
                       }))) 
    }
    x <- switch(sorting, nyt = x[order(aut, yr, ttl, decreasing = decreasing)],
                nyvt = x[order(aut, yr, vol, ttl, decreasing = decreasing)],
                anyt = x[order(alabs, aut, yr, ttl, decreasing = decreasing)],
                anyvt = x[order(alabs, aut, yr, vol, ttl, decreasing = decreasing)],
                ynt = x[order(yr, aut, ttl, decreasing = decreasing)],
                ydnt = x[order(yr, aut, ttl, decreasing = decreasing)],
                x[order(aut, ttl, yr, decreasing = decreasing)])  # DEFAULT = nty

    if (hasArg(return.ind)){  
      if (.bibstyle == 'authoryear')
        alabs <- unlist(lapply(rle(rank(paste0(aut, yr), ties.method = 'min'))$len, 
                               function(x){
                                 if (x == 1)
                                   ''
                                 else letters[seq_len(x)]
                               })) 
      x$.index <- switch(.bibstyle, numeric = seq_along(x), alphabetic = alabs, authoryear = alabs, NULL)  
    }
    x
  }else{
    x[order(tools::bibstyle(.bibstyle)$sortKeys(x), decreasing = decreasing), 
      drop = drop]
  }
}

MakeAlphaLabel <- function(author, year){
  
}

