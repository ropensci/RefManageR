tools::bibstyle('BibLaTeX', default = TRUE, envir = local({

##################################################################
## Formatting functions

fmtPrefix <- function(paper){
  switch(.BibOptions$bib.style, numeric = paste0("[", fmtNumPre(paper), "]"),
         alphabetic = paste0("[", paper$.index, "]"),
         draft = paste0('\\bold{', attr(paper, 'key'), '}'),
         NULL)
#   if (.BibOptions$bib.style == 'numeric'){
#     paste0("[", paper$.index, "]")
#   }else if()
#   
#   }else{
#     NULL
#   }
}

fmtNumPre <- function(doc){
  res <- doc$shorthand
  if (!length(res))
    res <- doc$.index
  res
}

cleanupLatex <- function (x){
    if (!length(x)) 
        return(x)
    latex <- try(tools::parseLatex(x), silent = TRUE)
    if (inherits(latex, "try-error")) {
        x
    }
    else {
        tools::deparseLatex(tools::latexToUtf8(latex), dropBraces = TRUE)
    }
}

collapse <- function(strings){ 
  paste(strings, collapse = "\n")
}

labelclean <- function(prefix = NULL, suffix = NULL, style = plain){
    f <- label(prefix, suffix, style)
    function(s) f(cleanupLatex(s))
}

label <- function(prefix = NULL, suffix = NULL, style = plain){
    force(prefix)
    force(suffix)
    force(style)
    function(s) if (length(s)) 
        style(paste0(prefix, collapse(s), suffix))
}

labelPersons <- function(prefix = NULL, suffix = NULL, style = plain){
    force(prefix)
    force(suffix)
    force(style)
    function(s) if (length(s)) 
        style(paste0(prefix, authorList(s), suffix))
}


emphclean <- function (s){
  emph(cleanupLatex(s))
}

emphcleanap <- function (s){
  emph(addPeriod(cleanupLatex(s)))
}

emph <- function (s){
  if (length(s)) 
    paste0("\\emph{", collapse(s), "}")
}

plainclean <- function (s){
  plain(cleanupLatex(s))
}

cleanap <- function(s){
  if (length(s))
    cleanupLatex(addPeriod(s))
}

plain <- function (pages){
  if (length(pages)) 
    collapse(pages)
}

sentence <- function (..., sep = ", "){
    strings <- c(...)
    if (length(strings)) {
        addPeriod(paste(strings, collapse = sep))
    }
}

addPeriod <- function (string){ 
  sub("([^.?!])$", "\\1.", string)
}

addPeriodTitle <- function (string){ 
  sub("([^.?!])$", "\\dQuote{\\1.}", string)
}

# authorList <- function(paper){
#     names <- sapply(paper$author, shortName)
#     if (length(names) > 1) 
#         result <- paste(paste(names[-length(names)], collapse = ", "), 
#             "and", names[length(names)])
#     else result <- names
#     result
# }

authorList <- function(aut){
 # browser()
    names <- sapply(aut, shortName)
    if (length(names) > 1){ 
        result <- paste(paste(names[-length(names)], collapse = ", "), 
            "and", names[length(names)])
    }else{
      result <- names
    }
    result
}

fmtSingleEditor <- function(nom, job, prefix = NULL, suffix = '.'){
  if (length(nom)){
    if(length(job)){
      res <- paste0(switch(tolower(job), 'compiler' = 'Comp. by ', 'editor' = 'Ed. by ', 
                    'founder' = 'Found. by ', 'continuator' = 'Cont. by ', 'redactor' = 'Red. by ', 
                    'reviser' = 'Rev. by ', 'collaborator' = 'In collab. with ', job), nom)
    }else{
      res <- paste0('Ed. by ', nom)
    }
    paste0(prefix, res, suffix)
  }
}

shortNameLF <- function(pers){
    if (length(pers$family)) {
        res <- cleanupLatex(pers$family)
        if (length(pers$given)){ 
          if (.BibOptions$abbrev.names){
            paste0(paste(res, paste(substr(sapply(pers$given, cleanupLatex), 
                1, 1), collapse = ". "), sep=', '), '.')
          }else{
            paste(res, paste(sapply(pers$given, cleanupLatex), collapse = ' '), sep = ', ')
          }
        }else{
          res
        }
    }else{
      paste(cleanupLatex(pers$given), collapse = " ")
    }
}

shortName <- function(pers){
    if (length(pers$family)) {
        res <- cleanupLatex(pers$family)
        if (length(pers$given)){ 
          if (.BibOptions$abbrev.names){
            paste0(c(substr(sapply(pers$given, cleanupLatex), 
                1, 1), res), collapse = ". ")
          }else{
            cleanupLatex(as.character(pers))
          }
        }else{
          res
        }
    }else{
      paste(cleanupLatex(pers$given), collapse = " ")
    }
}

DateFormatter <- function(dat, other = FALSE){
  if (!is.null(dat)){
    if (other){
      fmt <- switch(as.character(attr(dat, 'day.mon')), '2' = '%m/%d/%Y', '1' ='%m/%Y', '0' ='%Y')
    }else{
      fmt <- switch(as.character(attr(dat, 'day.mon')), '2' = '%b. %d, %Y', '1' = '%b. %Y', '0' = '%Y')
    }
    if (is.interval(dat)){
      begind <- int_start(dat)
      endd <- int_end(dat)
      paste(format(begind, fmt), format(endd, fmt), sep ='-')
    }else{
      format(dat, fmt)
    }
  }
}

sortKeys <- function(bib){
#  browser()
    result <- character(length(bib))
    for (i in seq_along(bib)) {
      authors <- bib[[i]]$sortname
      if (!length(authors))
        authors <- paste0(sapply(bib[[i]]$author, shortNameLF), collapse = '')
      if (authors == ''){ 
        authors <- paste0(sapply(bib[[i]]$editor, shortNameLF), collapse = '')
        if (authors == '')
          authors <- paste0(sapply(bib[[i]]$translator, shortNameLF), collapse = '')
#         if (!length(authors)) 
#           authors <- ""
      }
      result[i] <- authors
    }
    result
}  

sortKeysY <- function(bib){ 
    result <- character(length(bib))
    bib <- unclass(bib)
    for (i in seq_along(bib)) {
      res <- bib[[i]]$sortyear
      if (!length(res)){
        res <- attr(bib[[i]], 'dateobj')
        if (inherits(res, 'Interval')){
          res <- year(int_start(res))
        }else{
          res <- year(res)
        }
      }
      result[i] <- res
    }
    result
} 

sortKeysPS <- function(bib){
    result <- character(length(bib))
    for (i in seq_along(bib)) {
        res <- bib[[i]]$presort
        if (!length(res)){ 
          res <- bib[[i]]$sortkey
          if (!length(res)) 
            res <- "mm"
        }
        result[i] <- res
    }
    result
}

sortKeysLA <- function(bib, yrs){
    result <- character(length(bib))
    for (i in seq_along(bib)) {
        res <- bib[[i]]$shorthand
        if (!length(res)){ 
          res <- bib[[i]]$label
          if (!length(res)) 
            res <- ProcessNamesLA(bib[[i]]$shortauthor)
          if (!length(res)) 
            res <- ProcessNamesLA(bib[[i]]$shorteditor)
          if (!length(res)) 
            res <- ProcessNamesLA(bib[[i]]$author)
          if (!length(res)) 
            res <- ProcessNamesLA(bib[[i]]$editor)
          if (!length(res)) 
            res <- ProcessNamesLA(bib[[i]]$translator)          
          res <- paste0(res, substr(yrs[i], 3, 4))
        }
        result[i] <- res
    }
    result
}

ProcessNamesLA <- function(nam){
  if (length(nam)){
    if (!inherits(nam, 'person'))
      nam <- ArrangeAuthors(nam)
    nam <- nam$family
    res <- substr(nam, start = 1, stop = 1)
    switch(as.character(length(res)), '0'= NULL, '1' = substr(nam, 1, 3), '2' = paste0(res[seq_len(2)], collapse =''),
           paste0(res[seq_len(3)], collapse =''))
  }
}

sortKeysT <- function(bib){
  result <- character(length(bib))
  for (i in seq_along(bib)){
    res <- bib[[i]]$sorttitle
    if (!length(res))
      res <- bib[[i]]$title
    if (!length(res)){ 
      res <- bib[[i]]$maintitle
      if (!length(res)) 
        res <- bib[[i]]$booktitle        
      if (!length(res)) 
        res <- ""
    }
    result[i] <- res
  }
  result
} 

sortKeysV <- function(bib){
  result <- numeric(length(bib))
  for (i in seq_along(bib)){
    res <- bib[[i]]$volume
    if (!length(res)){
      res <- '0000'
    }else{
      tmp <- try(sprintf("%04d", as.numeric('res')), TRUE)
      if (!inherits(tmp, 'try-error'))
        res <- tmp
    }
    result[i] <- res
  }
  result
} 
  
#####################################################################################
## FIELDS: 
#####################################################################################
fmtJournDate <- function(s, usevenue = FALSE){
  res <- DateFormatter(attr(s, 'dateobj'))
  if (usevenue){
    paste0("(", paste0(c(cleanupLatex(gsub('[.?!]$', '', s[['venue']]), res)), collapse = ', '), ")")
  }else{
    paste0("(", paste0(c(s[['issue']], res), collapse = ' '), ")")  
  }
  
}

fmtDate <- function(dat){
  if (length(dat))
    DateFormatter(dat)
}

# fmtDOI <- function(s){
#   if (length(s) && .BibOptions$print.doi)
#     paste0('DOI: ', s)
# }

#fmtPages <- label(prefix = ', pp. ')

# pagination.strings <- cbind(c(' p.' ' col.', ' v.', ' l.', ' \u00a7.', ' par.'),
#                             c(' pp.' ' cols.', ' vv.', ' ll.', ' \u00a7\u00a7.', ' par.'))

fmtPages <- function(pgs, pref){
  if (length(pgs)){
    if (!length(pref)){
      if (length(grep('-', pgs))){
        paste0(', p. ', pgs)
      }else{
        paste0(', pp. ', pgs)
      }
    }else{
      if (length(grep('-', pgs))){
        paste0(switch(pref, column = ', cols. ', verse = ', vv. ', line = ', ll. ', 
            section = ', \u00a7\u00a7 ', paragraph = ', par. ', none = ', ', ', pp. '), pgs)

      }else{
        paste0(switch(pref, column = ', col. ', verse = ', v. ', line = ', l. ', 
                    section = ', \u00a7 ', paragraph = ', par. ', none = ', ', ', p. '), pgs)
      }
    }
  }
}

fmtTotalPages <- function(pgs, pref){
  if (length(pgs)){
    if (!length(pref)){
      if (pgs == 1){
        paste0(pgs, ' p.')        
      }else{
        paste0(pgs, ' pp.')
      }
    }else{
      if (pgs == 1){
        paste0(pgs, switch(pref, column = ' col.', verse = ' v.', line = ' l.', 
                    section = ' \u00a7', paragraph = ' par.', none = '.', ' p.'))
      }else{
        paste0(pgs, switch(pref, column = ' cols.', verse = ' vv.', line = ' ll.', 
                    section = ' \u00a7\u00a7.', paragraph = ' par.', none = '.', ' pp.'))
      }
    }
  }
}

fmtNote <- function(note, prefix = NULL, suffix = '.'){
  if (length(note))
    paste0(prefix, cleanupLatex(note), suffix)
}

fmtJournal <- function(s){
  if (length(s$journaltitle)){
    res <- paste0('In: ', emph(cleanupLatex(s$journaltitle)))
    if (length(s$journalsubtitle))
      res <- paste(addPeriod(res), emph(cleanupLatex(s$journalsubtitle)))
    return(res)
  }else if(!is.null(s$journal)){
    paste0('In: ', emph(cleanupLatex(s$journal)))  
  }
}

fmtVolume <- function(vol, num){
  if (length(vol)){
    res <- vol
    if (length(num))
      res <- paste(vol, num, sep='.')
    res
  }
}

fmtBVolume <- function(vol, num){
  if (length(vol)){
    res <- paste0('Vol. ', vol)
    if (length(num))
      res <- paste(res, num, sep='.')
    res
  }
}

fmtVolumes <- label(suffix = ' vols.')
fmtOrganization <- label(suffix = '.')

# fmtBAuthor <- function(doc){
#   if (dup)
#     return('\u2500\u2500\u2500')
#   #browser()
#   res <- NULL
#   if (length(doc$author)){
#     res <- doc$author
#   }else if (length(doc$editor)){
#     res <- doc$editor
#   }
#   if (length(res) > 1){ 
#     res <- paste(paste(sapply(res[-length(res)], shortNameLF), collapse = ", "), 
#         "and", shortName(res[length(res)]))
#   }else{
#     res <- shortNameLF(res)
#   }
#   if (!length(doc$author) && length(doc$editor)){
#     if (!length(doc$editortype)){
#       res <- paste0(res, ', ed.')
#     }else{
#       res <- paste0(res, switch(tolower(doc$editortype), 'compiler' = ', comp.', 'editor' = ', ed.', 
#                                 'founder' = ', found.', 'continuator' = ', cont.', 
#                                 'redactor' = ', red.', 'reviser' = ', rev.',
#                    'collaborator' = ', collab.', doc$editortype))
#     }
#   }else{
#     res <- addPeriod(res)
#   }
#   res
# }

fmtBAuthor <- function(doc){
  if (dup)
    return('\u2500\u2500\u2500')
  #browser()
  out <- NULL
  if (length(doc$author)){
    res <- doc$author
  }else if (length(doc$editor)){
    res <- doc$editor
  }
  nnames <- length(res)
  if (nnames){
    if (bibstyle == 'alphabetic' || bibstyle == 'numeric'){
      out <- shortName(res[1])
    }else{
      out <- shortNameLF(res[1])
    } 
    if (nnames == 2){
      out <- paste(out, shortName(res[-1]), sep = ' and ')
    }else if (nnames > 2){
      out <- paste(paste(out, paste0(sapply(res[-c(1, length(res))], shortName), collapse = ", "), sep = ', '), 
        shortName(res[length(res)]), sep = ' and ')
    }
  }

  if (!length(doc$author) && length(doc$editor)){
    if (!length(doc$editortype)){
      out <- paste0(out, ', ed.')
    }else{
      out <- paste0(out, switch(tolower(doc$editortype), 'compiler' = ', comp.', 'editor' = ', ed.', 
                                'founder' = ', found.', 'continuator' = ', cont.', 
                                'redactor' = ', red.', 'reviser' = ', rev.',
                   'collaborator' = ', collab.', doc$editortype))
    }
  }else{
    out <- addPeriod(out)
  }
  out
}

fmtPublisher <- function(pub, loc, addr){
  if (length(loc) || length(addr)){
    res <- if (length(loc))
               loc
           else addr
    if (length(pub))
      res <- paste(res, pub, sep = ': ')
    paste0(res, ', ')
  }else if (length(pub)){
    paste0(pub, ', ')
  }
}

fmtChapter <- label(prefix = '. Chap. ')

fmtISSN <- label(prefix = 'ISSN: ', suffix = '.')
fmtISBN <- label(prefix = 'ISBN: ', suffix = '.')
fmtISRN <- label(prefix = 'ISRN: ', suffix = '.')
fmtDOI <- label(prefix = 'DOI: ', suffix = '.')

fmtIssueTitle <- function(paper){
  if (length(paper$issuetitle)){
    res <- paste0(': \\emph{', paper$issuetitle)
    if (length(paper$issuesubtitle))
      res <- paste0(res, '. ', paper$issuesubtitle)
    paste0(res, '}')
  }
}
  
fmtURL <- function(paper){
  if (length(paper$url)){
    res <- paste0('\\url{', paper$url, '}')
    if (length(paper$urldate)){
      fDate <- try(ProcessDate(paper$urldate, NULL), TRUE)
      if (!is.null(fDate) && !inherits(fDate, 'try-error'))
        res <- paste0(res, ' (visited on ', DateFormatter(fDate, TRUE), ')')
    }
    addPeriod(res)
  }
}

fmtEprint <- function(paper){
  if (length(paper$eprint)){
    if (length(paper$eprinttype)){
      res <- paste0(switch(tolower(paper$eprinttype), 'arxiv'='arXiv', 'pubmed' = 'PMID', 
                    'googlebooks' = 'Google Books', 'jstor' = 'JSTOR', 'hdl' = 'HDL', paper$eprinttype), 
                    ': ', paper$eprint)
      if (tolower(paper$eprinttype) == 'arxiv'){
        if (length(paper$eprintclass)){
          res <- paste0(res, ' [', paper$eprintclass, ']')
        }else if(length(paper$primaryclass)){
          res <- paste0(res, ' [', paper$primaryclass, ']')
        }
      }
    }else if (length(paper$archiveprefix)){
      if (length(paper$eprintclass)){
        res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [', paper$eprintclass, ']')
      }else if(length(paper$primaryclass)){
        res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [', paper$primaryclass, ']')
      }
    }else{
      res <- paste0('eprint: ', paper$eprint)
    }
    addPeriod(res)
  }
}

fmtEditor <- function(doc, editor.used.already = FALSE, prefix = NULL, suffix = '.'){
  #browser()
  res <- NULL
  if (length(doc$editor)  && !editor.used.already){
    res <- c(res, fmtSingleEditor(authorList(doc$editor), doc$editortype, prefix, suffix))
  }
  if (length(doc$editora)){
    res <- c(res, fmtSingleEditor(authorList(doc$editora), doc$editoratype, prefix, suffix))
  }
  if (length(doc$editorb)){
    res <- c(res, fmtSingleEditor(authorList(doc$editorb), doc$editorbtype, prefix, suffix))
  }
  if (length(doc$editorc)){
    res <- c(res, fmtSingleEditor(authorList(doc$editorc), doc$editorctype, prefix, suffix))
  }
  paste0(res)
}

fmtJTitle <- function(title){
  if (length(grep('[.?!]$', title))){
    paste0("\\dQuote{", collapse(cleanupLatex(title)), "}")
  }else{
    paste0("\\dQuote{", collapse(cleanupLatex(title)), "}.")
  }
}

fmtVenue <- function(venue){
  if (length(venue)){
    venue <- gsub('[.?!]$', '', venue)
    paste0("(", collapse(cleanupLatex(title)), ").")
  }
}

fmtEventTitle <- cleanap

fmtBTitle <- emphcleanap

fmtIBTitle <- function(tl, stl, bib){
  if (bib){
    paste0(c(fmtBTitle(tl), fmtBTitle(stl)))
  }else{
    fmtJTitle(paste0(c(addPeriod(tl), stl), collapse =' '))
  }
}


# fmtBtitle <- function (s){
#   if (length(s)) 
#     addPeriod(paste0("\\emph{", cleanupLatex(s), "}"))
# }

# fmtAnnotator <- function(annot){
#   if (length(annot)){
#     paste0('With annots. by ', authorList(annot), '.')
#   }
# }
fmtAnnotator <- labelPersons(prefix = 'With annots. by ', suffix = '.')
fmtCommentator <- labelPersons(prefix = 'With a comment. by ', suffix = '.')
fmtIntroduction <- labelPersons(prefix = 'With an intro. by ', suffix = '.')
fmtForeword <- labelPersons(prefix = 'With a forew. by ', suffix = '.')
fmtAfterword <- labelPersons(prefix = 'With an afterw. by ', suffix = '.')
fmtHolder <- labelPersons(suffix = '.')
fmtIBAuthor <- labelPersons(suffix ='.')

# fmtCommentator <- function(comm){
#   if (length(comm)){
#     paste0('With a comment. by ', authorList(comm), '.')
#   }
# }

fmtVersion <- label(prefix = 'Version ', suffix = '.')

fmtEdition <- function(ed){
  if (length(ed)){
    if (!is.na(suppressWarnings(as.integer(ed)))){
      paste0(ed, switch(ed, '1'='st', '2'='nd', '3'='rd', 'th'), ' ed.')
    }else{
      paste0(ed, '.')
    }
  }
}

fmtEventDate <- function(ed, ven){
  if (length(ed) || length(ven)){
    fDate <- try(ProcessDate(ed, NULL), TRUE)
    if (is.null(fDate) || inherits(fDate, 'try-error')){
      if (length(ven))
        paste0('(', ven, ')')
    }else{
      paste0('(', paste0(c(ven, DateFormatter(fDate)), collapse = ', '), ')')  
    } 
  }
}

fmtTranslator <- function(paper){
  if (length(paper$translator))
    paste0('Trans. ', fmtLangOrig(paper$origlanguage), ' by ', authorList(paper$translator), '.')
}

fmtLangOrig <- function(lang){
  if (length(lang))
    paste0('from the ', sub("\\b(\\w)",    "\\U\\1", lang, perl=TRUE))
}

fmtLanguage <- function(lang){
  if (length(lang) && tolower(lang) != 'english')
    addPeriod(sub("\\b(\\w)",    "\\U\\1", lang, perl=TRUE))
}

fmtSeries <- label(prefix = '. ')

fmtPubstate <- function(ps){
  if (length(ps)){
    cleanupLatex(addPeriod(switch(ps, inpreparation = 'In preparation.', submitted = 'Submitted.', 
                forthcoming = 'Forthcoming.', inpress = 'In press.', prepublished = 'Pre-published.', ps)))
  }
}

fmtPLocation <- labelclean(prefix = '(', suffix = ')')
fmtAddendum <- cleanap
fmtAddOn <- cleanap
fmtHowPublished <- cleanap
fmtOtherField <- cleanap

fmtType <- function(type){
  if (length(type)){
    ind <- match(type, names(.BibEntryTypeField))
    if (is.na(ind)){
      cleanupLatex(type)
    }else{
      .BibEntryTypeField[ind]
    }
  }
}
# fmtType <- cleanap

# fmtOtherField <- function(field){
#   if (length(field)){
#     addPeriod(cleanupLatex(field))
#   }
# }

#####################################################################################
## Entry types: Bibliography Drivers in BibLaTeX (Sec. 4.2.3 in manual)
#####################################################################################
formatArticle <- function(paper){
#  browser()
#     collapse(c(fmtPrefix(paper), addPeriod(authorList(paper$author)), fmtJTitle(paper$title), 
#                fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
#                fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
#         fmtVersion(paper$version), sentence(paste0(c(fmtJournal(paper), 
#                                       paste0(c(fmtSeries(paper$series), fmtVolume(paper$volume, paper$number), 
#                                                fmtJournDate(paper)), collapse =' '), fmtIssueTitle(paper), 
#                                             fmtEditor(paper, suffix = NULL, prefix = '. '), 
#                                             fmtNote(paper$note, prefix ='. ', suffix = NULL)), collapse = ''),
#                         fmtPages(paper$pages, paper$pagination), sep = ''), fmtISSN(paper$issn), 
#                fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
#                fmtPubstate(paper$pubstate)
#                ))
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJTitle(paper$title), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
        fmtVersion(paper$version), sentence(paste0(c(paste0(c(fmtJournal(paper), 
                                      fmtSeries(paper$series)), collapse = ''), fmtVolume(paper$volume, paper$number), 
                                               fmtJournDate(paper)), collapse =' '), fmtIssueTitle(paper), 
                                            fmtEditor(paper, suffix = NULL, prefix = '. '), 
                                            fmtNote(paper$note, prefix ='. ', suffix = NULL),
                        fmtPages(paper$pages, paper$pagination), sep = ''), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}
# if main title fields are missing, title fields are moved before
formatBook <- function(paper, collection = FALSE){
  if (collection && length(paper$author))
    paper$author <- NULL
#   if (mv){
#     if (length(paper$maintitle))
#       paper$maintitle <- NULL
#     if (length(paper[['volume']]))
#       paper$volume <- NULL
#   }

  if (length(paper$maintitle)){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$maintitle), 
               fmtBTitle(paper$mainsubtitle), fmtAddOn(paper$maintitleaddon), 
               paste0(c(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$title)), collapse = ': '),
                        fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
        fmtEdition(paper$edition), fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note),  
                                        sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), addPeriod(fmtBVolume(paper$volume, paper$part)), 
               fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }
}

# formatMvBook <- function(paper){
#     collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
#                fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
#                fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
#                fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
#                fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
#                fmtEdition(paper$edition), fmtVolumes(paper$volumes), 
#                sentence(paper$series, paper$number, sep = ' '), 
#                fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
#                                                      fmtDate(attr(paper, 'dateobj')),                                  
#                fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
#                fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISSN(paper$issn), 
#                fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
#                fmtPubstate(paper$pubstate)
#                ))
# }

formatInBook <- function(paper, bookinbook = FALSE){
  if (length(paper$booktitle) && length(paper$maintitle)){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), 
               fmtIBTitle(paper$title, paper$subtitle, bookinbook),
               #fmtJTitle(paste0(c(addPeriod(paper$title), paper$subtitle), collapse =' ')),
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),  
               paste0(c('In: ', fmtIBAuthor(paper$bookauthor), fmtBTitle(paper$maintitle))), 
               fmtBTitle(paper$mainsubtitle), fmtAddOn(paper$maintitleaddon), 
               paste0(c(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$booktitle)), collapse = ': '),
                        fmtBTitle(paper$booksubtitle), fmtAddOn(paper$booktitleaddon),
               fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
        fmtEdition(paper$edition), fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note),  
                                        sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    if (length(paper$maintitle)){
      paper$booktitle <- paper$maintitle
      paper$booksubtitle <- paper$mainsubtitle
      paper$booktitleaddon <- paper$maintitleaddon
    }
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), 
               fmtIBTitle(paper$title, paper$subtitle, bookinbook),
               #fmtJTitle(paste0(c(addPeriod(paper$title), paper$subtitle), collapse =' ')), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               paste0(c('In: ', fmtIBAuthor(paper$bookauthor), fmtBTitle(paper$booktitle))), 
               fmtBTitle(paper$booksubtitle), fmtAddOn(paper$booktitleaddon), 
                          fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), addPeriod(fmtBVolume(paper$volume, paper$part)), 
               fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
           ))
    
  }
}

formatBooklet <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtHowPublished(paper$howpublished),
               fmtNote(paper$note), sentence(fmtPublisher(NULL, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}

formatManual <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtEdition(paper$edition), sentence(paper$series, paper$number, sep = ' '),
               addPeriod(fmtType(paper$type)), fmtVersion(paper$version),
               fmtNote(paper$note), fmtOrganization(paper$organization), 
               sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}
    
formatMisc <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtHowPublished(paper$howpublished), addPeriod(fmtType(paper$type)), fmtVersion(paper$version),
               fmtNote(paper$note), 
               sentence(fmtPublisher(paper$organization, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')), sep = ''), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))    
}

formatOnline <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtNote(paper$note), fmtOtherField(paper$organization),
               addPeriod(fmtDate(attr(paper, 'dateobj'))), 
               fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))    
}

formatPatent <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtIBTitle(paper$title, paper$subtitle, FALSE), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               sentence(fmtType(paper$type), paper$number, fmtPLocation(paper$location), sep = ' '), 
               fmtHolder(paper$holder), fmtNote(paper$note), addPeriod(fmtDate(attr(paper, 'dateobj'))), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))    
}

formatPeriodical <- function(paper){
  if (length(paper$author))
    paper$author <- NULL
#   if (mv){
#     if (length(paper$maintitle))
#       paper$maintitle <- NULL
#     if (length(paper[['volume']]))
#       paper$volume <- NULL
#   }

  if (length(paper$issuetitle)){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$title),
               paste0(c(paste0(c(paper$series, 
                  fmtVolume(paper$volume, paper$number), fmtJournDate(paper, TRUE)), collapse = ' '), 
                               fmtBTitle(paper$issuetitle)), collapse = ': '),
                        fmtBTitle(paper$issuesubtitle),
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),                                
               fmtNote(paper$note), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), sentence(paper$series, 
               fmtVolume(paper$volume, paper$number), fmtJournDate(paper), sep = ' '),
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),                                
               fmtNote(paper$note), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }
}

formatProceedings <- function(paper){
  if (length(paper$author))
    paper$author <- NULL
#   if (mv){
#     if (length(paper$maintitle))
#       paper$maintitle <- NULL
#     if (length(paper[['volume']]))
#       paper$volume <- NULL
#   }

  if (length(paper$maintitle)){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$maintitle), 
               fmtBTitle(paper$mainsubtitle), fmtAddOn(paper$maintitleaddon), fmtLanguage(paper$language), 
               paste0(c(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$title)), collapse = ': '),
                        fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtEventTitle(paper$eventtitle), sentence(paper$eventtitleaddon, 
            fmtEventDate(paper$eventdate, paper$venue), sep = ' '), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                           fmtOrganization(paper$organization), fmtNote(paper$note),  
                                        sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), fmtEventTitle(paper$eventtitle),
               fmtAddOn(paper$eventtitleaddon), fmtEventDate(paper$eventdate, paper$venue), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), addPeriod(fmtBVolume(paper$volume, paper$part)), 
               fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), fmtOrganization(paper$organization), 
               sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }
}

formatInProceedings <- function(paper){
  if (length(paper$booktitle) && length(paper$maintitle)){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), 
               fmtIBTitle(paper$title, paper$subtitle, FALSE),
               #fmtJTitle(paste0(c(addPeriod(paper$title), paper$subtitle), collapse =' ')),
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),  
               paste0(c('In: ', fmtBTitle(paper$maintitle))), 
               fmtBTitle(paper$mainsubtitle), fmtAddOn(paper$maintitleaddon), 
               paste0(c(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$booktitle)), collapse = ': '),
                        fmtBTitle(paper$booksubtitle), fmtAddOn(paper$booktitleaddon),
               fmtEventTitle(paper$eventtitle), sentence(paper$eventtitleaddon, 
                                              fmtEventDate(paper$eventdate, paper$venue), sep = ' '), 
               fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note), fmtOrganization(paper$organization),  
                                        sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    if (length(paper$maintitle)){
      paper$booktitle <- paper$maintitle
      paper$booksubtitle <- paper$mainsubtitle
      paper$booktitleaddon <- paper$maintitleaddon
    }
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), 
               fmtIBTitle(paper$title, paper$subtitle, FALSE),
               #fmtJTitle(paste0(c(addPeriod(paper$title), paper$subtitle), collapse =' ')), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               paste0(c('In: ', fmtBTitle(paper$booktitle))), 
               fmtBTitle(paper$booksubtitle), fmtAddOn(paper$booktitleaddon), fmtEventTitle(paper$eventtitle),
               sentence(paper$eventtitleaddon, fmtEventDate(paper$eventdate, paper$venue), sep = ' '),
               fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               addPeriod(fmtBVolume(paper$volume, paper$part)), 
               fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), fmtOrganization(paper$organization), 
               sentence(fmtPublisher(paper$publisher, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISBN(paper$isbn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
           ))
    
  }
}

formatReport <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), sentence(fmtType(paper$type), paper$number, sep = ' '), 
               fmtVersion(paper$version), fmtNote(paper$note), 
               sentence(fmtPublisher(paper$institution, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination), fmtISRN(paper$isrn),
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))    
}

formatThesis <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtIBTitle(paper$title, paper$subtitle, FALSE),
               fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), addPeriod(fmtType(paper$type)), 
               fmtNote(paper$note), 
               sentence(fmtPublisher(paper$institution, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$pagetotal, paper$bookpagination),
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))    
}

formatUnpublished <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtIBTitle(paper$title, paper$subtitle, FALSE),
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               fmtHowPublished(paper$howpublished), fmtNote(paper$note), 
               sentence(fmtPublisher(NULL, paper$location, paper$address),
                                                     fmtDate(attr(paper, 'dateobj')), sep = ''), 
               fmtDOI(paper$doi), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))    
}

environment()
}))

toRd.BibEntry <- function(obj, style = .BibOptions$bib.style, .sorting ='nty', ...) { 
  curstyle <- .BibOptions$bib.style 
  
  if (is.null(style)){
    style <- .BibOptions$bib.style <- 'BibLaTeX'
  }else if (style %in% tools::getBibstyle(TRUE)){
    .BibOptions$bib.style <- style
  }else if (style %in% c('authortitle', 'alphabetic', 'numeric', 'draft')){
    .BibOptions$bib.style <- style
    style <- 'BibLaTeX'
  }

  obj <- sort(obj, .bibstyle=.BibOptions$bib.style, sorting = .sorting, return.ind = FALSE)
  style <- tools::bibstyle(style)
  env <- new.env(hash = FALSE, parent = style)
 # browser()
  if ((.BibOptions$bib.style == 'authoryear' || .BibOptions$bib.style == 'authortitle') && .BibOptions$dashed){
    dups <- duplicated(style$sortKeys(obj))
    dashed <- TRUE
  }else{
    dashed <- FALSE
    assign('dup', 'FALSE', style)
  }
  assign('bibstyle', .BibOptions$bib.style, style)

  bib <- unclass(obj)
  result <- character(length(bib))
 # browser()
  for (i in seq_along(bib)) {
    assign('paper', bib[[i]], env)
    if (dashed)
      assign('dup', dups[i], style)
  	result[i] <- with(env,
  	  switch(tolower(attr(paper, "bibtype")),
  	    article = formatArticle(paper),  # Done
  	    book = formatBook(paper),        # Done
        mvbook = formatBook(paper, collection = FALSE),    # Done             
  	    inbook = formatInBook(paper),
        bookinbook = formatInBook(paper, TRUE),                 
        suppbook = formatInBook(paper),                 
        booklet = formatBooklet(paper),          
        collection = formatBook(paper, TRUE),                 
        mvcollection = formatBook(paper, collection = TRUE),                 
  	    incollection = formatInCollection(paper),
        suppcollection = formatInCollection(paper),         
        manual = formatManual(paper),
  	    misc = formatMisc(paper),    
        online = formatOnline(paper),
        patent = formatPatent(paper),
        periodical = formatPeriodical(paper),
        suppperiodical = formatArticle(paper),
        proceedings = formatProceedings(paper),
        mvproceedings = formatProceedings(paper),                  
        inproceedings = formatInProceedings(paper), 
        reference = formatBook(paper, TRUE),  # alias for collection   
        mvreference = formatBook(paper, collection = TRUE),                 
        inreference = formatInCollection(paper),         
        report = formatReport(paper),
        thesis = formatThesis(paper),                 
  	    unpublished = formatUnpublished(paper),
        set = NULL,
# Aliases                 
        techreport = formatReport(paper, ifelse(is.null(paper$type), 'technical report', paper$type)),          
        phdthesis = formatThesis(paper, ifelse(is.null(paper$type), 'PhD Thesis', paper$type)),
        mastersthesis = formatThesis(paper, ifelse(is.null(paper$type), "Master's Thesis", paper$type)),
        www = formatOnline(paper),
        electronic = formatOnline(paper),
        conference = formatInProceedings(paper),       
  	    paste("bibtype", attr(paper, "bibtype"), "not implemented") ))
  }
  .BibOptions$bib.style <- curstyle
  result
}