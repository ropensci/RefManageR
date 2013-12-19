tools::bibstyle('authoryear', default = FALSE, envir = local({

##################################################################
## Formatting functions

fmtPrefix <- function(paper){
  switch(.BibOptions$bib.style, numeric = paste0("[", paper$.index, "]"),
         alphabetic = fmtAlpha(paper),
         draft = paper$key,
         NULL)
#   if (.BibOptions$bib.style == 'numeric'){
#     paste0("[", paper$.index, "]")
#   }else if()
#   
#   }else{
#     NULL
#   }
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

collapseF <- function(strings){ 
  out <- paste(strings, collapse = "\n")
  gsub('\\.\\n, ', ',\\\n', out)
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
    result <- character(length(bib))
    for (i in seq_along(bib)) {
      authors <- bib[[i]]$sortname
      if (!length(authors))
        authors <- paste0(sapply(bib[[i]]$author, shortNameLF), collapse = '')
      if (!length(authors)){ 
        authors <- paste0(sapply(bib[[i]]$editor, shortNameLF), collapse = '')
        if (!length(authors))
          authors <- paste0(sapply(bib[[i]]$translator, shortNameLF), collapse = '')
        if (!length(authors)) 
          authors <- ""
      }
      result[i] <- authors
    }
    result
}  

sortKeysY <- function(bib){ 
    result <- character(length(bib))
    for (i in seq_along(bib)) {
      res <- bib[[i]]$sortyear
      if (!length(res)){
        res <- bib[[i]]$dateobj
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

sortKeysLA <- function(bib, auts, yrs){
    result <- character(length(bib))
    for (i in seq_along(bib)) {
        res <- bib[[i]]$shorthand
        if (!length(res)){ 
          res <- bib[[i]]$label
          if (!length(res)) 
            res <- substr(bib[[i]]$shortauthor, 1, 3)
          if (!length(res)) 
            res <- substr(bib[[i]]$shorteditor, 1, 3)
          if (!length(res)) 
            res <- substr(auts[i], 1, 3)
          res <- paste0(res, substr(yrs[i], 3, 4))
        }
        result[i] <- res
    }
    result
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
# fmtJournDate <- function(s){
#   res <- DateFormatter(attr(s, 'dateobj'))
#   paste0("(", paste0(c(s$issue, res), collapse = ' '), ")")
# }

 fmtDate <- function(dat, ind = ''){
   if (length(dat))
     paste0('(', DateFormatter(dat), ind, ').')
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
        paste0(switch(pref, column = ', col. ', verse = ', v. ', line = ', l. ', 
                    section = ', \u00a7 ', paragraph = ', par. ', none = ', ', ', p. '), pgs)
      }else{
        paste0(switch(pref, column = ', cols. ', verse = ', vv. ', line = ', ll. ', 
                    section = ', \u00a7\u00a7 ', paragraph = ', par. ', none = ', ', ', pp. '), pgs)
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
                    section = ' \u00a7\u00a7', paragraph = ' par.', none = '.', ' pp.'))
      }
    }
  }
}

fmtIssue <- label(prefix = '(', suffix =')')

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
      res <- paste(vol, num, sep='.')
    paste0(res, '.')
  }
}

fmtVolumes <- label(suffix = ' vols.')

fmtBAuthor <- function(doc){
  if (dup != '')
    return('\u2500\u2500\u2500')
  #browser()
  res <- NULL
  if (length(doc$author)){
    res <- doc$author
  }else if (length(doc$editor)){
    res <- doc$editor
  }
  if (length(res) > 1){ 
    res <- paste(paste(sapply(res[-length(res)], shortNameLF), collapse = ", "), 
        "and", shortName(res[length(res)]))
  }else{
    res <- shortNameLF(res)
  }
  if (!length(doc$author) && length(doc$editor)){
    if (!length(doc$editortype)){
      res <- paste0(res, ', ed.')
    }else{
      res <- paste0(res, switch(tolower(doc$editortype), 'compiler' = ', comp.', 'editor' = ', ed.', 
                                'founder' = ', found.', 'continuator' = ', cont.', 
                                'redactor' = ', red.', 'reviser' = ', rev.',
                   'collaborator' = ', collab.', doc$editortype))
    }
  }else{
    res <- addPeriod(res)
  }
  res
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
      fDate <- try(ProcessDate(paper$urldate, NULL))
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
fmtIBAuthor <- labelPersons(suffix ='. ')

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
    switch(ps, inpreparation = 'In preparation.', submitted = 'Submitted.', forthcoming = 'Forthcoming.',
           inpress = 'In press.', prepublished = 'Pre-published.', ps)
  }
}

fmtAddendum <- cleanap
fmtAddOn <- cleanap
fmtHowPublished <- cleanap

fmtOtherField <- function(field){
  if (length(field)){
    addPeriod(cleanupLatex(field))
  }
}

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
      collapseF(c(fmtBAuthor(paper$author), fmtDate(paper$dateobj, paper$.index), fmtJTitle(paper$title), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
        fmtVersion(paper$version), sentence(paste0(c(paste0(c(fmtJournal(paper), 
                                      fmtSeries(paper$series)), collapse = ''), fmtVolume(paper$volume, paper$number), 
                                               fmtIssue(paper[['issue']])), collapse =' '), fmtIssueTitle(paper), 
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
 # browser()
  if (length(paper$maintitle)){
    collapseF(c(fmtBAuthor(paper), fmtDate(attr(paper, 'dateobj'), paper$.index),  fmtBTitle(paper$maintitle), 
               fmtBTitle(paper$mainsubtitle), fmtAddOn(paper$maintitleaddon), 
               sentence(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$title), sep = ': '),
                        fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
        fmtEdition(paper$edition), fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note),  
                                        sentence(fmtPublisher(paper$publisher, paper$location, paper$address),                                
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    collapseF(c(fmtBAuthor(paper), fmtDate(attr(paper, 'dateobj'), paper$.index), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), fmtBVolume(paper$volume, paper$part), fmtVolumes(paper$volumes), 
               sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location, paper$address),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
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
#                fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
#                fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
#                fmtPubstate(paper$pubstate)
#                ))
# }

formatInBook <- function(paper, bookinbook = FALSE){
  if (length(paper$booktitle) && length(paper$maintitle)){
    collapseF(c(fmtBAuthor(paper), fmtDate(attr(paper, 'dateobj'), paper$.index),  
               fmtIBTitle(paper$title, paper$subtitle, bookinbook),
               #fmtJTitle(paste0(c(addPeriod(paper$title), paper$subtitle), collapse =' ')),
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),  
               paste0(c('In: ', fmtIBAuthor(paper$bookauthor), fmtBTitle(paper$maintitle))), 
               fmtBTitle(paper$mainsubtitle), fmtAddOn(paper$maintitleaddon), 
               sentence(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$booktitle), sep = ': '),
                        fmtBTitle(paper$booksubtitle), fmtAddOn(paper$booktitleaddon),
               fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
        fmtEdition(paper$edition), fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note),  
                                        sentence(fmtPublisher(paper$publisher, paper$location, paper$address),                                 
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    if (length(paper$maintitle)){
      paper$booktitle <- paper$maintitle
      paper$booksubtitle <- paper$mainsubtitle
      paper$booktitleaddon <- paper$maintitleaddon
    }
    collapseF(c(fmtBAuthor(paper), fmtDate(attr(paper, 'dateobj'), paper$.index),  
               fmtIBTitle(paper$title, paper$subtitle, bookinbook),
               #fmtJTitle(paste0(c(addPeriod(paper$title), paper$subtitle), collapse =' ')), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               paste0(c('In: ', fmtIBAuthor(paper$bookauthor), fmtBTitle(paper$booktitle))), 
               fmtBTitle(paper$booksubtitle), fmtAddOn(paper$booktitleaddon), 
                          fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), fmtBVolume(paper$volume, paper$part), fmtVolumes(paper$volumes), 
               sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location, paper$address),                                 
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
           ))
    
  }
}

formatBooklet <- function(paper){
    collapseF(c(fmtBAuthor(paper), fmtDate(attr(paper, 'dateobj'), paper$.index), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtAddOn(paper$titleaddon), 
               fmtLanguage(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtHowPublished(paper$howpublished),
               fmtNote(paper$note), sentence(fmtPublisher(NULL, paper$location, paper$address),                                
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}

environment()
}))