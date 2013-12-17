tools::bibstyle('BibLaTeX', envir = local({

#####################################################################################
## FIELDS: 
#####################################################################################
fmtJournDate <- function(s){
  res <- DateFormatter(attr(s, 'dateobj'))
  paste0("(", paste0(c(s$issue, res), collapse = ' '), ")")
}

fmtDOI <- function(s){
  if (length(s) && .BibOptions$print.doi)
    paste0('DOI: ', s)
}

fmtPages <- label(prefix = ', pp. ')

fmtNote <- label(prefix = '. ')

fmtJournal <- function(s){
  if (!is.null(s$journaltitle)){
    res <- paste0('In: ', emph(cleanupLatex(s$journaltitle)))
    if (length(s$journalsubtitle))
      res <- paste0(res, '. ', emph(cleanupLatex(s$journalsubtitle)))
    return(res)
  }else if(!is.null(s$journal)){
    paste0('In: ', emph(cleanupLatex(s$journal)))  
  }
}

fmtJournVol <- function(s){
  if (length(s$volume)){
    res <- s$volume
    if (length(s$number))
      res <- paste(s$volume, s$number, sep='.')
    res
  }
}

fmtISSN <- label(prefix = 'ISSN: ', suffix = '.')

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
      fDate <- try(ProcessDate(paper$urldate), FALSE)
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

fmtEditor <- function(doc){
  res <- NULL
  if (length(doc$editor)){
    res <- c(res, fmtSingleEditor(authorList(doc$editor), doc$editortype))
  }
  if (length(doc$editora)){
    res <- c(res, fmtSingleEditor(authorList(doc$editora), doc$editoratype))
  }
  if (length(doc$editorb)){
    res <- c(res, fmtSingleEditor(authorList(doc$editorb), doc$editorbtype))
  }
  if (length(doc$editorc)){
    res <- c(res, fmtSingleEditor(authorList(doc$editorc), doc$editorctype))
  }
  paste0(res)
}

fmtEdition <- function(s){
  f(cleanupLatex(s))  
}

fmtTitle <- function(title){
  if (length(grep('[.?!]$', title))){
    paste0("\\dQuote{", collapse(cleanupLatex(title)), "}")
  }else{
    paste0("\\dQuote{", collapse(cleanupLatex(title)), "}.")
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

# fmtCommentator <- function(comm){
#   if (length(comm)){
#     paste0('With a comment. by ', authorList(comm), '.')
#   }
# }

fmtAnnotator <- labelPersons(prefix = 'With a comment. by ', suffix = '.')

fmtVersion <- label(prefix = 'Version ', suffix = '.')

fmtTranslator <- function(paper){
  if (length(paper$translator))
    paste0('Trans. ', fmtLangOrig(paper$origlanguage), authorList(paper$translator), '.')
}

fmtLangOrig <- function(lang){
  if (length(lang))
    paste0('\\bold{from ', lang, '} ')
}

fmtSeries <- label(prefix = '. ')

fmtOtherField <- function(field){
  if (length(field)){
    addPeriod(cleanupLatex(field))
  }
}

##################################################################
## Formatting functions

fmtPrefix <- function(paper){
  if (.BibOptions$bib.prefix == 'numeric'){
    paste0("[", paper$.index, "]")
  }else{
    NULL
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

emph <- function (s){
  if (length(s)) 
    paste0("\\emph{", collapse(s), "}")
}

plainclean <- function (s){
  plain(cleanupLatex(s))
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

fmtSingleEditor <- function(nom, job){
  if (length(nom)){
    if(length(job)){
      paste(switch(tolower(job), 'compiler' = '. Comp. by', 'editor' = '. Ed. by', 'founder' = '. Found. by', 
                   'continuator' = '. Cont. by', 'redactor' = '. Red. by', 'reviser' = '. Rev. by',
                   'collaborator' = '. In collab. with', job), nom)
    }else{
      paste0('. Ed. by ', nom)
    }
  }
}

shortName <- function(person){
    if (length(person$family)) {
        result <- cleanupLatex(person$family)
        if (length(person$given)) 
            paste(result, paste(substr(sapply(person$given, cleanupLatex), 
                1, 1), collapse = ""))
        else result
    }
    else paste(cleanupLatex(person$given), collapse = " ")
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
      paste0(format(begind, fmt), format(endd, fmt), collapse ='-')
    }else{
      format(dat, fmt)
    }
  }
}

sortKeys <- function(bib){
    result <- character(length(bib))
    for (i in seq_along(bib)) {
        authors <- authorList(bib[[i]]$author)
        if (!length(authors)) 
            authors <- authorList(bib[[i]]$editor)
        if (!length(authors)) 
            authors <- ""
        result[i] <- authors
    }
    result
}
#####################################################################################
## Entry types: Bibliography Drivers in BibLaTeX (Sec. 4.2.3 in manual)
#####################################################################################
formatArticle <- function(paper){
    collapse(c(fmtPrefix(paper), addPeriod(authorList(paper$author)), fmtTitle(paper$title), 
               fmtOtherField(paper$titleaddon), fmtOtherField(paper$language),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
        fmtVersion(paper$version), sentence(paste0(c(fmtJournal(paper), fmtSeries(paper$series), 
                                            fmtJournVol(paper), fmtJournDate(paper), fmtIssueTitle(paper), 
                                            fmtEditor(paper), fmtNote(paper$note)), collapse = ' '),
                        fmtPages(paper$pages), sep=''), fmtISSN(paper$issn), fmtEprint(paper), fmtURL(paper),
               fmtOtherField(paper$addendum), fmtOtherField(paper$pubstate)
               ))
}
environment()
}))
