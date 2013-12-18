tools::bibstyle('BibLaTeX', envir = local({

#####################################################################################
## FIELDS: 
#####################################################################################
fmtJournDate <- function(s){
  res <- DateFormatter(attr(s, 'dateobj'))
  paste0("(", paste0(c(s$issue, res), collapse = ' '), ")")
}

fmtDate <- function(dat){
  if (length(dat))
    DateFormatter(dat)
}

fmtDOI <- function(s){
  if (length(s) && .BibOptions$print.doi)
    paste0('DOI: ', s)
}

#fmtPages <- label(prefix = ', pp. ')
fmtPages <- function(pgs, pref){
  if (length(pgs)){
    if (!length(pref)){
      paste0(', pp. ', pgs)
    }else{
      paste0(switch(pref, column = ', cols. ', verse = ', vv. ', line = ', ll. ', 
                    section = ', \u00a7\u00a7 ', paragraph = ', par. ', none = ', ', ', pp. '), pgs)
    }
  }
}

fmtTotalPages <- function(pgs, pref){
  if (length(pgs)){
    if (!length(pref)){
      paste0(pgs, ' pp.')
    }else{
      paste0(pgs, switch(pref, column = ' cols.', verse = ' vv.', line = ' ll.', 
                    section = ' \u00a7\u00a7', paragraph = ' par.', none = '.', ', pp.'))
    }
  }
}

fmtNote <- function(note, prefix = NULL, suffix = '.'){
  if (length(note))
    paste0(prefix, note, suffix)
}

fmtJournal <- function(s){
  if (length(s$journaltitle)){
    res <- paste0('In: ', emph(cleanupLatex(s$journaltitle)))
    if (length(s$journalsubtitle))
      res <- paste0(res, '. ', emph(cleanupLatex(s$journalsubtitle)))
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
  #browser()
  res <- NULL
  if (length(doc$author)){
    names <- sapply(doc$author, shortName)
  }else if (length(doc$editor)){
    names <- sapply(doc$editor, shortName)
  }
  if (length(names) > 1){ 
    res <- paste(paste(names[-length(names)], collapse = ", "), 
        "and", names[length(names)], '.')
  }else{
    res <- paste0(names, '.')
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
  }
  res
}

fmtPublisher <- function(pub, loc){
  if (length(loc)){
    res <- loc
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

fmtSeries <- label(prefix = '. ')

fmtPubstate <- function(ps){
  if (length(ps)){
    switch(ps, inpreparation = 'In preparation.', submitted = 'Submitted.', forthcoming = 'Forthcoming.',
           inpress = 'In press.', prepublished = 'Pre-published.', ps)
  }
}

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

shortName <- function(person){
    if (length(person$family)) {
        result <- cleanupLatex(person$family)
        if (length(person$given)){ 
          if (.BibOptions$abbrev.names){
            paste(result, paste(substr(sapply(person$given, cleanupLatex), 
                1, 1), collapse = ""))
          }else{
            cleanupLatex(person)
          }
        }else{
          result
        }
    }else{
      paste(cleanupLatex(person$given), collapse = " ")
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
    collapse(c(fmtPrefix(paper), addPeriod(authorList(paper$author)), fmtJTitle(paper$title), 
               fmtOtherField(paper$titleaddon), fmtOtherField(paper$language),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
        fmtVersion(paper$version), sentence(paste0(c(fmtJournal(paper), 
                                      paste0(c(fmtSeries(paper$series), fmtVolume(paper$volume, paper$number), 
                                               fmtJournDate(paper)), collapse =' '), fmtIssueTitle(paper), 
                                            fmtEditor(paper, suffix = NULL, prefix = '. '), 
                                            fmtNote(paper$note, prefix ='. ', suffix = NULL)), collapse = ''),
                        fmtPages(paper$pages, paper$pagination), sep = ''), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtOtherField(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}
# if main title fields are missing, title fields are moved before
formatBook <- function(paper){
 # browser()
  if (length(paper$maintitle)){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$maintitle), 
               fmtBTitle(paper$mainsubtitle), fmtOtherField(paper$maintitleaddon), 
               sentence(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$title), sep = ': '),
                        fmtBTitle(paper$subtitle), fmtBTitle(paper$titleaddon),
               fmtOtherField(paper$titleaddon), fmtOtherField(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
        fmtEdition(paper$edition), fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note),  
                                            sentence(fmtPublisher(paper$publisher, paper$location),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtOtherField(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }else{
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtOtherField(paper$titleaddon), 
               fmtOtherField(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), fmtBVolume(paper$volume, paper$part), fmtVolumes(paper$volumes), 
               sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtOtherField(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
  }
}

formatMvBook <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$title), 
               fmtBTitle(paper$subtitle), fmtOtherField(paper$titleaddon), 
               fmtOtherField(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
               fmtEdition(paper$edition), fmtVolumes(paper$volumes), 
               sentence(paper$series, paper$number, sep = ' '), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$publisher, paper$location),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtOtherField(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}

formatInBook <- function(paper){
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtBTitle(paper$booktitle), 
               fmtBTitle(paper$booksubtitle), fmtOtherField(paper$booktitleaddon), 
               sentence(fmtBVolume(paper$volume, paper$part), fmtBTitle(paper$title), sep = ': '),
                        fmtBTitle(paper$subtitle), fmtBTitle(paper$titleaddon),
               fmtOtherField(paper$titleaddon), fmtOtherField(paper$language), fmtEditor(paper, !length(paper$author)),
               fmtTranslator(paper), fmtCommentator(paper$commentator), fmtAnnotator(paper$annotator),
               fmtIntroduction(paper$introduction), fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
        fmtEdition(paper$edition), fmtVolumes(paper$volumes), sentence(paper$series, paper$number, sep = ' '),
                                            fmtNote(paper$note),  
                                            sentence(fmtPublisher(paper$publisher, paper$location),
                                                     fmtDate(attr(paper, 'dateobj')),                                  
               fmtChapter(paper$chapter), fmtPages(paper$pages, paper$bookpagination), sep = ''), 
               fmtTotalPages(paper$totalpages), fmtISSN(paper$issn), 
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtOtherField(paper$addendum), 
               fmtPubstate(paper$pubstate)
               ))
}
environment()
}))

toRd.BibEntry <- function(obj, style=NULL, ...) {
    obj <- sort(obj, .bibstyle=style)
    style <- tools::bibstyle(style)
    env <- new.env(hash = FALSE, parent = style)
    bib <- unclass(obj)
    result <- character(length(bib))
    for (i in seq_along(bib)) {
      env$paper <- bib[[i]]
    	result[i] <- with(env,
    	    switch(tolower(attr(paper, "bibtype")),
    	    article = formatArticle(paper),  # Done
    	    book = formatBook(paper),        # Done
          mvbook = formatMvBook(paper),    # Done             
    	    inbook = formatInBook(paper),
          bookinbook = formatBookInBook(paper),                 
          suppbook = formatInBook(paper),                 
          booklet = formatBooklet(paper),          
          collection = formatCollection(paper),                 
          mvcollection = formatMvCollection(paper),                 
    	    incollection = formatInCollection(paper),
          suppcollection = formatSuppCollection(paper),         
          manual = formatManual(paper),
    	    misc = formatMisc(paper),    
          online = formatOnline(paper),
          patent = formatPatent(paper),
          periodical = formatPeriodical(paper),
          suppperiodical = formatSuppPeriodical(paper),
          proceedings = formatProceedings(paper),
          mvproceedings = formatMvProceedings(paper),                  
          inproceedings = formatInProceedings(paper), 
          reference = formatReference(paper),    
          mvreference = formatMvReference(paper),                 
          inreference = formatInReference(paper),         
          report = formatReport(paper),
          thesis = formatThesis(paper),                 
    	    unpublished = formatUnpublished(paper),
# Aliases                 
          techreport = formatReport(paper, ifelse(is.null(paper$type), 'technical report', paper$type)),          
          phdthesis = formatThesis(paper, ifelse(is.null(paper$type), 'PhD Thesis', paper$type)),
          mastersthesis = formatThesis(paper, ifelse(is.null(paper$type), "Master's Thesis", paper$type)),
          www = formatOnline(paper),
          electronic = formatOnline(paper),
          conference = formatInProceedings(paper),       
    	    paste("bibtype", attr(paper, "bibtype"), "not implemented") ))
    }
    result
}