#' @keywords internal
#' @noRd
MakeBibLaTeX <- function(docstyle = "text", authortitle = FALSE) {
  DF <- function(dat, other = FALSE){
    if (!is.null(dat)){
      if (other){
          fmt <- switch(as.character(attr(dat, 'day.mon')), '2' = '%m/%d/%Y',
                        '1' ='%m/%Y', '0' ='%Y')
      }else{
          fmt <- switch(as.character(attr(dat, 'day.mon')), '2' = '%b. %d, %Y',
                        '1' = '%b. %Y', '0' = '%Y')
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

  penv <- GetFormatFunctions(docstyle, DF)
  assign("authortitle", authortitle, penv)
  with(penv, {
    ##################################################################
    ## Formatting functions

    fmtPrefixSimple <- function(paper)
        switch(.BibOptions$bib.style,
               numeric = paste0("[", fmtNumPre(paper), "]"),
               alphabetic = paste0("[", paper$.index, "]"),
               draft = paste0('\\bold{', attr(paper, 'key'), '}'), NULL)

  fmtPrefix <- switch(docstyle, html = function(paper){
    res <- fmtPrefixSimple(paper)
    if (length(res)){
      key <- attr(paper, "key")
      ind <- .cites$indices[key]
      key <- gsub("[^_a-zA-Z0-9-]", "", key, useBytes = FALSE)
      res <- if (!is.na(ind) && ind)
        paste0("\\code{", key, "}\\href{#cite-", key, "}{", res, "}")
      else res
    }
    res
  }, markdown = function(paper){
    res <- fmtPrefixSimple(paper)
    if (length(res)){
      key <- attr(paper, "key")
      ind <- .cites$indices[key]
      key <- gsub("[^_a-zA-Z0-9-]", "", key, useBytes = FALSE)
      res <- if (!is.na(ind) && ind)
        paste0("<a name=bib-", key, "></a>[", res, "](#cite-", key, ")")
      else res
    }
    res
  }, fmtPrefixSimple)



  fmtNumPre <- function(doc){
    res <- doc$shorthand
    if (!length(res))
      res <- doc$.index
    res
  }

  sentenceP <- function (..., pgs = NULL, tp = NULL, sep = ", "){
    strings <- c(...)
    res <- NULL
    if (length(strings))
      res <- paste(strings, collapse = sep)
    res <- gsub('\\.$', '', res, useBytes = FALSE)
    if (!is.null(pgs))
      res <- paste(res, pgs, sep = ', ')
    if (!is.null(tp))
      res <- paste(res, tp, sep = '. ')
    addPeriod(res)
  }

  #############################################################################
  ## FIELDS:
  #############################################################################
  fmtJournDate <- function(s, usevenue = FALSE){
    res <- DateFormatter(attr(s, 'dateobj'))
    if (usevenue){
        paste0("(", paste0(c(cleanupLatex(gsub('[.?!]$', '', s[['venue']],
                                               useBytes = FALSE)),
                             res), collapse = ', '), ")")
    }else{
      paste0("(", paste0(c(s[['issue']], res), collapse = ' '), ")")
    }

  }

  fmtDate <- function(dat){
    if (length(dat))
      DateFormatter(dat)
  }

  fmtPages <- function(pgs, pref){
    if (length(pgs)){
      if (!length(pref)){
        if (grepl('-', pgs, useBytes = FALSE)){
          paste0('pp. ', pgs)
        }else{
          paste0('p. ', pgs)
        }
      }else{
        if (grepl('-', pgs, useBytes = FALSE)){
          paste0(switch(pref, column = 'cols. ', verse = 'vv. ', line = 'll. ',
                        section = '\u00a7\u00a7 ', paragraph = 'par. ',
                        none = '', 'pp. '), pgs)

        }else{
          paste0(switch(pref, column = 'col. ', verse = 'v. ', line = 'l. ',
                        section = '\u00a7 ', paragraph = 'par. ',
                        none = '', 'p. '), pgs)
        }
      }
    }
  }

  fmtTotalPages <- function(pgs, pref){
    if (length(pgs)){
      if (!length(pref)){
        if (pgs == 1){
          paste0(pgs, ' p')
        }else{
          paste0(pgs, ' pp')
        }
      }else{
        if (pgs == 1){
          paste0(pgs, switch(pref, column = ' col', verse = ' v', line = ' l',
                             section = ' \u00a7', paragraph = ' par',
                             none = '', ' p'))
        }else{
          paste0(pgs, switch(pref, column = ' cols', verse = ' vv',
                             line = ' ll',
                             section = ' \u00a7\u00a7', paragraph = ' par',
                             none = '', ' pp'))
        }
      }
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

  fmtBAuthor <- function(doc){
    res <- fmtBAuthorSimple(doc, .BibOptions$max.names)
    if (length(res) && authortitle){
      if (docstyle == "html"){
        key <- attr(doc, "key")
        ind <- .cites$indices[key]
        key <- gsub("[^_a-zA-Z0-9-]", "", key, useBytes = FALSE)
        res <- if (!is.na(ind) && ind)
                 paste0("\\code{", key, "}\\href{#cite-", key, "}{", res, "}")
               else res
      }else if (docstyle == "markdown"){
        key <- attr(doc, "key")
        ind <- .cites$indices[key]
        key <- gsub("[^_a-zA-Z0-9-]", "", key, useBytes = FALSE)
        res <- if (!is.na(ind) && ind)
                 paste0("<a name=bib-", key, "></a>[", res, "](#cite-",
                        key, ")")
               else res
      }
    }
    res
  }


  fmtBAuthorSimple <- function(doc, max.n){
    if (doc$.duplicated)
      return(switch(docstyle, html = "---", markdown = "\\-\\-\\-",
                    "\u2014\u2013\u2014"))
    out <- NULL
    if (length(doc$author)){
      res <- doc$author
    }else if (length(doc$editor)){
      res <- doc$editor
    }else if (length(doc$translator)){
      res <- doc$translator
    }else{
      return()
    }
    nnames <- length(res)
    if (nnames){
      if (bibstyle != "authortitle"){
        out <- shortName(res[1L])
      }else{
        out <- shortNameLF(res[1L])
      }
      if (nnames == 2L){
        if (max.n < 2L){
          out <- paste0(out, ' et al.')
        }else{
          out <- paste(out, shortName(res[-1L]), sep = ' and ')
        }
      }else if (nnames > 2L){
        if (nnames > max.n){
          if (max.n <= 1L){
            out <- paste0(out, ' et al.')
          }else{
              out <- paste0(paste(out, paste0(vapply(res[2L:max.n],
                                                     shortName, ""),
                                              collapse = ", "), sep = ', '),
                            ', et al.')
          }
        }else{
            out <- paste(paste(out, paste0(vapply(res[-c(1L, length(res))],
                                                  shortName, ""),
                                           collapse = ", "), sep = ', '),
                       shortName(res[length(res)]), sep = ', and ')
        }
      }
    }

    if (!length(doc$author) && length(doc$editor)){
      if (!length(doc$editortype)){
        out <- paste0(out, ', ed.')
      }else{
          out <- paste0(out, switch(tolower(doc$editortype),
                                    'compiler' = ', comp.',
                                    'editor' = ', ed.',
                                    'founder' = ', found.',
                                    'continuator' = ', cont.',
                                    'redactor' = ', red.',
                                    'reviser' = ', rev.',
                                    'collaborator' = ', collab.',
                                    doc$editortype))
          
      }
    }else{
      out <- addPeriod(out)
    }
    out
  }

  fmtPublisher <- function(pub, loc, addr){
    if (length(loc) || length(addr)){
      res <- if (length(loc))
                 plainclean(loc)
             else plainclean(addr)
      if (length(pub))
        res <- paste(res, plainclean(pub), sep = ': ')
      paste0(res, ', ')
    }else if (length(pub)){
      paste0(plainclean(pub), ', ')
    }
  }

  fmtEventDate <- function(ed, ven){
    if (length(ed) || length(ven)){
      fDate <- try(ProcessDate(ed, NULL), TRUE)
      if (is.null(fDate) || inherits(fDate, 'try-error')){
        if (length(ven))
          paste0('(', cleanupLatex(ven), ')')
      }else{
        paste0('(', paste0(c(cleanupLatex(ven), DateFormatter(fDate)),
                           collapse = ', '), ')')
      }
    }
  }

  #############################################################################
  ## Entry types: Bibliography Drivers in BibLaTeX (Sec. 4.2.3 in manual)
  #############################################################################
  formatArticle <- function(paper){
        collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJTitle(paper$title),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator), fmtVersion(paper$version),
                 sentenceP(paste0(c(paste0(c(fmtJournal(paper),
                                             fmtSeries(paper$series)),
                                           collapse = ''),
                                    fmtVolume(paper$volume, paper$number),
                                    fmtJournDate(paper)), collapse =' '),
                           fmtBTitle(paper$issuetitle, paper$issuesubtitle),
                           fmtEditor(paper, suffix = NULL, prefix = '. '),
                           fmtNote(paper$note, prefix ='. ', suffix = NULL),
                           pgs = fmtPages(paper$pages, paper$pagination),
                           sep = ''),
                 fmtISSN(paper$issn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }
  # if main title fields are missing, title fields are moved before
  formatBook <- function(paper, collection = FALSE){
    if (collection && length(paper$author))
      paper$author <- NULL

    if (length(paper$maintitle)){
        collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                   fmtBTitle(paper$maintitle, paper$mainsubtitle),
                 fmtAddOn(paper$maintitleaddon),
                 paste0(c(fmtBVolume(paper$volume, paper$part),
                          fmtBTitle(paper$title, paper$subtitle)),
                        collapse = ': '), fmtAddOn(paper$titleaddon),
                 fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition), fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                 fmtNote(paper$note),
                 sentence(fmtPublisher(paper$publisher, paper$location,
                                                            paper$address),
                          fmtDate(attr(paper, 'dateobj')),
                          fmtChapter(paper$chapter),
                          fmtPages(paper$pages, paper$bookpagination),
                          fmtTotalPages(paper$pagetotal, paper$bookpagination),
                          sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
    }else{
        collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                   fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)), fmtTranslator(paper),
                 fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition),
                 addPeriod(fmtBVolume(paper$volume, paper$part)),
                 fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                 fmtNote(paper$note), sentenceP(fmtPublisher(paper$publisher,
                                                             paper$location,
                                                             paper$address),
                                          fmtDate(attr(paper, 'dateobj')),
                                          fmtChapter(paper$chapter),
                                          pgs = fmtPages(paper$pages,
                                                         paper$bookpagination),
                                          tp = fmtTotalPages(paper$pagetotal,
                                                         paper$bookpagination),
                                          sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
    }
  }

  formatInBook <- function(paper, bookinbook = FALSE){
    if (length(paper$booktitle) && length(paper$maintitle)){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtIBTitle(paper$title, paper$subtitle, bookinbook),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 paste0(c('In: ', fmtIBAuthor(paper$bookauthor),
                          fmtBTitle(paper$maintitle, paper$mainsubtitle))),
                 fmtAddOn(paper$maintitleaddon),
                 paste0(c(fmtBVolume(paper$volume, paper$part),
                          fmtBTitle(paper$booktitle, paper$booksubtitle)),
                        collapse = ': '), fmtAddOn(paper$booktitleaddon),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition), fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                                              fmtNote(paper$note),
                 sentenceP(fmtPublisher(paper$publisher, paper$location,
                                        paper$address), fmtDate(attr(paper,
                                                                    'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
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
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 paste0(c('In: ', fmtIBAuthor(paper$bookauthor),
                          fmtBTitle(paper$booktitle, paper$booksubtitle))),
                 fmtAddOn(paper$booktitleaddon),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition), addPeriod(fmtBVolume(paper$volume,
                                                                 paper$part)),
                 fmtVolumes(paper$volumes), sentence(cleanupLatex(paper$series),
                                                     paper$number, sep = ' '),
                 fmtNote(paper$note), sentenceP(fmtPublisher(paper$publisher,
                                                             paper$location,
                                                             paper$address),
                                        fmtDate(attr(paper, 'dateobj')),
                                        fmtChapter(paper$chapter),
                                        pgs = fmtPages(paper$pages,
                                                       paper$bookpagination),
                                        tp = fmtTotalPages(paper$pagetotal,
                                                         paper$bookpagination),
                                        sep = ''), fmtISBN(paper$isbn),
                 fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper),
                 fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
             ))

    }
  }

  formatBooklet <- function(paper){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)), fmtTranslator(paper),
                 fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtHowPublished(paper$howpublished), fmtNote(paper$note),
                 sentenceP(fmtPublisher(NULL, paper$location, paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper),
                 fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }

  formatInCollection <- function(paper){
    if (length(paper$booktitle) && length(paper$maintitle)){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtIBTitle(paper$title, paper$subtitle, FALSE),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 paste0(c('In: ', fmtBTitle(paper$maintitle,
                                            paper$mainsubtitle))),
                 fmtAddOn(paper$maintitleaddon),
                 paste0(c(fmtBVolume(paper$volume, paper$part),
                          fmtBTitle(paper$booktitle, paper$booksubtitle)),
                        collapse = ': '), fmtAddOn(paper$booktitleaddon),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition), fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                 fmtNote(paper$note), sentenceP(fmtPublisher(paper$publisher,
                                                             paper$location,
                                                             paper$address),
                                        fmtDate(attr(paper, 'dateobj')),
                                        fmtChapter(paper$chapter),
                                        pgs = fmtPages(paper$pages,
                                                       paper$bookpagination),
                                        tp = fmtTotalPages(paper$pagetotal,
                                                         paper$bookpagination),
                                        sep = ''), fmtISBN(paper$isbn),
                 fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper),
                 fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
      ))
    }else{
      if (length(paper$maintitle)){
        paper$booktitle <- paper$maintitle
        paper$booksubtitle <- paper$mainsubtitle
        paper$booktitleaddon <- paper$maintitleaddon
      }
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtIBTitle(paper$title, paper$subtitle, FALSE),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 paste0(c('In: ', fmtBTitle(paper$booktitle,
                                            paper$booksubtitle))),
                 fmtAddOn(paper$booktitleaddon),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition),
                 addPeriod(fmtBVolume(paper$volume, paper$part)),
                 fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                 fmtNote(paper$note), sentenceP(fmtPublisher(paper$publisher,
                                                             paper$location,
                                                             paper$address),
                                        fmtDate(attr(paper, 'dateobj')),
                                        fmtChapter(paper$chapter),
                                        pgs = fmtPages(paper$pages,
                                                       paper$bookpagination),
                                        tp = fmtTotalPages(paper$pagetotal,
                                                         paper$bookpagination),
                                        sep = ''), fmtISBN(paper$isbn),
                 fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper),
                 fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
      ))

    }
  }

  formatManual <- function(paper){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)),
                 fmtEdition(paper$edition),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                 addPeriod(fmtType(paper$type)), fmtVersion(paper$version),
                 fmtNote(paper$note), fmtOrganization(paper$organization),
                 sentenceP(fmtPublisher(paper$publisher, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination),
                           sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }

  formatMisc <- function(paper){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)),
                 fmtHowPublished(paper$howpublished),
                 addPeriod(fmtType(paper$type)), fmtVersion(paper$version),
                 fmtNote(paper$note), sentence(fmtPublisher(paper$organization,
                                                            paper$location,
                                                            paper$address),
                                               fmtDate(attr(paper, 'dateobj')),
                                               sep = ''),
                 fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper),
                 fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
                 ))
  }

  formatOnline <- function(paper){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)),
                 fmtNote(paper$note), fmtOtherField(paper$organization),
                 addPeriod(fmtDate(attr(paper, 'dateobj'))), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }

  formatPatent <- function(paper){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtIBTitle(paper$title, paper$subtitle, FALSE),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 sentence(fmtType(paper$type), paper$number,
                          fmtPLocation(paper$location), sep = ' '),
                 fmtHolder(paper$holder), fmtNote(paper$note),
                 addPeriod(fmtDate(attr(paper, 'dateobj'))), fmtDOI(paper$doi),
                 fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }

  formatPeriodical <- function(paper){
    if (length(paper$author))
      paper$author <- NULL

    if (length(paper$issuetitle)){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 paste0(c(paste0(c(cleanupLatex(paper$series),
                                   fmtVolume(paper$volume, paper$number),
                                   fmtJournDate(paper, TRUE)), collapse = ' '),
                          fmtBTitle(paper$issuetitle, paper$issuesubtitle)),
                        collapse = ': '),
                 fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtNote(paper$note), fmtISSN(paper$issn), fmtDOI(paper$doi),
                 fmtEprint(paper), fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
    }else{
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 sentence(cleanupLatex(paper$series),
                          fmtVolume(paper$volume, paper$number),
                          fmtJournDate(paper), sep = ' '),
                 fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)), fmtTranslator(paper),
                 fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword),
                 fmtAfterword(paper$afterword), fmtNote(paper$note),
                 fmtISSN(paper$issn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
    }
  }

  formatProceedings <- function(paper){
    if (length(paper$author))
      paper$author <- NULL

    if (length(paper$maintitle)){
        collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                   fmtBTitle(paper$maintitle, paper$mainsubtitle),
                 fmtAddOn(paper$maintitleaddon), fmtLanguage(paper$language),
                 paste0(c(fmtBVolume(paper$volume, paper$part),
                          fmtBTitle(paper$title, paper$subtitle)),
                        collapse = ': '),
                 fmtAddOn(paper$titleaddon), fmtEventTitle(paper$eventtitle),
                 sentence(paper$eventtitleaddon,
                          fmtEventDate(paper$eventdate, paper$venue),
                          sep = ' '),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtVolumes(paper$volumes), sentence(cleanupLatex(paper$series),
                                                     paper$number, sep = ' '),
                 fmtOrganization(paper$organization), fmtNote(paper$note),
                 sentenceP(fmtPublisher(paper$publisher, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
    }else{
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtEventTitle(paper$eventtitle),
                 fmtAddOn(paper$eventtitleaddon), fmtEventDate(paper$eventdate,
                                                               paper$venue),
                 fmtLanguage(paper$language),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 fmtEdition(paper$edition), addPeriod(fmtBVolume(paper$volume,
                                                                 paper$part)),
                 fmtVolumes(paper$volumes), sentence(cleanupLatex(paper$series),
                                                     paper$number, sep = ' '),
                 fmtNote(paper$note), fmtOrganization(paper$organization),
                 sentenceP(fmtPublisher(paper$publisher, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
    }
  }

  formatInProceedings <- function(paper){
    if (length(paper$booktitle) && length(paper$maintitle)){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtIBTitle(paper$title, paper$subtitle, FALSE),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 paste0(c('In: ', fmtBTitle(paper$maintitle,
                                            paper$mainsubtitle))),
                 fmtAddOn(paper$maintitleaddon),
                 paste0(c(fmtBVolume(paper$volume, paper$part),
                          fmtBTitle(paper$booktitle, paper$booksubtitle)),
                        collapse = ': '), fmtAddOn(paper$booktitleaddon),
                 fmtEventTitle(paper$eventtitle),
                 sentence(paper$eventtitleaddon, fmtEventDate(paper$eventdate,
                                                              paper$venue),
                          sep = ' '),
                 fmtEditor(paper, !length(paper$author)), fmtTranslator(paper),
                 fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword),
                 fmtAfterword(paper$afterword), fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                 fmtNote(paper$note), fmtOrganization(paper$organization),
                 sentenceP(fmtPublisher(paper$publisher, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
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
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 paste0(c('In: ', fmtBTitle(paper$booktitle,
                                            paper$booksubtitle))),
                 fmtAddOn(paper$booktitleaddon),fmtEventTitle(paper$eventtitle),
                 sentence(paper$eventtitleaddon,
                          fmtEventDate(paper$eventdate, paper$venue),
                          sep = ' '),
                 fmtEditor(paper, !length(paper$author)),
                 fmtTranslator(paper), fmtCommentator(paper$commentator),
                 fmtAnnotator(paper$annotator),
                 fmtIntroduction(paper$introduction),
                 fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                 addPeriod(fmtBVolume(paper$volume, paper$part)),
                 fmtVolumes(paper$volumes),
                 sentence(cleanupLatex(paper$series),
                          paper$number, sep = ' '),
                 fmtNote(paper$note), fmtOrganization(paper$organization),
                 sentenceP(fmtPublisher(paper$publisher, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
             ))

    }
  }

  formatReport <- function(paper, type = NULL){
    # check if got bibtex techreport with no type
    if (length(type))
      paper$type <- type
    if (!is.null(paper$school) && is.null(paper$institution))
      paper$institution <- paper$school

      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtBTitle(paper$title, paper$subtitle),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 sentence(fmtType(paper$type), paper$number, sep = ' '),
                 fmtVersion(paper$version), fmtNote(paper$note),
                 sentenceP(fmtPublisher(paper$institution, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           pgs = fmtPages(paper$pages, paper$bookpagination),
                           tp = fmtTotalPages(paper$pagetotal,
                                              paper$bookpagination), sep = ''),
                 fmtISRN(paper$isrn), fmtDOI(paper$doi), fmtEprint(paper),
                 fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }

  formatThesis <- function(paper, type = NULL){
    if (length(type))
      paper$type <- type
    if (!is.null(paper$school) && is.null(paper$institution))
      paper$institution <- paper$school

    collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
               fmtIBTitle(paper$title, paper$subtitle, FALSE),
               fmtAddOn(paper$titleaddon),
               fmtLanguage(paper$language), addPeriod(fmtType(paper$type)),
               fmtNote(paper$note),
               sentenceP(fmtPublisher(paper$institution, paper$location,
                                      paper$address),
                         fmtDate(attr(paper, 'dateobj')),
                         fmtChapter(paper$chapter),
                         pgs = fmtPages(paper$pages, paper$bookpagination),
                         tp = fmtTotalPages(paper$pagetotal,
                                            paper$bookpagination), sep = ''),
               fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper),
               fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
               ))
  }

  formatUnpublished <- function(paper){
      collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                 fmtIBTitle(paper$title, paper$subtitle, FALSE),
                 fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                 fmtHowPublished(paper$howpublished), fmtNote(paper$note),
                 sentence(fmtPublisher(NULL, paper$location, paper$address),
                          fmtDate(attr(paper, 'dateobj')), sep = ''),
                 fmtDOI(paper$doi), fmtURL(paper), fmtAddendum(paper$addendum),
                 fmtPubstate(paper$pubstate)
                 ))
  }

  environment()
  })
}

