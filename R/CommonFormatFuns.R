#' @keywords internal
#' @noRd
GetFormatFunctions <- function(docstyle = "text", DateFormatter){
    force(DateFormatter)
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

    cleanap <- function(s){
      if (length(s))
        cleanupLatex(addPeriod(s))
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

    shortName <- function(pers){
        if (length(pers$family)) {
            res <- cleanupLatex(pers$family)
            if (length(pers$given)){
              if (.BibOptions$first.inits){
                paste0(c(substr(vapply(pers$given, cleanupLatex, ""),
                    start = 1L, stop = 1L), res), collapse = ". ")
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

    fmtEventTitle <- cleanap

    fmtIBTitle <- function(tl, stl, bib){
      if (bib){  # bookinbook entry
        fmtBTitle(tl, stl)
      }else{
        if (!is.null(stl)){
          fmtJTitle(paste0(c(addPeriod(tl), stl), collapse =' '))
        }else{
          fmtJTitle(tl)
        }
      }
    }

    fmtAddendum <- cleanap
    fmtAddOn <- cleanap
    fmtHowPublished <- cleanap
    fmtOtherField <- cleanap
    fmtVolumes <- label(suffix = ' vols.')
    fmtOrganization <- label(suffix = '.')


    fmtJTitle <- function(title){
      if (!is.null(title))
        if (grepl('[.?!]$', title, useBytes = TRUE))
          paste0("\\dQuote{", collapse(cleanupLatex(title)), "}")
        else paste0("\\dQuote{", collapse(cleanupLatex(title)), "}.")
    }

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

    fmtTranslator <- function(paper){
      if (length(paper$translator))
          paste0('Trans. ', fmtLangOrig(paper$origlanguage), ' by ',
                 authorList(paper$translator), '.')
    }

    fmtLangOrig <- function(lang){
      if (length(lang))
        paste0('from the ', sub("\\b(\\w)",    "\\U\\1", lang, perl=TRUE,
                                useBytes = TRUE))
    }

    fmtLanguage <- function(lang){
      if (length(lang) && tolower(lang) != 'english')
        addPeriod(sub("\\b(\\w)",    "\\U\\1", lang, perl=TRUE,useBytes = TRUE))
    }

    fmtSeries <- label(prefix = '. ')

    fmtPubstate <- function(ps){
      if (length(ps)){
          cleanupLatex(addPeriod(switch(ps, inpreparation = 'In preparation.',
                                        submitted = 'Submitted.',
                                        forthcoming = 'Forthcoming.',
                                        inpress = 'In press.',
                                        prepublished = 'Pre-published.', ps)))
      }
    }

    fmtPLocation <- labelclean(prefix = '(', suffix = ')')
    fmtAnnotator <- labelPersons(prefix = 'With annots. by ', suffix = '.')
    fmtCommentator <- labelPersons(prefix = 'With a comment. by ', suffix = '.')
    fmtIntroduction <- labelPersons(prefix = 'With an intro. by ', suffix = '.')
    fmtForeword <- labelPersons(prefix = 'With a forew. by ', suffix = '.')
    fmtAfterword <- labelPersons(prefix = 'With an afterw. by ', suffix = '.')
    fmtHolder <- labelPersons(suffix = '.')
    fmtIBAuthor <- labelPersons(suffix ='.')

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

    fmtBTitle <- function(tl, stl){
      if (length(tl)){
        if (!is.null(stl))
          tl <- paste0(c(addPeriod(tl), stl), collapse =' ')
        if (grepl('[.?!]$', tl, useBytes = TRUE)){
          emph(cleanupLatex(tl))
        }else{
          paste0(emph(cleanupLatex(tl)), '.')
        }
      }
    }

    fmtEditor <- function(doc, editor.used.already = FALSE, prefix = NULL,
                          suffix = '.'){
      res <- NULL
      if (length(doc$editor)  && !editor.used.already){
          res <- c(res, fmtSingleEditor(authorList(doc$editor), doc$editortype,
                                        prefix, suffix))
      }
      if (length(doc$editora)){
          res <- c(res, fmtSingleEditor(authorList(doc$editora),
                                        doc$editoratype, prefix, suffix))
      }
      if (length(doc$editorb)){
          res <- c(res, fmtSingleEditor(authorList(doc$editorb),
                                        doc$editorbtype, prefix, suffix))
      }
      if (length(doc$editorc)){
        res <- c(res, fmtSingleEditor(authorList(doc$editorc),
                                      doc$editorctype, prefix, suffix))
      }
      paste0(res)
    }

    fmtVenue <- function(venue){
      if (length(venue)){
        venue <- gsub('[.?!]$', '', venue, useBytes = TRUE)
        paste0("(", collapse(cleanupLatex(venue)), ").")
      }
    }

    fmtEprint <- switch(docstyle, html = function(paper){
      if (length(paper$eprint)){
        if (length(paper$eprinttype)){
          eprinttype <- tolower(paper$eprinttype)
          res <- paste0(switch(eprinttype, 'arxiv'='arXiv', 'pubmed' = 'PMID',
                               'googlebooks' = 'Google Books',
                               'jstor' = 'JSTOR', 'hdl' = 'HDL',
                               paper$eprinttype),
                        ': ')

          if (eprinttype %in% c("arxiv", "pubmed", "jstor")){
            base.url <- switch(eprinttype,
                               jstor = "https://www.jstor.org/stable/",
                               arxiv = "https://arxiv.org/abs/",
                               pubmed = paste0("https://eutils.ncbi.nlm.nih.",
                                               "gov/entrez/eutils/",
                                               "elink.fcgi?dbfrom=pubmed&",
                                               "cmd=prlinks&retmode=ref&id="),
                               "")
            res <- paste0(res, "\\href{", base.url, paper$eprint, "}{",
                          paper$eprint)
            if (eprinttype == "arxiv"){
              if(length(paper$eprintclass)){
                res <- paste0(res, " [", paper$eprintclass, ']')
              }else if (length(paper$primaryclass)){
                res <- paste0(res, " [", paper$primaryclass, ']')
              }
            }
            res <- paste0(res, "}")
          }else{
            res <- paste0(res, paper$eprinttype)
          }
        }else if (length(paper$archiveprefix)){
          if (length(paper$eprintclass)){
            res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [',
                          paper$eprintclass, ']')
          }else if(length(paper$primaryclass)){
            res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [',
                          paper$primaryclass, ']')
          }else
            res <- paste0('eprint: ', paper$eprint)
        }else{
          res <- paste0('eprint: ', paper$eprint)
        }
        addPeriod(res)
      }
    }, markdown = function(paper){
      if (length(paper$eprint)){
        if (length(paper$eprinttype)){
          eprinttype <- tolower(paper$eprinttype)
          res <- paste0(switch(eprinttype, 'arxiv'='arXiv', 'pubmed' = 'PMID',
                               'googlebooks' = 'Google Books',
                               'jstor' = 'JSTOR',
                               'hdl' = 'HDL', paper$eprinttype),
                        ': ')

          if (eprinttype %in% c("arxiv", "pubmed", "jstor")){
            base.url <- switch(eprinttype,
                               jstor = "https://www.jstor.org/stable/",
                               arxiv = "https://arxiv.org/abs/",
                               pubmed = paste0("https://eutils.ncbi.nlm.nih.",
                                               "gov/entrez/eutils/",
                                               "elink.fcgi?dbfrom=pubmed&",
                                               "cmd=prlinks&retmode=ref&id="),
                               "")
            res <- paste0(res, "[", paper$eprint)
            if (eprinttype == "arxiv"){
              if(length(paper$eprintclass)){
                res <- paste0(res, " [", paper$eprintclass, ']')
              }else if (length(paper$primaryclass)){
                res <- paste0(res, " [", paper$primaryclass, ']')
              }
            }
            res <- paste0(res, "](", base.url, paper$eprint, ")")
          }else{
            res <- paste0(res, paper$eprinttype)
          }
        }else if (length(paper$archiveprefix)){
          if (length(paper$eprintclass)){
            res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [',
                          paper$eprintclass, ']')
          }else if(length(paper$primaryclass)){
            res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [',
                          paper$primaryclass, ']')
          }else
            res <- paste0('eprint: ', paper$eprint)
        }else{
          res <- paste0('eprint: ', paper$eprint)
        }
        addPeriod(res)
      }
    }, function(paper){
      if (length(paper$eprint)){
        if (length(paper$eprinttype)){
          res <- paste0(switch(tolower(paper$eprinttype), 'arxiv' = 'arXiv',
                               'pubmed' = 'PMID',
                               'googlebooks' = 'Google Books',
                               'jstor' = 'JSTOR',
                               'hdl' = 'HDL', paper$eprinttype),
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
            res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [',
                          paper$eprintclass, ']')
          }else if(length(paper$primaryclass)){
            res <- paste0(paper$archiveprefix, ': ', paper$eprint, ' [',
                          paper$primaryclass, ']')
          }else
            res <- paste0('eprint: ', paper$eprint)
        }else{
          res <- paste0('eprint: ', paper$eprint)
        }
        addPeriod(res)
      }
    })
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
    fmtChapter <- label(prefix = '. Chap. ')

    fmtISSN <- label(prefix = 'ISSN: ', suffix = '.')
    fmtISBN <- label(prefix = 'ISBN: ', suffix = '.')
    fmtISRN <- label(prefix = 'ISRN: ', suffix = '.')
    #' @importFrom util URLencode
    fmtDOI <- switch(docstyle, html = function(doi){
                            if (length(doi)){
                              paste0("DOI: \\href{https://doi.org/",
                                     URLencode(doi, reserved = TRUE), "}{", doi, "}.")
                            }
                          }, markdown = function(doi){
                              if (length(doi)){
                                paste0("DOI: [", doi, "](https://doi.org/",
                                       URLencode(doi, reserved = TRUE), ").")
                              }
                            }, label(prefix = 'DOI: ', suffix = '.'))

    fmtURL <- function(paper){
      if (length(paper[['url']])){
        res <- paper$url
        res <- switch(docstyle, html = paste0("URL: \\url{", res, "}"),
                      markdown = paste0("URL: [", res, "](", res, ")"),
                      paste0('\\url{', res, '}'))
        if (length(paper$urldate)){
          fDate <- try(ProcessDate(paper$urldate, NULL), TRUE)
          if (!is.null(fDate) && !inherits(fDate, 'try-error'))
            res <- paste0(res, ' (visited on ', DateFormatter(fDate, TRUE), ')')
        }
        addPeriod(res)
      }else if (length(paper$urldate)){
        fDate <- try(ProcessDate(paper$urldate, NULL), TRUE)
        if (!is.null(fDate) && !inherits(fDate, 'try-error'))
          paste0('(Visited on ', DateFormatter(fDate, TRUE), ').')
      }
    }

    addPeriod <- function (string){
      sub("([^.?!])$", "\\1.", string, useBytes = TRUE)
    }

    authorList <- function(aut){
        names <- vapply(aut, shortName, "")
        if (length(names) > 1L){
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
          res <- paste0(switch(tolower(job), 'compiler' = 'Comp. by ',
                               'editor' = 'Ed. by ',
                               'founder' = 'Found. by ',
                               'continuator' = 'Cont. by ',
                               'redactor' = 'Red. by ', 'reviser' = 'Rev. by ',
                               'collaborator' = 'In collab. with ', job), nom)
        }else{
          res <- paste0('Ed. by ', nom)
        }
        paste0(prefix, res, suffix)
      }
    }

    shortNameLF <- function(pers){
      fam <- pers$family
      lfam <- length(fam)
      if (lfam) {
          von <- lfam > 1L && substr(fam[1L], 1L, 1L) %in% letters
          if (von){
            res <- cleanupLatex(fam[2L:lfam])
          }else{
            res <- cleanupLatex(fam)
          }
          if (length(pers$given)){
            if (.BibOptions$first.inits){
              res <- paste0(paste(res, paste(substr(vapply(pers$given,
                                                           cleanupLatex, ""),
                                                    1L, 1L), collapse = ". "),
                                  sep = ", "), ".")

            }else{
                res <- paste(res, paste(vapply(pers$given, cleanupLatex, ""),
                                        collapse = " "), sep = ", ")
            }
          }
          if (von)
              res <- paste(res, cleanupLatex(fam[1L]))
          res
      }else{
        paste(cleanupLatex(pers$given), collapse = " ")
      }
    }

    sortKeys <- function(bib){
        result <- character(length(bib))
        for (i in seq_along(bib)) {
          authors <- bib[[i]]$sortname
          if (!length(authors))
            authors <- paste0(vapply(bib[[i]]$author, shortNameLF, ""),
                              collapse = '')
          if (authors == ''){
            authors <- paste0(vapply(bib[[i]]$editor, shortNameLF, ""),
                              collapse = '')
            if (authors == '')
                authors <- paste0(vapply(bib[[i]]$translator, shortNameLF, ""),
                                  collapse = '')
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
            if (is.null(res)){
              res <- 9999
            }else if (inherits(res, 'Interval')){
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
          res <- "0000"
        }else{
          tmp <- suppressWarnings(as.numeric(res))
          res <- if (!is.na(tmp))
                     sprintf("%04d", tmp)
                 else res
        }
        result[i] <- res
      }
      result
    }

    sortKeysLA <- function(bib, yrs){
        result <- character(length(bib))
        max.names <- .BibOptions$max.names
        for (i in seq_along(bib)) {
            res <- bib[[i]]$shorthand
            if (!length(res)){
              res <- bib[[i]]$label
              if (!length(res))
                res <- ProcessNamesLA(bib[[i]]$shortauthor, max.names)
              if (!length(res))
                res <- ProcessNamesLA(bib[[i]]$shorteditor, max.names)
              if (!length(res))
                res <- ProcessNamesLA(bib[[i]]$author, max.names)
              if (!length(res))
                res <- ProcessNamesLA(bib[[i]]$editor, max.names)
              if (!length(res))
                res <- ProcessNamesLA(bib[[i]]$translator, max.names)
              res <- paste0(res, substr(yrs[i], 3, 4))
            }
            result[i] <- res
        }
        result
    }

    ProcessNamesLA <- function(nam, mn = .BibOptions$max.names){
      nam.len <- length(nam)
      if (nam.len){
        if (!inherits(nam, 'person'))
          nam <- ArrangeAuthors(nam)
        if (nam.len == 1){
          res <- paste0(nam$family, collapse = '')
          res <- regmatches(res,
                            regexpr(paste0("[[:upper:]][[:punct:]]?[[:alpha:]]",
                                           "[[:punct:]]?[[:alpha:]]"),
                                         res, useBytes = TRUE))
          res <- gsub('[[:punct:]]', '', res, useBytes = TRUE)
        }else if (nam.len == 2 || nam.len == 3){
          res <- vapply(nam$family, paste0, "", collapse = '')
          res <- paste0(regmatches(res, regexpr('[[:upper:]]', res,
                                                useBytes = TRUE)),
                        collapse = '')
        }else{
          res <- paste0(nam$family[[1]], collapse = '')
          res <- regmatches(res,
                            regexpr(paste0("[[:upper:]][[:punct:]]?[[:alpha:]]",
                                           "[[:punct:]]?[[:alpha:]]"),
                                         res, useBytes = TRUE))
          res <- gsub('[[:punct:]]', '', res, useBytes = TRUE)
          res <- paste0(res, '+')
        }
        res
      }
    }

    GetLastNames <- function(bib){
        result <- character(length(bib))
        for (i in seq_along(bib)) {
          authors <- paste0(unlist(bib[[i]]$author$family), collapse = '')
          if (authors == "")
            authors <- paste0(unlist(bib[[i]]$editor$family), collapse = '')
          if (authors == "")
            authors <- paste0(unlist(bib[[i]]$translator$family), collapse = '')
          if (authors == "")
            authors <- cleanupLatex(bib[[i]]$title)
          if (!length(authors))
            authors <- ""
          result[i] <- authors
        }
        result
    }

    environment()
}
