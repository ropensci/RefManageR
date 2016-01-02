#' @keywords internal
ReadFirstPages <- function(doc, page.one = TRUE){
  doc <- unlist(doc)
  res <- list()
  found.abstract <- FALSE

  # arXiv
  arXiv <- FALSE
  if (TRUE){
    ind <- grep('arXiv:', doc, useBytes = TRUE)[1]
    if(!is.na(ind)){
      arXiv <- TRUE
      found.abstract <- TRUE  # assume no cover page for an arXiv paper
      arxinfo <- doc[ind]
      res$eprinttype <- 'arxiv'
      # need to check date since arXiv identifier format changed in Apr-07
      m <- regexpr('[0-9]{1,2}[[:space:]][A-Z][a-z]{2}[[:space:]][0-9]{4}', arxinfo,
                   useBytes = TRUE)

      adate <- strptime(regmatches(arxinfo, m), format='%d %b %Y')
      if (length(adate)){
          res$date <- adate
          if (adate >= strptime('01 Apr 2007', format='%d %b %Y')){
              m <- regexec('arXiv:([0-9]{4}[\\.][0-9]{4}v[0-9])[[:space:]]\\[([[:graph:]]+)\\]',
                           arxinfo, useBytes = TRUE)
            regm <- regmatches(arxinfo, m)
            res$eprintclass <- regm[[1]][3]
            res$eprint <- regm[[1]][2]
          }else{
            m <- regexec('arXiv:([[:graph:]]+)\\s', arxinfo, useBytes = TRUE)
            regm <- regmatches(arxinfo, m)
            res$eprint <- regm[[1]][2]
          }
          res$url <- paste0('http://arxiv.org/abs/', res$eprint)
      }
    }
  }

  if (TRUE){
    if (!arXiv){  # try to get url
      ind <- grep('^[Uu]Rr][Ll]: ', doc, useBytes = TRUE)
      if (length(ind))
        res$url <- gsub('^[Uu]Rr][Ll]: ', '', doc[ind], useBytes = TRUE)


      # volume
      m <- regexec('(Vol|Volume)[[:punct:]]?[[:space:]]?([0-9]+)', doc, useBytes = TRUE)
      if (length(m[[1]]) != 1)
        res$volume <- unlist(regmatches(doc, m))[3]

      # number
      m <- regexec('([Nn]o\\.|Number|Issue)[[:space:]]([0-9]{1,3})', doc, useBytes = TRUE)
      if (length(m[[1]]) != 1)
        res$number <- unlist(regmatches(doc, m))[3]
      # pages -pdftotext has trouble with "-"
      m <- regexec('([0-9]{1,4}) ?[-\u1390\u2212\ufe58\ufe63\uff0d\u2012-\u2015] ?([0-9]{1,4})[[:punct:]]?$',
                   doc, useBytes = TRUE)
      tmatch <- unlist(regmatches(doc, m))
      if (length(tmatch))
        res$pages <- paste0(tmatch[-1], collapse='-')


      # make lame, conservative attempt to get journal
      journ.ind <- regexec('^([[:alpha:] -]{2,})[,\\.;]?[[:print:]]*\\(?\\<((19|20)[0-9]{2})\\>',
                           doc[1], useBytes = TRUE)  # [[:upper:]][[:alpha:]]+
      if (length(journ.ind[[1]]) != 1){
        temp <- regmatches(doc[1], journ.ind)[[1]]
        res$journal <- gsub(' $', '', temp[2], useBytes = TRUE)
        res$year <- temp[3]
        doc <- doc[-1L]
      }

      # year
      if (is.null(res$year)){
        m <- regexpr('\\<(19|20)[0-9]{2}\\>', doc, useBytes = TRUE)
        if (any(m != -1L))
          res$year <- regmatches(doc, m)[1L]
      }
    }

    # keywords
    ind <- grep('Key ?words( and phrases)?(:|.)[[:space:]]?', doc, ignore.case = TRUE,
                useBytes = TRUE)
    if (length(ind)){
        res$keywords <- sub('Key ?words( and phrases)?(:|.)[[:space:]]?', '', doc[ind],
                            ignore.case = TRUE, useBytes = TRUE)
        if (ind+1 <= length(doc) && grepl('^([[:alpha:]]+[,;]? ?)+\\.?$', doc[ind+1],
                    useBytes = TRUE))
          res$keywords <- paste(res$keywords, doc[ind+1])
        if (ind+2 <= length(doc) && grepl('^([[:alpha:]]+[,;]? ?)+\\.?$', doc[ind+2],
                    useBytes = TRUE))
          res$keywords <- paste(res$keywords, doc[ind+2])
        ## keywords need to be comma separated for BibLaTeX
        res$keywords <- gsub(';', ',', res$keywords, useBytes = TRUE)
        res$keywords <- gsub('[;,]$', '', res$keywords, useBytes = TRUE)
        doc <- doc[1L:(ind-1)]  # shorten doc used to search for author and title
        found.abstract <- TRUE
    }else if(length(abst.ind <- grep('^[1I]\\.?[[:blank:]]Introduction([[:space:]]|$)', doc,
                                     useBytes = TRUE))){
      if (abst.ind > 2L){
        doc <- doc[1L:(abst.ind - 1L)]
        if (page.one)  # allows for first section at top of 2nd page
          found.abstract <- TRUE
      }
    }

    temp <- try(GetAuthorTitle(doc, found.abstract, res$keywords))
    if(inherits(temp, 'try-error'))
      return(NA)

    res[names(temp)] <- temp
    if (length(ind))
      res$found.abstract <- TRUE
  }  # end JSTOR if


  return(res)
}

#' @keywords internal
CheckJSTOR <- function(doc1, doc2, file){
  ind <- grep('http://www\\.jstor\\.org/stable/([0-9]+)', doc1, useBytes = TRUE)[1]
  if (!is.na(ind)){
    res <- try(GetJSTOR(doc1), TRUE)
    if (inherits(res, 'try-catch'))
      return(NA)
    res$eprint <- gsub('[^0-9]+', '', doc1[ind], useBytes = TRUE)
    res$eprinttype = 'jstor'
    res$url <- paste0('http://www.jstor.org/stable/', res$eprint)
    res$file <- normalizePath(file)

  }else if (length(ind <- grep('http://links.jstor.org/sici', doc1, useBytes = TRUE))){
    ## old format for JSTOR papers
    res <- try(GetJSTOR(doc1), TRUE)
    if (inherits(res, 'try-catch'))
      return(NA)
    res$url <- sub('Stable URL:[[:space:]]', '', doc1[ind], useBytes = TRUE)
  }else{
    return(NA)
  }

  if (!is.null(res$journal)){
    attr(res, 'entry') <- 'article'
  }else{
    attr(res, 'entry') <- 'misc'
  }

  attr(res, 'key') <- CreateBibKey(res$title, res$author, res$year)


  ##########################################
  # try for keywords and DOI on page 2
  ind <- grep('Key ?words( and phrases)?:[[:space:]]?', doc2, ignore.case = TRUE, useBytes = TRUE)

  if (length(ind)){
      res$keywords <- sub('[[:space:]]*Key ?words( and phrases)?:[[:space:]]?', '', doc2[ind],
                          ignore.case = TRUE, useBytes = TRUE)
      if (ind+1 <= length(doc2) && grepl('^([[:alpha:]]+[ ,;]?)+\\.?$', doc2[ind+1],
                  useBytes = TRUE))
        res$keywords <- paste(res$keywords, doc2[ind+1])
      if (ind+2 <= length(doc2) && grepl('^([[:alpha:]]+[ ,;]?)+\\.?$', doc2[ind+2],
                  useBytes = TRUE))
        res$keywords <- paste(res$keywords, doc2[ind+2])
      ## keywords need to be comma separated for BibLaTeX
      res$keywords <- gsub(';', ',', res$keywords, useBytes = TRUE)
  }

  pattern  <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
  m <- regexpr(pattern, doc2, perl=TRUE, useBytes = TRUE)
  if (any(m != -1))
    res$doi <- unlist(regmatches(doc2, m))

  return(res)
}

#' @keywords internal
GetJSTOR <- function(doc){  # take extra caution for long title, author list, or journal info
  aut.ind <- grep('Author\\(s\\): ', doc, useBytes = TRUE)
  if (!length(aut.ind)){  # old format for JSTOR papers.
    url.ind <- grep('Stable URL', doc, useBytes = TRUE)
    source.ind <- grep('([[:print:]]+),[[:space:]]Vol\\. ', doc, useBytes = TRUE)

    # immediately above source.ind is author, if line above that has a semicolon, it is also author info
    aut.ind <- grep(';', doc[1:(source.ind-1)], useBytes = TRUE)
    if(!is.na(aut.ind)){
      author <- paste0(doc[aut.ind:(source.ind-1)], collapse = ' ')
    }else{
      aut.ind <- source.ind-1
      author <- doc[aut.ind]
    }
    author <- gsub(';', ',', author, useBytes = TRUE)
    author <- as.person(author)

    publisher.ind <- grep('published by', doc, useBytes = TRUE)[1]
    m <- regexec('published by ([[:alpha:], -]+).?$', doc[publisher.ind], useBytes = TRUE)
    publisher <- regmatches(doc[publisher.ind], m)[[1]][2]

    jinfo <- paste0(doc[source.ind:(url.ind-1)], collapse = ' ')
  }else{
    reviewed.ind <- grep('Reviewed work\\(s\\):', doc, useBytes = TRUE)
    if (length(reviewed.ind))
      doc <- doc[-reviewed.ind]

    publisher.ind <- grep('Published by: ', doc, useBytes = TRUE)
    url.ind <- grep('Stable URL: ', doc, useBytes = TRUE)
    publisher <- sub('Published by: ', '',
                     paste0(doc[publisher.ind:(url.ind-1)], collapse = ' '), useBytes = TRUE)
    source.ind <- grep('Source: ', doc, useBytes = TRUE)
    author <- paste0(doc[aut.ind:(source.ind-1)], collapse = ' ')
    author <- sub('Author\\(s\\): ', '', author, useBytes = TRUE)
    author <- as.person(author)

    jinfo <- paste0(doc[source.ind:(publisher.ind-1)], collapse = ' ')
  }
  # on rare occasions there is text before title which is followed by a blank line
  blank.ind <- which(doc[1:aut.ind] == '')
  if (!length(blank.ind))
    blank.ind <- 0
  title <- paste0(doc[(blank.ind+1):(aut.ind-1)], collapse=' ')

  pattern <- paste0('^(Source:[[:space:]])?([[:print:]]+),[[:space:]]Vol\\. ([0-9]+)(, No\\. ([[:digit:]/]+))?\\.? ',
    '\\(([[:upper:]][[:lower:]]{2}\\.?, )?([0-9]{4})\\), pp. ([[:digit:] -]+)')
  m <- regexec(pattern, jinfo, useBytes = TRUE)
  journal.info <- unlist(regmatches(jinfo, m))
  if (!length(journal.info))  # Source: line has unexpected format
    return(list(title = title, author = author, publisher = publisher))

  journal <- journal.info[3]
  volume <- journal.info[4]
  if ((number <- journal.info[6]) == '')
    number <- NULL  # some journals have no number/issue

  year <- journal.info[8]  # journal.info[length(journal.info)-1]
  ## gsub for edge case # gsub('pp. ', '', journal.info[length(journal.info)])
  pages <- gsub(' ', '', journal.info[9], useBytes = TRUE)
  ## poppler doesn't read the JSTOR en-dash always
  if(!grepl('-', pages, useBytes = TRUE)){
    psplit <- floor(nchar(pages)/2)
    pages <- paste0(substr(pages, 1, psplit), '-', substr(pages, psplit+1, nchar(pages)))
  }

  return(list(title = title, author = author, journal = journal, volume = volume, number = number,
              pages = pages, year = year, publisher = publisher))
}

#' @keywords internal
GetAuthorTitle <- function(doc, found.abstract, kw){
  if (!identical(found.abstract, TRUE)){
    abst.ind <- grep('^A[Bb][Ss][Tt][Rr][Aa][Cc][Tt]|^S[Uu][Mm][Mm][Aa][Rr][Yy]|^S[Yy][Nn][Oo][Pp][Ss][Ii][Ss][:.]?\\>', doc, useBytes = TRUE)
    if (length(abst.ind) && abst.ind > 2L){  # assume title/author comes before Abstract. need 2nd cond. for ind==1
      doc <- doc[1L:(abst.ind - 1L)]
      found.abstract <- TRUE
    }
  }

  BAD.WORDS <- paste0('Online|Supplement|[Dd]ata|University|College|Centre|Center|Working|Faculty|Science',
                      '|\\<Univ\\>|\\<of\\>|\\<the\\>|Foundation|Analysis|Series|Paper|\\<url\\>|Research|Labs',
                      '|Institute|School|Technical|Department|Staff|\\<to\\>|\\<in\\>')
  aut.ind <- regexpr(paste0(# invalid words negate match
    #  "(?!", BAD.WORDS,  ")",
    "^((B[Yy]|A[Uu][Tt][Hh][Oo][Rr][Ss]?|and):?[[:space:]])?",
    "([[:upper:]][[:alpha:]]*[\\.]?[ -]",            # first name, maybe hypenated or abbrev.
    "([[:alpha:]]*[\\.]?[ -]){0,3}",           # optional middle name or initial, maybe hypenated
    #"([[:upper:]][[:alpha:]]*[\\.]?[ -])*",           # optional middle name or initial, maybe hypenated
    "[[:upper:]][[:alpha:]'-]+([[:space:]]?[^[:alnum:]])?",        # last name + potential extra char to
    "(,? Jr| II| III| IV)?(,? MD.?)?(,? P(h|H)D.?)?",  # optional qualifications
    #  "(?<!", BAD.WORDS, ")",
    "(,.|;.)*( and| &)?[[:space:]]?)+$"),             # and, ",", ";", or "&" to seperate names. Repeat
    doc[-1], perl=FALSE, useBytes = TRUE) # first line can't have authors
  aut.match <- regmatches(doc[-1], aut.ind)
  if (length(aut.match) == 0){
    aut.match <- NULL
  }else{
      ## remove MD and PhD
      aut.match <- gsub("(,? MD)?(,? P(H|h)D)?", '', aut.match, useBytes = TRUE)
      ## remove punct at end of last name
      aut.match <- gsub("[^[:alpha:] ,'-]", '', aut.match, useBytes = TRUE)
    # aut.match <- gsub("^A[Uu][Tt][Hh][Oo][Rr]( |: )|^B[Yy]( |: )", '', aut.match)  # remove author or by at start
      aut.match <- gsub("^((B[Yy]|A[Uu][Tt][Hh][Oo][Rr][Ss]?|and):?[[:space:]])?", '',
                        aut.match, useBytes = TRUE)  # remove author or by at start
    aut.match <- gsub("\\<AND\\>", "and", aut.match, useBytes = TRUE)
    # remove bad words. can't get negative look-ahead working :(
    temp <- grep(BAD.WORDS, aut.match, ignore.case = TRUE, useBytes = TRUE)
    if (length(temp)){
      aut.ind[which(aut.ind > 0L)[temp]] <- -1L
      aut.match <- aut.match[-temp]
    }
    if (!is.null(kw)){  # extra protection from including title with author
        temp <- grep(paste0(unlist(strsplit(kw, ',? ')), collapse = '|'), aut.match,
                     ignore.case = TRUE, useBytes = TRUE)
      if (length(temp)){
        aut.ind[which(aut.ind > 0L)[temp]] <- -1L
        aut.match <- aut.match[-temp]
      }
    }
  }

  match.ind <- which(aut.ind > -1L) + 1  # +1 because had doc[-1] above
  # if didn't find abstract, make attempt at not including names from doc body
  if (!found.abstract && length(aut.match) > 1L){
    spaces <- diff(match.ind)
    first.too.big <- which(match.ind > 2L)[1]
    if (!is.na(first.too.big))
      aut.match <- aut.match[seq_len(first.too.big)]
  }

  BAD.WORDS <- paste0('\\bSupplement\\b|University|\\bCollege\\b|\\bCentre\\b|\\bCenter\\b|Working|Faculty',
                      '|Paper|\\b[Uu][Rr][Ll]\\b|Labs|\\bJournal\\b|Institute|\\bSchool\\b')
  if (length(match.ind)){  # if found author, assume title comes before author
    ind <- match.ind[1]
    doc <- doc[(ind-1L):1L]  # reverse doc, assume title comes just before authors
  }

  # starting either author match and going backwards, or starting from line 1, search for title
  # have two flags to allow for multiline titles
  N <- length(doc)
  i <- 1
  first.match <- FALSE
  done.match <- FALSE
  while (i <= N && !done.match){
    temp <- grep(BAD.WORDS, doc[i], ignore.case = TRUE, useBytes = TRUE)
    title.ind <- regexpr(paste0(  # "(?!", BAD.WORDS, ")",
                                "^[\u201c\u022]?[[:upper:]][[:alpha:]'\u201c\u201d\u022-]+[ -]",
                                #"([[:alpha:]:,' ]){2,}(\\.|!|\\?)?$"),
                                "([[:alpha:]:,' \u201c\u201d\u022-]+){2,}.?$"),
                                #"(?<!", BAD.WORDS, ")"),
                         doc[i], perl = TRUE, useBytes = TRUE)
    if (title.ind != -1 && !length(temp)){
      if (!first.match){
        first.match <- TRUE
        title.match <- regmatches(doc[i], title.ind)
      }else{
        title.match <- c(title.match, regmatches(doc[i], title.ind))
      }
    }else if (first.match){
      done.match <- TRUE
    }
    i <- i + 1
  }

  # title.match <- regmatches()
  if (!first.match){
    title.match <- NULL
  }else{
    if (length(match.ind)){  # undo reversing of doc when author matched
      if (i > 3L){
        if (grepl("[[:alpha:]', -]+", doc[i-3L], useBytes = TRUE))
          title.match <- c(doc[i-3L], title.match)
      }
      title.match <- rev(title.match)
    }
    title.match <- paste0(title.match, collapse = ' ')
    # simple fix for case troubles
    title.match <- gsub("([[:alpha:]])([[:alpha:]'-]*)", "\\U\\1\\L\\2", title.match,
                        perl = TRUE, useBytes = TRUE)
    ## remove superscripted char indicating footnote for title
    title.match <- gsub('[^\\w]*$', '', title.match, perl = TRUE, useBytes = TRUE)
  }

  #return(list(ind=which(aut.ind != -1L), match=aut.match, ab.ind=ind, title.match))
  return(list(author = aut.match, title = paste0(title.match, collapse = ' '),
              found.abstract = found.abstract))
}


#' @keywords internal
CleanAuthorTitle <- function(bib1, bib2, bibMeta, file){
  # browser()
  if (!is.null(bibMeta)){ # Don't let Metadata date overwrite year from pdf text
    if (!is.null(bib1$year) || !is.null(bib2$year))
      bibMeta$date <- NULL
  }
  if (bib2$found.abstract && (!is.null(bib2$author) || !is.null(bib2$title))){
    if(!is.null(bibMeta))
      bib1 <- AddListToList(bib1, bibMeta)
    bib <- AddListToList(bib2, bib1)
  }else{
    if(!is.null(bibMeta))
      bib2 <- AddListToList(bib2, bibMeta)
    bib <- AddListToList(bib1, bib2)
  }

  if (is.null(bib$author)){
    if (is.null(bib$title)){
      message('Could not retrieve author or title info for the following file, it will not be added:')
      message(file)
      return(NA)
    }else{
      message('Could not retrieve author info for the following file, it needs to be checked:')
      message(file)
    }
  }else{
    bib$author <- as.person(bib$author)
    if (is.null(bib$title)){
      message('Could not retrieve title info for the following file, it needs to be checked:')
      message(file)
    }
  }
  if (!is.null(bib$journal) && !is.null(bib$title)  && !is.null(bib$author)){
    attr(bib, 'entry') <- 'article'
  }else{
    attr(bib, 'entry') <- 'misc'
  }
  attr(bib, 'key') <- CreateBibKey(bib$title, bib$author, bib$year)
  bib$file <- normalizePath(file)
  bib$found.abstract <- NULL

  return(bib)
}

#' @keywords internal
SearchDOIText <- function(txt){
  pattern  <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
  m <- regexpr(pattern, txt, perl = TRUE, useBytes = TRUE)
  if(all(m == -1)){
    return(NA)
  }else{
    return(regmatches(txt, m)[1])
  }
}

#' @keywords internal
ProcessPDFMeta <- function(x, enc = 'UTF-8'){
  res <- list()
  a.check <- t.check <- NULL
  Encoding(x) <- enc
  tags <- c("Title", "Author")
  re <- sprintf("^(%s)", paste(sprintf("%-16s", sprintf("%s:",
                                                        tags)), collapse = "|"))
  found.tags <- substring(x, 1L, 16L)
  ind <- pmatch(tags, found.tags)
  if (!is.na(ind[1])){
    title.info <- sub(re, "", x[ind[1]], useBytes = TRUE)
    if (grepl('[[:upper:]][[:lower:]-]* [[:alpha:]]+', title.info, useBytes = TRUE))
      res$title <- title.info
  }

  if (!is.na(ind[2])){
    aut.info <- sub(re, "", x[ind[2]], useBytes = TRUE)
    if (grepl("\\w[\\.'-]? \\w", aut.info, useBytes = TRUE))
      res$author <- aut.info
  }

  # add keywords if available
  ind <- pmatch('Keywords', found.tags)
  if(!is.na(ind)){
    res$keywords <- sub('Keywords:[[:space:]]+', '', x[ind], useBytes = TRUE)
    if (res$keywords =='')
      res$keywords <- NULL
  }

  ind <- pmatch('Subject:', found.tags)
  if (!is.na(ind)){
    subj <- sub('Subject:[[:space:]]+', '', x[ind], useBytes = TRUE)

    if (subj != ''){
      res <- c(res, ProcessPDFSubject(subj))
    }
  }


  # if year not in Subject, use ModDate or CreationDate
  if (is.null(res$year)){
    ind <- pmatch('ModDate', found.tags)
    if(!is.na(ind)){
      date <- sub('ModDate:[[:space:]]+', '', x[ind], useBytes = TRUE)
      date <- suppressWarnings(lubridate::parse_date_time(date,
                                             orders = c("%m/%d/%y %H:%M:%S", "%m/%d %H:%M:%S %y")))
      if (!is.na(date)){
        res$year <- year(date)
        res$date <- trunc(date, 'days')
      }
    }else if (!is.na(ind <-pmatch('CreationDate', found.tags))){
      date <- sub('CreationDate:[[:space:]]+', '', x[ind], useBytes = TRUE)
      date <- suppressWarnings(lubridate::parse_date_time(date,
                                     orders = c("%m/%d/%y %H:%M:%S", "%m/%d %H:%M:%S %y")))
      if (!is.na(date)){
        res$year <- year(date)
        res$date <- trunc(date, 'days')
      }
    }
  }

  if (length(res))
    return(res)
  else return(NA)
}

#' @keywords internal
ProcessPDFSubject <- function(subj){
  res <- list()

  journ.ind <- regexec('^([[:upper:]][[:alpha:] ]+)[,\\.;]?[[:print:]]*\\(?\\<(19|20)[0-9]{2}\\>',
                       subj, useBytes = TRUE)
  temp <- unlist(regmatches(subj, journ.ind))
  if (length(temp)){
    res$journal <- temp[2]
    if (temp[3] != '')
      res$year <- temp[3]
  }

  m <- regexec('[Vv]ol(\\.|ume)?[[:space:]]([0-9]{1,3})', subj, useBytes = TRUE)
  if (length(m[[1]]) != 1)
    res$volume <- unlist(regmatches(subj, m))[3]

  m <- regexec('([Nn]o\\.|Number|Issue)[[:space:]]([0-9]{1,3})', subj, useBytes = TRUE)
  if (length(m[[1]]) != 1)
    res$number <- unlist(regmatches(subj, m))[3]

  if (is.null(res$volume) && is.null(res$number)){
    m <- regexec('\\(([0-9]{1,3})\\)[[:blank:]]?([0-9]{1,3})', subj, useBytes = TRUE)[[1]]
    if (length(m[[1]]) != 1){
      temp <- regmatches(subj, m)[[1]]
      res$volume <- temp[2]
      res$number <- temp[3]
    }
  }

  # be extra careful matching hypen. usuallly it's \u2013, an en dash
  m <- regexpr('[0-9]+[-\u2212\ufe58\ufe63\uff0d\u2012-\u2015][0-9]+', subj, useBytes = TRUE)
  if (m != -1){
      res$pages <- grep('[0-9]+[-\u2212\ufe58\ufe63\uff0d\u2012-\u2015][0-9]+', subj,
                        value = TRUE, useBytes = TRUE)
  }
  return(res)
}

#' @keywords internal
AddListToList <- function(list1, list2){
  c1 <- is.na(list1) || length(list1)==0
  c2 <- is.na(list2) || length(list2)==0

  if (c1 && c2)
    return(NA)

  if (c1)
    return(list2[list2 != ''])

  if (c2)
    return(list1[list1 != ''])

  list1[list1 == ''] <- NULL
  list2[list2 == ''] <- NULL
  ind <- !names(list2) %in% names(list1)
  if(sum(ind))
    list1[names(list2)[ind]] <- list2[ind]
  return(list1)
}
