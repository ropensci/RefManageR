ReadFirstPages <- function(doc, file){
  res <- list()
  # JSTOR
  JSTOR <- FALSE
  ind <- grep('http://www\\.jstor\\.org/stable/([0-9]+)', doc)[1]
  if (!is.na(ind)){
    JSTOR <- TRUE
    res <- try(GetJSTOR(doc))
    if (inherits(res, 'try-catch'))
      return(NA)
    res$eprint <- gsub('[^0-9]+', '', doc[ind])
    res$eprinttype = 'jstor'
    res$url <- paste0('http://www.jstor.org/stable/', res$eprint)
    attr(res, 'entry') <- 'article'
  }
  
  # arXiv
  arXiv <- FALSE
  if (!JSTOR){
    ind <- grep('(arXiv)', doc)[1]
    if(!is.na(ind)){
      arXiv <- TRUE
      arxinfo <- doc[ind]
      res$eprinttype <- 'arXiv'
      # need to check date since arXiv identifier format changed in Apr-07
      m <- regexpr('[0-9]{1,2}[[:space:]][A-Z][a-z]{2}[[:space:]][0-9]{4}', arxinfo)
      adate <- strptime(regmatches(arxinfo, m), format='%d %b %Y')
      if (adate >= strptime('01 Apr 2007', format='%d %b %Y')){
        m <- regexec('arXiv:([0-9]{4}[\\.][0-9]{4}v[0-9])[[:space:]]\\[([[:graph:]]+)\\]', arxinfo)
        regm <- regmatches(arxinfo, m)
        res$eprintclass <- regm[[1]][3]
        res$eprint <- regm[[1]][2]
      }else{
        m <- regexec('arXiv:([[:graph:]]+)\\s', arxinfo)
        regm <- regmatches(arxinfo, m)
        res$eprint <- regm[[1]][2]
      }
      res$url <- paste0('http://arxiv.org/abs/', eprint)
      attr(res, 'entry') <- 'online'
    }
  }
  
  if (!JSTOR){
    if (!arXiv){  # try to get url
      ind <- grep('^[Uu]Rr][Ll]: ', doc)
      if(length(ind))
        res$url <- gsub('^[Uu]Rr][Ll]: ', '', doc[ind])
    }
    
    # make lame, conservative attempt to get journal
   # browser()
    journ.ind <- regexec('^([[:upper:]][[:alpha:]]+)[,\\.;] [[:print:]]*(\\<(19|20)[0-9]{2}\\>)', doc[1])
    if (journ.ind != -1){
      temp <- regmatches(doc[1], journ.ind)[[1]]
      res$journal <- temp[2]
      res$year <- temp[3]
      attr(res, 'entry') <- 'article'
    }else{
      attr(res, 'entry') <- 'misc'
    }
    
    # volume
    #volume <- grep('(?:Vol|Volume)[[:punct:]]?[[:space:]]?[0-9]+', doc2, value=TRUE)[1]
    res$volume <- grep('(Vol|Volume)[[:punct:]]?[[:space:]]?[0-9]+', doc, value=TRUE)[1]
    if (!is.na(volume)){
      res$volume <- gsub('(Vol|Volume)[[:punct:]]?[[:space:]]?', '', volume)
    }else{
      res$volume <- NULL
    }
    
    # number
    res$number <- grep('(No|Number)[[:punct:]]?[[:space:]]?[0-9]+', doc, value=TRUE)[1]
    if (!is.na(number)){
      res$number <- gsub('(No|Number)[[:punct:]]?[[:space:]]?', '', number)
    }else{
      res$number <- NULL
    }
    
    # year
    # year <- grep('[(]?(19|20)[0-9]{2}[)]?', doc3, value=TRUE)[1]
    if (is.null(res$year)){
      m <- regexpr('\\<(19|20)[0-9]{2}\\>', doc)
      if(any(m != -1))
        res$year <- regmatches(doc, m)[1]
    }
    
    #browser()
    temp <- try(GetAuthorTitle(doc))
    if(inherits(temp, 'try-error')){
      message('Error reading file')
      message(file)
      return(NA)
    }
    if (is.null(temp$author)){
      if (is.null(temp$title)){
        message('Could not retrieve author or title info for the following file, it will not be added:')
        message(file)
        return(NA)
      }else{
        message('Could not retrieve author info for the following file, it needs to be checked:')
        res$title <- temp$title
        m <- regexpr('\\<([[:alpha:]]{4,})\\>', temp$title)
        if(m != -1){
          key.title <- regmatches(title, m)
          attr(res, 'key') <- paste0(key.title, res$year)
        }
        message(file)
      }
    }else{
      res$author <- as.person(temp$author)
      if (is.null(temp$title)){
        message('Could not retrieve title info for the following file, it will not be added:')
        message(file)
        attr(res, 'key') <- paste0(res$author[1]$family[1], res$year)
      }else{
        res$title <- temp$title
        m <- regexpr('\\<([[:alpha:]]{4,})\\>', title)
        if(m != -1){
          key.title <- regmatches(res$title, m)
          attr(res, 'key') <- paste0(res$author[1]$family[1], res$year, key.title)  
        }
        attr(res, 'key') <- paste0(res$author[1]$family[1], res$year)
      }
    }
    
  }  # end JSTOR if
  # keywords
  ind <- grep('K[Ee][Yy][[:space:]]?[Ww][Oo][Rr][Dd][Ss]:[[:space:]]*', doc)
  # m <- regexpr('Keywords:[[:space:]]*[A-Za-z]+\\s', doc, perl=TRUE)
  # regmatches(doc, m)
  if(length(ind)){
    res$keywords <- sub('K[Ee][Yy][[:space:]]?[Ww][Oo][Rr][Dd][Ss]:[[:space:]]*', '', doc[ind]) 
    res$keywords <- gsub(';', ',', res$keywords)  # keywords need to be comma separated for BibLaTeX
  }
  
  return(res)
}


GetJSTOR <- function(doc){  # take extra caution for long title, author list, or journal info
  aut.ind <- grep('Author\\(s\\): ', doc)
  
  title <- doc[1:(aut.ind-1)]
  
  reviewed.ind <- grep('Reviewed work\\(s\\):', doc)
  if (length(reviewed.ind))
    doc <- doc[-reviewed.ind]
  
  publisher.ind <- grep('Published by: ', doc)
  source.ind <- grep('Source: ', doc)
  author <- paste0(doc[aut.ind:(source.ind-1)], collapse = '')
  author <- gsub('Author\\(s\\): ', '', author)
  
  journal.info <- strsplit(paste0(doc[source.ind:(publisher.ind-1)], collapse = ''),
                           '(?!\\.)?\\)?, ', perl = TRUE)[[1]]
  journal <- gsub('Source: ', '', journal.info[1])
  volume <- gsub('Vol. ', '', journal.info[2])
  # be more cautious with number
  if ((num.ind <- grep('No.', journal.info))){
    m <- regexpr('[0-9]+[[:punct:]]?[0-9]*[[:alpha:]]?', journal.info[num.ind])
    number <- regmatches(journal.info[num.ind], m)
  }
  year <- journal.info[length(journal.info)-1]
  pages <- gsub('pp. ', '', journal.info[length(journal.info)])
  publisher <- gsub('Published by: ', '', doc[publisher.ind])
  
  return(list(title = title, author = author, journal = journal, volume = volume, number = number, 
              pages = pages, year = year, publisher = publisher, eprint = eprint, eprinttype = eprinttype))
}

GetAuthorTitle <- function(doc){
  ind <- grep('^A[Bb][Ss][Tt][Rr][Aa][Cc][Tt][:.]?\\>', doc)[1]
  if (!is.na(ind) && ind > 2L)  # assume title/author comes before Abstract. need 2nd cond. for ind==1
    doc <- doc[1L:(ind - 1L)]
  #  browser()
  #   aut.ind <- regexpr(paste0("^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
  #                       "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
  #                       "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
  #                       "(, )?(Jr)?(II)?(III)?(IV)?(, )?(MD)?(, )?(Ph|HD)?.?",  # optional qualifications      
  #                       "(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
  #                doc)
  
  #   aut.ind <- regexpr(paste0(# invalid words negate match
  #     "(?!Online|Supplement|Data|University|College|Institute|School)",
  #     "^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
  #     "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
  #     "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
  #     "(, Jr| II| III| IV)?(,? MD.?)?(,? P(h|H)D.?)?",  # optional qualifications      
  #     "(?<!Online|Supplement|Data|University|College|Institute|School)", 
  #     "(and)?([,;&$].?)?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
  #                      doc, perl=TRUE)
  
  aut.ind <- regexpr(paste0(# invalid words negate match
    "(?!Online|Supplement|Data|University|College|Institute|School)",
    "^([[:upper:]][[:lower:]]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
    "([[:upper:]][[:lower:]]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
    "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
    "(, Jr| II| III| IV)?(,? MD.?)?(,? P(h|H)D.?)?",  # optional qualifications      
    "(?<!Online|Supplement|Data|University|College|Institute|School)", 
    "(,.|;.)*( and| &)?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
                     doc, perl=TRUE)
  aut.match <- regmatches(doc, aut.ind)
  if (length(aut.match) == 0){
    aut.match <- NULL
  }else{
    aut.match <- gsub("(,? MD)?(,? P(H|h)D)?", '', aut.match)  # remove MD and PhD
    aut.match <- gsub("[^[:alpha:] ,'-]", '', aut.match)  # remove punct at end of last name
  }
  
  match.ind <- which(aut.ind > -1L)
  # if didn't find abstract, make attempt at not including names from doc body
  if (is.na(ind) && length(aut.match) > 1L){
    spaces <- diff(match.ind)
    first.too.big <- which(match.ind > 2L)[1]
    if (!is.na(first.too.big))
      aut.match <- aut.match[1L:first.too.big]
  }
  
  if (length(match.ind)){  # if found author, assume title comes before author, slightly less cautious
    ind <- match.ind[1]
    if (ind==1L)  # Assumed title has been missed
      return(list(author = NULL, title = NULL))
    doc <- doc[1L:(ind-1L)]
    title.ind <- regexpr(paste0("^[[:upper:]][[:alpha:]'-]*(,|-|:)?[ -]",
                                #"([[:alpha:]:,' ]){2,}(\\.|!|\\?)?$"),
                                "([[:alpha:]:,' ]){2,}$"),
                         doc)
  }else{
    title.ind <- regexpr(paste0("(?!Online|Supplement|Data|University|College|Institute|School|Keywords)",
                                "^[[:upper:]][[:alpha:]'-]*(,|-|:)?[ -]",
                                "([[:alpha:]:,' -]){2,}(\\.|!|\\?)?$",
                                "(?<!Online|Supplement|Data|University|College|Institute|School|Keywords)"),
                         doc, perl = TRUE)
  }
  title.match <- regmatches(doc, title.ind)
  if(!length(title.match)){
    title.match <- NULL
  }else{
    match.ind <- which(title.ind > -1L)
    if (length(match.ind) != 1L){
      spaces <- diff(match.ind)
      first.too.big <- which(spaces > 2L)[1]
      if (!is.na(first.too.big))
        title.match <- title.match[1:first.too.big]
    }
  }
  
  #return(list(ind=which(aut.ind != -1L), match=aut.match, ab.ind=ind, title.match))
  return(list(author = aut.match, title = paste0(title.match, collapse = ' ')))
}

SearchDOIText <- function(txt){
  if (length(txt) <= 1)
    return(NA)
  pattern  <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
  m <- regexpr(pattern, txt, perl=TRUE)
  if(all(m == -1)){
    return(NA)
  }else{
    return(regmatches(txt, m)[1])
  }
}

# SearchDOIFirstPage <- function(path, enc = encoding){
#   #browser()
#   system2("pdftotext", paste(shQuote('-l'), shQuote('2'), shQuote('-enc'), shQuote(enc), shQuote(normalizePath(path))))
#   tmp.file <- sub('.pdf', '.txt', path)
#   txt <- suppressWarnings(readLines(tmp.file, encoding = enc))
#   res <- SearchDOIMeta(txt)
#   file.remove(tmp.file)
#   return(c(doi = res, doc = txt))
# }

ProcessPDFMeta <- function(x, enc = 'UTF-8', path, check.doi = FALSE){
  #status <- system2("pdfinfo", args = c('-enc', encoding, shQuote(normalizePath(file))), 
  #                  stdout = outfile)
  # browser()
  res <- list()
  a.check <- t.check <- NULL
  Encoding(x) <- enc
  tags <- c("Title", "Author")
  #             , "Creator", "Subject"
  #             "Producer", "CreationDate", "ModDate", "Tagged", "Form", 
  #             "Pages", "Encrypted", "Page size", "File size", "Optimized", 
  #             "PDF version")
  re <- sprintf("^(%s)", paste(sprintf("%-16s", sprintf("%s:", 
                                                        tags)), collapse = "|"))
  #lines <- readLines(temp.file, warn = FALSE)
  # ind <- which(grepl(re, x))
  found.tags <- substring(x, 1L, 16L)
  ind <- pmatch(tags, found.tags)
  if (!is.na(ind[1])){
    title.info <- sub(re, "", x[ind[1]])
    t.check <- grep('[[:upper:]][[:lower:]-]* [[:alpha:]]+', title.info)
    if (length(t.check))
      res$title <- title.info
  }
  
  if (!is.na(ind[2])){
    aut.info <- sub(re, "", x[ind[2]])
    a.check <- grep("\\w[\\.'-]? \\w", aut.info)
    if (length(a.check))
      res$author <- aut.info
  }
  
  if (all(is.na(ind)) || (!length(t.check)  && !length(a.check))){
    message('Ignoring the following entry due both missing Title and Author metadata:')
    message(path)
    return(NA)
  }


  
  #tags <- sub(": *", "", substring(x, 1L, 16L))
  #info <- split(sub(re, "", x), cumsum(ind))
  #info <- sub(re, "", x)[ind]

  
  # add keywords if available
  ind <- pmatch('Keywords', found.tags)
  if(!is.na(ind))
    res$keywords <- sub('Keywords:[[:space:]]+', '', x[ind])
  
  ind <- pmatch('Subject:', found.tags)
  if (!is.na(ind)){
    subj <- sub('Subject:[[:space:]]+', '', x[ind])
    
    if (subj != ''){
      res <- c(res, ProcessPDFSubject(subj, check.doi))
    }
  }
  
  
  
  # if year not in Subject, use ModDate or CreationDate
  if (is.null(res$year)){
    ind <- pmatch('ModDate', found.tags)
    if(!is.na(ind)){
      res$date <- sub('ModDate:[[:space:]]+', '', x[ind])
      res$date <- strptime(res$date, format = "%m/%d/%y %H:%M:%S")
      if (!inherits(res$date, 'try-error')){
        res$year <- year(res$date)
        res$date <- trunc(res$date, 'days')
      }
    }else if (!is.na(ind <-pmatch('CreationDate', found.tags))){
      res$date <- sub('CreationDate:[[:space:]]+', '', x[ind])
      res$date <- try(strptime(res$date, format = "%m/%d/%y %H:%M:%S"), TRUE)
      if (!inherits(res$date, 'try-error')){
        res$year <- year(res$date)
        res$date <- trunc(res$date, 'days')
      }
    }
    # if year still NULL due to no ModDate or CreationDate, or wrong format of either: give up
    if (is.null(res$year)){
      message('Cannot determine year/date, the following entry will be ignored:')
      message(path)
      return(NA)      
    }
  }
  
 # res$file <- path
  
  # create key
  aut <- NULL
  if (length(a.check)){
    aut <- try(tolower(res$author[1]$family[1]), silent = TRUE)  # get last name of first author
    if (inherits(aut, 'try-error')){
      aut <- NULL
  #  res$author <- NULL
    }else{
      aut <- gsub(',', '', aut)  # remove trailing commas
    }
  }
  if (length(t.check)){
    first.word <- tolower(strsplit(res$title, ' ')[[1]][[1]])  # get first word of title
  }else{
    first.word <- 'missing'
  }
  attr(res, 'key') <- paste0(aut, res$year, first.word)
  
  if (!is.null(res$journal)){
    attr(res, 'entry') <- 'article'
  }else{
    attr(res, 'entry') <- 'misc'
  }
  
  return(res)
}


ProcessPDFSubject <- function(subj, check.doi = FALSE){
  res <- list()
 # browser()
  # first check for doi
  if (check.doi){
    pattern  <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
    m <- regexpr(pattern, temp, perl=TRUE)
    if (m != -1){
      res$doi <- regmatches(temp, m)
      subj <- regmatches(temp, m, invert = TRUE)
      subj <- gsub('doi:', '', subj)
    }
  }
  
  journ.ind <- regexec('^([[:upper:]][[:alpha:] ]+)[,\\.;]?[[:print:]]*(\\<(19|20)[0-9]{2}\\>)?', subj)
  temp <- regmatches(subj, journ.ind)
  if (length(temp)){
    temp <- regmatches(subj, journ.ind)
    res$journal <- temp[2]
    if (temp[3] != '')
      res$year <- temp[3]
  }
  
#   strs <- strsplit(subj, ',[[:space:]]?')[[1]]
#   # m <- regexpr('/^[^,]*/', subj, perl = TRUE)
#   # m <- regexpr('[[:print:^,]]+,', subj)
#   res$journal <- strs[1]
#   strs <- strs[-1]
  
  m <- regexpr('[Vv]ol(\\.|ume)?[[:space:]]?[0-9]{1,3}', subj)
  if (m != -1)
    res$volume <- regmatches(subj, m)
  
  m <- regexpr('^[Nn](o\\.|umber)[[:space:]]?[0-9]{1,3}', subj)
  if (m != -1)
    res$number <- regmatches(subj, m)
  
  if (is.null(res$volume) && is.null(res$number)){
    m <- regexec('\\(([0-9]{1,3})\\)[[:blank:]]?([0-9]{1,3})', subj)[[1]]
    if (m != -1){
      temp <- regmatches(subj, m)[[1]]
      res$volume <- temp[2]
      res$number <- temp[3]
    }
  }
  
#   m <- regexpr('^[[:digit:]]{4}$', strs)
#   ind <- which(m!=-1)
#   if (length(ind)){
#     res$year <- regmatches(strs, m)
#     strs <- strs[-ind]
#   }
  
  # be extra careful matching hypen. usuallly it's \u2013, an en dash
  m <- regexpr('[0-9]+[-\u2212\ufe58\ufe63\uff0d\u2012-\u2015][0-9]+', subj)
  if (m != -1){
    res$pages <- grep('[0-9]+[-\u2212\ufe58\ufe63\uff0d\u2012-\u2015][0-9]+', subj, value=TRUE)
  }
  
#   # if anything is left in the subject, add it to note field
#   if(length(strs))
#     res$note <- paste(strs, collapse = '')
  
  return(res)
}

AddListToList <- function(list1, list2){
  c1 <- is.na(list1) || length(list1)==0
  c2 <- is.na(list2) || length(list2)==0
  
  if (c1 && c2)
    return(NA)
  
  if (c1)
    return(list2)
  
  if (c2)
    return(list1)
  
  ind <- !names(list2) %in% names(list1)
  if(sum(ind))
    list1[names(list2)[ind]] <- list2[ind]
  return(list1)
}