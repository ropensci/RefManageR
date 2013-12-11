# Mathew W. McLean
# December 1, 2013
# Interact with PubMed

# http://www.ncbi.nlm.nih.gov/books/NBK25500/
# http://www.bioinformatics.org/texmed/
# http://www.poirrier.be/~jean-etienne/software/pyp2b/
# http://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
# http://ropensci.github.io/rebi/

# http://www.ncbi.nlm.nih.gov/books/NBK3837/

ReadNCBI <- function(query, database = 'PubMed', ...){
  .params <- list(...)
  bad.ind <- which(!names(.params) %in% c('usehistory', 'WebEnv', 'query_key', 'retstart', 'retmax', 'field',
                                          'datetype', 'reldate', 'mindate', 'maxdate'))
  .parms <- .params
  if(length(bad.ind)){
    warning('Invalid .params specified and will be ignored')
    .parms <- .parms[-bad.ind]
  }
  base.url <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi'
  
  stopifnot(!missing(query))

  .parms$term <- query
  .parms$db <- database
  .parms$usehistory = 'y'
  results <- try(getForm(base.url, .params = .parms))
  if (inherits(results, 'try-error'))
    return(NA)
  
  tdoc <- xmlParse(results)
  #browser()
  webenv <- unlist(xpathApply(tdoc, '/eSearchResult/WebEnv', xmlValue))
  query.key <- unlist(xpathApply(tdoc, '/eSearchResult/QueryKey', xmlValue))
  ids <- unlist(xpathApply(tdoc, '/eSearchResult/IdList/Id', xmlValue)) 
  if (!length(ids)){
    message('No Results.')
    return()
  }
  
  res <- GetPubMedByID(id = ids, db = database, WebEnv = webenv, query_key = query.key)
  
  return(res)
}

GetPubMedByID <- function(id, db = 'pubmed', ...){
 # browser()
  parms <- list(db = db, ...)
  #parms[[1]] <- NULL
  if (!missing(id))
    parms$id <- paste0(id, collapse=',')
  
  parms$retmode <- 'xml'
  parms$rettype <- 'medline'

  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  temp <- getForm(base.url, .params = parms)
  tdoc <- xmlParse(temp)
  #    browser()
  # Note: directly using xpathApply on tdoc won't work if some results are missing certain fields    
  results <- getNodeSet(tdoc, '//PubmedArticleSet/PubmedArticle')      

  if(!length(results)){
    message('No results.')
    return()
  }
      
  results <- lapply(results, ProcessPubMedResult)
  
  res <- MakeCitationList(results)
  
  return(res)
  # first.inits <- gsub('(\\w)', '\\1\\. ', first.inits, perl=TRUE)
}

GetPubMedRelated <- function(id, database = 'pubmed', return.sim.scores = FALSE, max.results = 100){
#   .params <- list(...)
#   bad.ind <- which(!names(.params) %in% c('usehistory', 'WebEnv', 'query_key', 'retstart', 'retmax', 'field',
#                                           'datetype', 'reldate', 'mindate', 'maxdate'))
#   .parms <- list(cmd = 'neighbor_history', dbfrom = 'pubmed')
#   if(length(bad.ind)){
#     warning('Invalid .params specified and will be ignored')
#     .parms <- .parms[-bad.ind]
#   }

  stopifnot(!missing(id))
  
  if (inherits(id, 'BibEntry')){
    ind <- unlist(id$eprinttype)=='pubmed'
    id <- unlist(id$eprint[ind]) 
    if (is.null(id))
      stop('BibEntry object provided contains no PubMed Ids.')
  }
  parms <- list(cmd = 'neighbor_score', dbfrom = 'pubmed', db = database, id = id)
  if (length(id > 1))
    parms$id <- paste0(id, collapse=',')
  
  base.url <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi'

  results <- try(getForm(base.url, .params = parms))
  if (inherits(results, 'try-error'))
    return(NA)
  
  tdoc <- xmlParse(results)
 # browser()
  temp <- getNodeSet(tdoc, '/eLinkResult/LinkSet/LinkSetDb')[[1]]
  temp <- xmlDoc(temp)
  ids <- unlist(xpathApply(temp, '/LinkSetDb/Link/Id', xmlValue))
  if (is.null(id)){
    message('No results.')
    return()
  }
  
  ind <- seq_len(min(max.results, length(ids)))
  if (return.sim.scores)
    scores <- unlist(xpathApply(temp, '/LinkSetDb/Link/Score', xmlValue))[ind]

  #ids <- unlist(xpathApply(tdoc, '/eSearchResult/IdList/Id', xmlValue)) 
#   if (!length(ids)){
#     message('No Results.')
#     return()
#   }
  
  res <- GetPubMedByID(id = ids[ind], db = database)
  if (return.sim.scores)
    res[] <- lapply(scores, function(x) c(score = x))
  
  return(res)
}

ProcessPubMedResult <- function(article){
  tdoc <- xmlDoc(article)
  res <- list()
  
  title <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/ArticleTitle',
                                 xmlValue))
  res$title <- gsub('\\.$', '', title)
  last.names <- unlist(xpathApply(tdoc, 
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/LastName', 
                                  xmlValue))
  first.names <- unlist(xpathApply(tdoc,  
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/ForeName',
                         xmlValue)) 
  res$author <- as.person(paste(first.names, last.names))
  
  res$year <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Year',
                                 xmlValue))
    if (is.null(res$year))  # search extra hard for year
        res$year <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/ArticleDate/Year',
                                 xmlValue))
    if (is.null(res$year))
      res$year <- unlist(xpathApply(tdoc, '//PubmedArticle/PubmedData/History/PubMedPubDate/Year',
                                 xmlValue))[1]
#   month <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Month',
#                                  xmlValue))
#   day <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Month',
#                                  xmlValue))  
#   browser()
#   date <- paste(year, month, day, sep='/')
  
  res$journal <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/Title',
                              xmlValue))
  
  res$volume <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Volume',
                              xmlValue))
  
  res$number <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Issue',
                              xmlValue))
  
  res$pages <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Pagination/MedlinePgn',
                            xmlValue)) 
  res$pmid <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/PMID',
                          xmlValue))
  doi <- unlist(xpathApply(tdoc, '//PubmedArticle/PubmedData/ArticleIdList/ArticleId',
                              xmlValue))
  res$doi <- grep('/', doi, value = TRUE)
  
  free(tdoc)
  res$eprinttype <- 'pubmed'
  # res <- list(title = title, author = author, year = year, journal = journal, volume = volume, 
  #            number = number, pages = pages, eprint = pmid, doi = doi, eprinttype = 'pubmed')
  
  if (!is.null(res$journal)){
    attr(res, 'entry') <- 'article'
  }else{
    attr(res, 'entry') <- 'misc'
  }
  
  attr(res, 'key') <- CreateBibKey(res$title, res$author, res$year)
    
  return(MakeBibEntry(res, FALSE))
}

LookupPubMedID <- function(bib, index){
  stopifnot(inherits(bib, 'BibEntry'))
  if (missing(index))
    index <- seq_along(bib)
  
  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi?db=pubmed&retmode=xml&bdata="
  cit.strings <- sapply(bib[index], MakeCitationString)
  
  .url <- paste0(base.url,
                paste0(cit.strings, collapse = '%0D'))
  results <- getURL(.url)
  
  m <- gregexpr('KeY\\|(NOT_FOUND|[0-9]+)', results)
  res <- unlist(regmatches(results, m))
  res <- sub('KeY\\|', '', res)
  ind <- grep('[0-9]', res)
  if (length(ind)){
    message(paste0('Success for entries: ', paste0(ind, collapse=', ')))
    bib[ind] <- lapply(res[ind], function(x) c(eprinttype = 'pubmed', eprint = x))
  }else{
    message('No PubMed ID\'s found')
  }
  
  return(bib)
}

MakeCitationString <- function(bib.entry){
  first.page <- sub('-[0-9]+', '', bib.entry$pages)
  res <- paste(bib.entry$journal, bib.entry$year, bib.entry$volume, first.page, 
        paste0(as.character(bib.entry$author), collapse = ' '), 'KeY', sep = '|')  # unlist(bib.entry$author$family)
  res <- curlEscape(res)
  res <- gsub('%7C', '|', res)
  res <- gsub('%26', 'and', res)
  res <- paste0(res, '|')
  return(gsub('%20', '+', res))
}