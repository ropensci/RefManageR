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
  parms <- list(id = id, db = db, ...)
  #parms[[1]] <- NULL
  if (length(id > 1))
    parms$id <- paste0(id, collapse=',')
  
  parms$retmode <- 'xml'
  parms$rettype <- 'medline'

  if (is.null(id))
    stop('Must specify either id or query')
  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  temp <- getForm(base.url, .params = parms)
  tdoc <- xmlParse(temp)
      
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

ProcessPubMedResult <- function(article){
  tdoc <- xmlDoc(article)
  
  title <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/ArticleTitle',
                                 xmlValue))
  title <- gsub('\\.$', '', title)
  last.names <- unlist(xpathApply(tdoc, 
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/LastName', 
                                  xmlValue))
  first.names <- unlist(xpathApply(tdoc,  
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/ForeName',
                         xmlValue)) 
  author <- as.person(paste(first.names, last.names))
  
  year <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Year',
                                 xmlValue))
    
#   month <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Month',
#                                  xmlValue))
#   day <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Month',
#                                  xmlValue))  
#   browser()
#   date <- paste(year, month, day, sep='/')
  
  journal <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/Title',
                              xmlValue))
  
  volume <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Volume',
                              xmlValue))
  
  number <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Issue',
                              xmlValue))
  
  pages <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Pagination/MedlinePgn',
                            xmlValue)) 
  pmid <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/PMID',
                          xmlValue))
  doi <- unlist(xpathApply(tdoc, '//PubmedArticle/PubmedData/ArticleIdList/ArticleId',
                              xmlValue))
  doi <- grep('/', doi, value = TRUE)
  
  free(tdoc)
  res <- list(title = title, author = author, year = year, journal = journal, volume = volume, 
              number = number, pages = pages, eprint = pmid, doi = doi, eprinttype = 'pubmed')
  
  if (!is.null(res$journal)){
    attr(res, 'entry') <- 'article'
  }else{
    attr(res, 'entry') <- 'misc'
  }
  
  m <- regexpr('\\<([[:alpha:]]{4,})\\>', title)
  if(m != -1){
    key.title <- tolower(regmatches(title, m))
    attr(res, 'key') <- paste0(tolower(last.names[1]), year, key.title)  
  }else{
    attr(res, 'key') <- paste0(tolower(last.names[1]), year)
  }
    
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