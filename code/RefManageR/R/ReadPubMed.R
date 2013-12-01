# Mathew W. McLean
# December 1, 2013
# Interact with PubMed

# http://www.ncbi.nlm.nih.gov/books/NBK25500/
# http://www.bioinformatics.org/texmed/
# http://www.poirrier.be/~jean-etienne/software/pyp2b/
# http://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
# http://ropensci.github.io/rebi/

GetPubMed <- function(id, db = 'pubmed'){
  browser()

  if (is.null(id))
    stop('Must specify either id or query')
  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  temp <- getForm(base.url, db = db, id = id, retmode = 'xml', rettype = 'medline')
  tdoc <- xmlParse(temp)
  
  title <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/ArticleTitle',
                                 xmlValue))
  last.names <- unlist(xpathApply(tdoc, 
                '//PubmedArticleSet/PubmedArticle/MedlineCitation/Article/AuthorList/Author/LastName', 
                                  xmlValue))
  first.names <- unlist(xpathApply(tdoc,  
                '//PubmedArticleSet/PubmedArticle/MedlineCitation/Article/AuthorList/Author/ForeName',
                         xmlValue)) 
  author <- as.person(paste(first.names, last.names))
  
  year <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Year',
                                 xmlValue))
  
  journal <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Journal/Title',
                              xmlValue))
  
  volume <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Volume',
                              xmlValue))
  
  number <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Issue',
                              xmlValue))
  
  pages <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/Article/Pagination/MedlinePgn',
                            xmlValue)) 
  pmid <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/MedlineCitation/PMID',
                          xmlValue))
  doi <- unlist(xpathApply(tdoc, '/PubmedArticleSet/PubmedArticle/PubmedData/ArticleIdList/ArticleId',
                              xmlValue))
  doi <- grep('/', doi, value = TRUE)
  
  return(list(title = title, author = author, year = year, journal = journal, volume = volume, number = number,
              pages = pages, pmid = pmid, doi = doi))
  # first.inits <- gsub('(\\w)', '\\1\\. ', first.inits, perl=TRUE)
}