
setwd('~/BayesFactorsOld/papers/')
path <- 'Chinese-PostPropInSSModel(Chinese2013).pdf'
path <- 'RouderMorey-BFforANOVA.pdf'
path <- 'Ray-XMCK_IP_detection.pdf'
temp <- system(paste0('pdfinfo ', path), intern = TRUE)
system2('pdftotext', paste0('-f 1 -l 2 ', path))
temp <- readLines(gsub('.pdf', '.txt', path))

pattern = "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
# pattern <- "(10[.][0-9]{4,}[^\\s\"/<>]*/[^\\s\"<>]+)"
# pattern <- '\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?!["&\'<>])[[:graph:]])+)\b'
m <- regexpr(pattern, temp2, perl=TRUE)
regmatches(temp2, m)

m <- regexpr(pattern, temp, perl=TRUE)
doi <- regmatches(temp, m)
test2 <- ReadCrossRef(doi)
toBibtex(test2)



system2('pdftotext', '-l 2 Scott-VarParmNear0slow.pdf')
system2('pdftotext', '-l 2 Ohara-BayesVarSelectionReview.pdf')
system2('pdftotext', '-l 2 ProprietyOfPosteriorsForGLMMs.pdf')
system2('pdftotext', '-l 2 arxivtest.pdf')
system2('pdftotext', '-l 2 jasatest.pdf')
system2('pdftotext', '-l 2 mearxiv.pdf')
system2('pdftotext', '-l 2 pubmed1.pdf')
system2('pdftotext', '-l 2 pubmed2.pdf')
system2('pdftotext', '-l 2 Gelfand-ImproperPriorsForGLMs(JASA1999).pdf')
system2('pdftotext', '-l 2 jstorLT.pdf')
doc <- readLines('Scott-VarParmNear0slow.txt')
doc2 <- readLines('Ohara-BayesVarSelectionReview.txt')
doc3 <- readLines('ProprietyOfPosteriorsForGLMMs.txt')
doc4 <- readLines('arxivtest.txt')
jasa <- readLines('jasatest.txt')
docme <- readLines('mearxiv.txt', encoding = 'UTF-8')
docpm1 <- readLines('pubmed1.txt', encoding = 'UTF-8')
docpm2 <- readLines('pubmed2.txt', encoding = 'UTF-8')
jstordoc <- readLines('Gelfand-ImproperPriorsForGLMs(JASA1999).txt', encoding = 'UTF-8')
jstordoc2 <- readLines('jstorLT.txt', encoding = 'UTF-8')

# keywords
ind <- grep('K[Ee][Yy][[:space:]]?[Ww][Oo][Rr][Dd][Ss]:[[:space:]]*', doc)
m <- regexpr('Keywords:[[:space:]]*[A-Za-z]+\\s', doc, perl=TRUE)
regmatches(doc, m)
keywords <- sub('Keywords:[[:space:]]*', '', doc[ind]) 
keywords <- gsub(';', ',', keywords)

tnames <- c('Ana-Maria Staicu', 'A.-M. Staicu', 'R. de Vries', 'Oscar de la Hoya', 'Mathew McLean', 
            'Manuel Febrero-Bande', 'Shaq O\'Neal', 'T. T. T. Smith')
# author     first name                 opt. middle name          last name
#ind <- grep("[A-Z][a-z]*[\\.]?[[:space:]-][A-Z]?[a-z]*[\\.]?[[:space:]]?[[:upper:]][[:alpha:]'-]+", docme)

# match one author
ind <-  grep("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", docme)
m <- regexpr("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", doc2)
regmatches(doc2, m)
ind <- grep("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", tnames)
m <- regexpr("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", tnames)
regmatches(tnames, m)

# match multiple authors
mnames <- c('M. W. McLean, Ana-Maria Staicu, David Ruppert', 'Giles Hooker', 'Manny Pacquiao and T. L. Lewis')
ind <- grep("([A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+([[:print:]]*)?)+", mnames)
ind2 <- grep("([A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+[[:space:]]?(and)?[,;&]?[[:space:]])+", mnames)

m <- regexpr(paste0("([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
  "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
"[[:upper:]][[:alpha:]'-]+[[:space:]]?",  # last name potential extra char to 
"(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
                    mnames)
m <- regexpr("([A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+([[:print:]]*)?)+", mnames)
regmatches(mnames, m)

gsub("[^[:alpha:] '-]", '', tempnames)  # remove special characters end of names (e.g. McLean*, Carroll^#)

# e.g.: Ana-Maria Staicu, A.-M. Staicu, R. de Vries, Oscar de la Hoya, Mathew McLean, Manuel Febrero-Bande
# Shaq O'Neal, T. T. T. Smith
# ind <- regexpr('[A-Z]\\.|[a-z]+[.]?[[:space:]][A-Z]?[a-z]*[.]?[[:space:]]?[A-Z][a-z]+', docme)
# ind <- regexpr('[[:upper:]](\\.|[a-z]+)[.]?[[:space:]][A-Z]?[a-z]*[.]?[[:space:]]?[[:upper:]][[:alpha:]]+', docme)
# regmatches(docme, ind)

GetAuthorTitle <- function(doc){
  ind <- grep('^A[Bb][Ss][Tt][Rr][Aa][Cc][Tt][:.]?\\>', doc)[1]
  if (!is.na(ind) && ind > 2)  # assume title/author comes before Abstract. need 2nd cond. for ind==1
    doc <- doc[1:(ind-1)]
#  browser()
#   aut.ind <- regexpr(paste0("^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
#                       "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
#                       "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
#                       "(, )?(Jr)?(II)?(III)?(IV)?(, )?(MD)?(, )?(Ph|HD)?.?",  # optional qualifications      
#                       "(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
#                doc)
  
  aut.ind <- regexpr(paste0(# invalid words negate match
    "(?!Online|Supplement|Data|University|College|Institute|School)",
    "^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
    "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
    "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
    "(, )?(Jr)?(II)?(III)?(IV)?(, )?(MD)?(, )?(Ph|HD)?.?",  # optional qualifications      
    "(?<!Online|Supplement|Data|University|College|Institute|School)", 
    "(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
                     doc, perl=TRUE)
  aut.match <- regmatches(doc, aut.ind)
  aut.match <- gsub(",?( MD)?,?( P(H|h)D)?", '', aut.match)  # remove MD and PhD
  aut.match <- gsub("[^[:alpha:] ,'-](, MD)?(, P(H|h)D)?", '', aut.match)  # remove punct at end of last name
  return(list(ind=which(aut.ind != -1), match=aut.match, ab.ind=ind))
}


aut.ind <- regexpr(paste0(# invalid words negate match
  "(?!Online|Supplement|Data|University|College|Institute|School)",
  "^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
                          "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
                          "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
                          "(, )?(Jr)?(II)?(III)?(IV)?(, )?(MD)?(, )?(Ph|HD)?.?",  # optional qualifications      
  "(?<!Online|Supplement|Data|University|College|Institute|School)", 
                          "(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
                   doc, perl=TRUE)
regmatches(doc, aut.ind)

# volume
#volume <- grep('(?:Vol|Volume)[[:punct:]]?[[:space:]]?[0-9]+', doc2, value=TRUE)[1]
volume <- grep('(Vol|Volume)[[:punct:]]?[[:space:]]?[0-9]+', doc2, value=TRUE)[1]
if (!is.na(volume)){
  volume <- gsub('(Vol|Volume)[[:punct:]]?[[:space:]]?', '', volume)
}else{
  volume <- NULL
}

# number
number <- grep('(No|Number)[[:punct:]]?[[:space:]]?[0-9]+', doc2, value=TRUE)[1]
if (!is.na(number)){
  number <- gsub('(No|Number)[[:punct:]]?[[:space:]]?', '', number)
}else{
  number <- NULL
}

# year
# year <- grep('[(]?(19|20)[0-9]{2}[)]?', doc3, value=TRUE)[1]
m <- regexpr('\\<(19|20)[0-9]{2}\\>', doc)
if(any(m != -1))
  year <- regmatches(doc, m)[1]
# if(is.na(year)){
#   year <- NULL
# }else{
#   year <- 
#     sub('\\<(19|20)([0-9]{2})\\>', '', year)
#   grep('\\<(19|20)([0-9]{2})\\>', doc3, value=TRUE)[1]
# }

# URL

# title

# journal

# arXiv
ind <- grep('(arXiv)', doc)[1]
if(!is.na(ind)){
  arxinfo <- doc[ind]
  eprinttype <- 'arXiv'
  # need to check date since arXiv identifier format changed in Apr-07
  m <- regexpr('[0-9]{1,2}[[:space:]][A-Z][a-z]{2}[[:space:]][0-9]{4}', arxinfo)
  adate <- strptime(regmatches(arxinfo, m), format='%d %b %Y')
  if (adate >= strptime('01 Apr 2007', format='%d %b %Y')){
    m <- regexec('arXiv:([0-9]{4}[\\.][0-9]{4}v[0-9])[[:space:]]\\[([[:graph:]]+)\\]', arxinfo)
    regm <- regmatches(arxinfo, m)
    eprintclass <- regm[[1]][3]
    eprint <- regm[[1]][2]
  }else{
    m <- regexec('arXiv:([[:graph:]]+)\\s', arxinfo)
    regm <- regmatches(arxinfo, m)
    eprint <- regm[[1]][2]
  }
  url <- paste0('http://arxiv.org/abs/', eprint)
}

# JSTOR
ind <- grep('http://www\\.jstor\\.org/stable/([0-9]+)', jstordoc)[1]
if (!is.na(ind)){
  m <- ('http://www\\.jstor\\.org/stable/([0-9]+)', jstordoc[ind])
}


year <- gsub()
year <- grep('')
