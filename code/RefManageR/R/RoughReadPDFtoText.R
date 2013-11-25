N <- 30

not.done <- seq.int(N)
res <- lapply(seq.int(N)[not.done], function(i){x <- sample(2, 1);if(x==2){NA}else{1}} )
not.done <- not.done[is.na(res)]

res <- lapply(seq.int(N)[not.done], function(i){x <- sample(2, 1);if(x==2){NA}else{1}} )
not.done <- not.done[is.na(res)]

length(not.done)

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



system2('pdftotext', '-l 2 -layout Scott-VarParmNear0slow.pdf M:/test.txt')
system2('pdftotext', '-l 2 Ohara-BayesVarSelectionReview.pdf')
system2('pdftotext', '-l 2 -layout ProprietyOfPosteriorsForGLMMs.pdf')
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

setwd('~/BayesFGAM/papers')
system2('pdftotext', '-l 2 Marriott-DiagnosticsForVB(arXiv2013).pdf')
docarx <- readLines('Marriott-DiagnosticsForVB(arXiv2013).txt')
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

