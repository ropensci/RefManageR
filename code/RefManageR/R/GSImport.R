require(scholar)
require(stringr)

# str_replace_all(temp, fixed('â€'), '-')

GSImport <- function(scholar.id = 'CJOHNoQAAAAJ', start=0, num.res=100, encoding='UTF-8'){
  uri <- 'http://scholar.google.com/citations'
  doc <- getForm(uri, hl='en', user= scholar.id, oe=encoding, pagesize=min(num.res, 100),
                 view_op='list_works', cstart = start, .encoding = encoding)
  cites <- xpathApply(htmlParse(doc, encoding = encoding), "//tr[@class=\"cit-table item\"]")
  
  tmp <- lapply(cites2, ParseGSCites)
  browser()
}