require(scholar)
require(stringr)

# str_replace_all(temp, fixed('â€'), '-')

GSImport <- function(scholar.id = 'CJOHNoQAAAAJ'){
  uri <- 'http://scholar.google.com/citations'
  doc <- htmlParse(paste0('http://scholar.google.com/citations?user=', scholar.id, '&pagesize=100'))
  cites <- xpathApply(doc, "//tr[@class=\"cit-table item\"]")
  parse_cites <- function(l) {
    td <- l[[1]]
    title <- xmlValue(td[[1]])
    author <- xmlValue(td[[3]])
    cited_by <- as.numeric(xmlValue(l[[2]][[1]]))
    year <- as.numeric(xmlValue(l[[4]]))
    src <- xmlValue(td[[5]])
    first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", 
                                      src)) - 1
    ids <- which(first_digit < 0)
    first_digit <- replace(first_digit, ids, str_length(src)[ids])
    journals <- str_trim(str_sub(src, 1, first_digit))
    trailing_commas <- as.numeric(regexpr(",$", journals)) - 
      1
    ids <- which(trailing_commas < 0)
    trailing_commas <- replace(trailing_commas, ids, 
                               str_length(journals)[ids])
    journals <- str_sub(journals, 1, trailing_commas)
    numbers <- str_trim(str_sub(src, first_digit + 1, 
                                str_length(src)))
    return(data.frame(title = title, author = author, 
                      journal = journals, number = numbers, cites = cited_by, 
                      year = year))
  }
  tmp <- lapply(cites, parse_cites)
  browser()
}