BibLaTeX_entry_field_db <- list()  # tools:::BibTeX_entry_field_db

BibLaTeX_entry_field_db$article <- c('author', 'title', 'journaltitle|journal', 'year|date')

BibLaTeX_entry_field_db$book <- c('author', 'title', 'year|date')

# multivolume book
BibLaTeX_entry_field_db$mvbook <- c('author', 'title', 'year|date')  

# different from BibTeX
BibLaTeX_entry_field_db$inbook <- c('author', 'title', 'booktitle|maintitle', 'year|date')  

# e.g. book reprint in selected works of author
BibLaTeX_entry_field_db$bookinbook <- c('author', 'title', 'booktitle|maintitle', 'year|date')  

# supplemental material for book
BibLaTeX_entry_field_db$suppbook <- c('author', 'title', 'booktitle|maintitle', 'year|date')  

# book without formal publisher
BibLaTeX_entry_field_db$booklet <- c('author|editor', 'title', 'year|date')  

# single-volume work, with lots of separate individual works
BibLaTeX_entry_field_db$collection <- c('editor', 'title', 'year|date') 

# multi-volume collection
BibLaTeX_entry_field_db$mvcollection <- c('editor', 'title', 'year|date') 

# contribution to a collection
BibLaTeX_entry_field_db$incollection <- c('author', 'editor', 'title', 'booktitle', 'year|date') 

# supplement to a collection
BibLaTeX_entry_field_db$suppcollection <- c('author', 'editor', 'title', 'booktitle', 'year|date') 

#manual
BibLaTeX_entry_field_db$manual <- c('author|editor', 'title', 'year|date') 

# misc
BibLaTeX_entry_field_db$misc <- character(0)

# online
BibLaTeX_entry_field_db$online <- c('author|editor', 'title', 'year|date', 'url|eprinttype') 

# patent
BibLaTeX_entry_field_db$patent <- c('author', 'title', 'number', 'year|date') 

# periodical
BibLaTeX_entry_field_db$periodical <- c('editor', 'title', 'year|date') 

# supplement to periodical; alias for article in most styles; e.g. regular columns, obituaries, letters to editor, etc.
BibLaTeX_entry_field_db$suppperiodical <- c('editor', 'title', 'year|date') 

# proceedings
BibLaTeX_entry_field_db$proceedings <- c('editor', 'title', 'year|date') 

# multi-volume proceedings
BibLaTeX_entry_field_db$mvproceedings <- c('editor', 'title', 'year|date') 

# inproceedings: an article in conference proceedings
BibLaTeX_entry_field_db$inproceedings <- BibLaTeX_entry_field_db$proceedings

# reference: alias for collection for most styles; specific type of collection e.g. encyclopedia or dictionary
BibLaTeX_entry_field_db$reference <- c('editor', 'title', 'year|date') 

# multi-volume reference: alias for mvcollection for most styles; 
BibLaTeX_entry_field_db$mvreference <- BibLaTeX_entry_field_db$reference

# inreference: alias for incollection for most styles; 
BibLaTeX_entry_field_db$inreference <- BibLaTeX_entry_field_db$reference

# report: need to specify type field. replaces techreport
BibLaTeX_entry_field_db$report <- c('author', 'title', 'type', 'institution', 'year|date') 

# set: special, see Section 3.11.5 of biblatex manual
BibLaTeX_entry_field_db$set <- c('entryset')

# thesis: replaces 'mastersthesis' and 'phdthesis' types by adding 'type' field
BibLaTeX_entry_field_db$thesis <- c('author', 'title', 'type', 'institution', 'year|date')  

# unpublished
BibLaTeX_entry_field_db$unpublished <- c('author', 'title', 'year|date') 

# xdata. special containers for data to be enherited by other entries which specify an 'xdata' field; see Section 3.11.6 of bibtex manual
BibLaTeX_entry_field_db$xdata <- character(0)

######################
# custom types # not supported by standard bib. styles, for which they will be treated as 'misc' type
BibLaTeX_entry_field_db$`custom[a]` <- character(0)
BibLaTeX_entry_field_db$`custom[b]` <- character(0)
BibLaTeX_entry_field_db$`custom[c]` <- character(0)
BibLaTeX_entry_field_db$`custom[d]` <- character(0)
BibLaTeX_entry_field_db$`custom[e]` <- character(0)
BibLaTeX_entry_field_db$`custom[f]` <- character(0)

#######################################
# aliases for compatibility with BibTeX
BibLaTeX_entry_field_db$conference <- BibLaTeX_entry_field_db$inproceedings
BibLaTeX_entry_field_db$electronic <- BibLaTeX_entry_field_db$online
BibLaTeX_entry_field_db$mastersthesis <- BibLaTeX_entry_field_db$thesis
BibLaTeX_entry_field_db$phdthesis <- BibLaTeX_entry_field_db$thesis
BibLaTeX_entry_field_db$techreport <- BibLaTeX_entry_field_db$report
BibLaTeX_entry_field_db$www <- BibLaTeX_entry_field_db$online

##########################################
# unsupported types: converted to 'misc' type by standard bib styles that don't support them
# no checking for required fields
BibLaTeX_entry_field_db$artwork <- character(0)
BibLaTeX_entry_field_db$audio <- character(0)
BibLaTeX_entry_field_db$bibnote <- character(0)
BibLaTeX_entry_field_db$commentary <- character(0)
BibLaTeX_entry_field_db$image <- character(0)
BibLaTeX_entry_field_db$jurisdiction <- character(0)
BibLaTeX_entry_field_db$legislation <- character(0)
BibLaTeX_entry_field_db$legal <- character(0)
BibLaTeX_entry_field_db$letter <- character(0)
BibLaTeX_entry_field_db$movie <- character(0)
BibLaTeX_entry_field_db$music <- character(0)
BibLaTeX_entry_field_db$performance <- character(0)
BibLaTeX_entry_field_db$review <- character(0)
BibLaTeX_entry_field_db$software <- character(0)
BibLaTeX_entry_field_db$standard <- character(0)
BibLaTeX_entry_field_db$video <- character(0)

