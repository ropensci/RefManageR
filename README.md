RefManageR
========

R package RefManageR

Overview
=====================================
The purpose of this work is to develop an R package which will make it very easy for users to manipulate bibliographic information within R.  R already has a useful `bibentry` class and `person` class for managing `BibTeX` entries and author information.  There is also a very useful function in the package `bibtex` for reading a `.bib` file into and creating an object of class `bibentry` from the references in the file.  

We will add additional functionality to these existing tools in a number of ways: 1) By including many additional tools for importing bibliographic references, 2) By providing convenient tools for manipulating objects of class `bibentry` including searching and merging, 3) By providing tools for summarizing `bibentry` objects including plotting, printing, tabling, writing to files or to the web, etc., and 4) By providing support for `BibLaTeX` which is to this point not available in R, but offers much, much greater functionality for using `.bib` files in `LaTeX` than `BibTeX`.

Change log:
=============================
9. bug fix for `names<-.BibEntry`
12. fix for print function when entry has urldate but no url field
30. Correct some documentation typos
45. Fix pmidrelated field when `batch.mode = FALSE` in `GetPubMedRelated`

To Do:
==================================================================================================================
Important
-------------------
30. should bib[key1, key2, key3, ...] work?
9. pandoc style citations? e.g. [@key]
   * csl files
8. seealso in operators are doubled. e.g. see ?\`[.BibEntry\`
12. old build fails on getBibstyle. is it in Namespace file?
9. 76. In JSS paper need to document
  - toBibtex and toBiblatex examples
  - conversion with as.BibEntry, as.data.frame
  - ReadPubMed functions
  - check.entries options
  - return.ind option
  - expanded crossreferences
6. `[[<-` with length > 1 `BibEntry` objects
98. toBibtex - UTF-8 to latex option
87. merge should handle year and date specially
90. formatting of name fields with useregex TRUE
5. Write to Zotero
99. expand `@strings`

### Optional Additional Ideas
- implement a useful summary function
- BibStyle function so no conflicts with bibentry styles
- `check='warn'` or `check=convert.to.misc` option
- read from WorldCat (*investigated - worldcat sucks*)   
- Read bibentries from clipboard
- UpdateSingleAuthor function
* Make sure unicode handled properly? Unicode_alphabetic_tokenizer function in Unicode pkg  
Also see ?Encoding and ?iconv.  biblatex to bibtex function should use `iconv`
- plot function. sig.: plot(db, field, plottype, ...)  both 'author' and 'author-full' possible          
  * See igraph pkg or perhaps adapt http://bl.ocks.org/mbostock/3037015 (add counts on edges)
  * See the [wordcloud package](http://blog.fellstat.com/?cat=11)
- make table function.  table(db, field)  both 'author' and 'author-full'    
* http://opencitations.net/
* write "-.BibEntry" function
* see the `scholar` R package.  Imports bibtex data using scholar id's
* see R package CITAN for "scientometrics"
* see knitcitations: install_github("knitcitations", "cboettig")
* Support for additional features for BibEntry objects
  1. mkbibquote
  3. xref (*relavent when citing in LaTeX doc, not in R*)
  60. related, relatedtype, relatedstring, relatedoptions p. 26
  7. support for set entry type (for citing multiple references with one key)
  15. BibLaTeX manual p. 29 Section 2.3 Usage Notes.
* handle Zotero using techreport for arXiv files
* add merge function as wrapper for "+.BibEntry"
* add search function as wrapper for "[.BibEntry"
* is [`stringi`](http://docs.rexamine.com/R-man/stringi/stringi-encoding.html) worth using over `Unicode` package?
* Distinguish what can be imported and needs to be re-implemented from `bibtex` and `utils`
* efficient ways to update database with only recently added papers, mention in JSS manuscript
* read all of R Journal paper on person and bibentry classes
* OR in search function - split
* discuss methods(class='bibentry') in JSS manuscript
* add saved search parameter to ReadZotero function
* summary function
* add option for sorting results of subsetting based on diff fields and printing. See: utils:::sort.bibentry
* add ORCHID to person class
* Integrate with: http://ropensci.org/packages/
* add 'html' and 'html5' option to print function
  * Check out [R2HTML](http://cran.r-project.org/web/packages/R2HTML/R2HTML.pdf)
  * consider  option for html or html5 (if html5 use outline+summary to separate years)
  * option for numbering ( < ul > or < ol > )
  * option for download pdf or display url
  * option link.text='Download'
  * handle eprints

BUGS
=================================================================================
- (**FIXED**) merge doesn't work when `length(bib1) > length(bib2)` in `bib1+bib2`
- (**FIXED**) fix tryCatch in MakeBibEntry 
- (**FIXED**) `$` doesn't work for creating field that does not exist in any entries
  - happened in ReadPDFs when adding file info after calling MakeCitationList
  - possible problem when list elements sent to make citation list are already BibEntry class?
- if doi + url both available, `print` function formats and includes both as url
- ReadPDF  
  - (**FIXED**) key needs to be made after scanning text and meta data in ReadPDFs, not by both separately  
  - (**FIXED**) msgs for entries with no author+title should not occur in both reading of text and metadata in `ReadPDFs` 
  - handle ligatures in ReadPDFs: ff: "< U + F B 0 0 >" 
  - (**FIXED**) volume and number not working
  - (**FIXED**) files[[13]] - BoveHeld-BFapproximation(ArXiv2013).pdf should work for both authors - only gets one
  - year for WoodKohnShivelyJiang-BayesSSselection(JRSSB2002).pdf in GetJSTOR
  - (**FIXED**) handle results from CrossRef being list of BibEntry obj, instead of BibEntry obj. itself
  - add DOI's and file names at end
  - make sure all corner cases work: no crossref, no c.ref results, no metadata, no meta res, no JSTOR res,
    all jstor, all crossref+jstor
  - catch errors in MakeBibEntry
  - handle headers better when reading pdf, including getting year, etc. from 2nd line of doc
- (**FIXED**) `$<-`: `bib.entry.obj$field.name <- value.vec` adds vector of values to each bib. entry
- (**FIXED**) testbib <- ReadBib(system.file("REFERENCES.bib", package="bibtex")) fails, but read.bib works
- `bib[numeric_index, 'field_name']` or `bib['field_name', numeric_index]` not working
- warnings in ReadBib
- (**FIXED**) handling duplicates authors when printing in "authoryear" format (`extrafield` in BibLaTeX)
- Don't duplicate printing of Date if urldate or eventdate but no date in entry
- (**FIXED**) '\u00a7' encoded incorrectly for fmtPages, for some reason correct for fmtTotalPages
- authoryear bibstyle date, pages and title, totalpages
- (maybe) `bib[-seq_along(bib)]` returns nothing, probably should return NULL, empty list, or empty bib
- (**FIXED**) crossrefs in `toBibtex.BibEntry`
- (should be **FIXED**) capitalization when assigning bibtypes
- (**FIXED**) no.print.fields in print.BibEntry
- (**FIXED**) test.bib not printing: `Error in as.POSIXlt.numeric(x, tz = tz(x)) : 'origin' must be supplied` - is year(interval)
- (**FIXED**) BibEntry doesn't create `dateobj` if it is not supplied
- (**FIXED** Map vs. mapply -- simplify) error when extracting one entry with a crossref
- (**FIXED**) author (year)., pp. 12
- 'authoryear' style still screws up unicode when `bookpagination = true`
- (**FIXED**) 'authoryear' style should only display year for date
- print not working with `@strings`: pass strings to `expand_crossref` then 
`if (!is.null(attr(test2, 'strings'))){
 tmp <-  lapply(unclass(test2), function(x, strs){ 
    y <- mapply(function(string, name, bib){
    x <- gsub(string, name, bib)
    names(x) <- names(bib)
    x
  }, strs, names(strs), MoreArgs = list(bib = x), SIMPLIFY = FALSE)
    attributes(y) <- attributes(x)
    y
    }, strs = attr(test2,'strings'))
 tmp2 <- lapply(tmp, MakeBibEntry)
 attributes(tmp) <- attributes(test2)
}`
- (**FIXED**) indices wrong for search: e.g. `testb[location='berlin']`
- (**FIXED**) last year of dates being truncated: DateFormatter issue
- (**FIXED**) searching for multiple authors on same paper
- (**FIXED**) `.bibstyle = alphabetical` - label when et al. needed
- (**FIXED**) sorting with von, de la, etc.
- (**FIXED** same as useprefix=false in BibLaTeX) `.bibstyle = alphabetical` labels when von, de la, etc.
- (**FIXED**) ArrangeAuthors with UTF-8 input
- (**FIXED**) Herrmann entry in biblatexExamples.bib is not processed correctly by ArrangeAuthors
- `authoryear` still has some encoding issues - e.g. print(testb[location="München"], .bibstyle = 'authoryear')
- (**FIXED**) `sorting = "none"` an bibstyle needs labels
- (**FIXED**) CreateBibKey when special chars in author or title
- (**FIXED**) Printing year followed by editor for Article entries has extra period  `(2013).. Ed by M. McLean, pp. 60-70.`
- BibOptions are not reset on error - especially bad if return.ind is made TRUE inside function. Should be fixable with try-catch blocks in `print` and `[` and `search` and `sort` and `[<-`

DONE     
==================================================================================================================

* read from Zotero: http://www.zotero.org/support/dev/server_api/v2/read_requests
* get bib entries from Google Scholar by scholar ID
* Can get DOIs or BibTeX entries from CrossRef
* Read PDFs to get bib information
* `BibLaTeX` entry types and fields supported - still needing printing and conversion
* Unicode can be supported with little effort. encoding option in WriteBib; Unicode pkg; Encoding function
* use unclass(bibentryObj) to get entry type and key
* made date field date object
* Extract operator works with date
* created names function to return keys of multiple entries or fields of one entry
* Extract function to takes list argument
* Extract function to takes vector of keys
* Exract works with entry types
* `$<-` works to assign fields from _bibentry_ class
* levels.BibEntry to access fields
* added unlist and relist functions. relist use somewhat odd.
* let users set "[.BibEntry" and "+.BibEntry" defaults using getOptions() or BibEntryOptions() accessors and mutators
* add function for merging which handles duplicates (needs debugging)
* allow vector fields in search function `all(pmatch(searchterm, fields))` perhaps?
* `]]<-` for replacing single BibEntries
* `]<-` for for updating (multiple) fields of (multiple) entries
* search PubMed and import Bibtex entry, get Pubmed ID from PubMed
* open BibEntry using file, doi, url, or eprint
- @strings for shortcut for commonly used strings (*Implemented already for bibentry*)
- new open function; convert all other RefManager.R functions to work with bibentry class
- created multiple bibstyles for biblatex
- implemented all sorting options
- increased crossref support
- eprints supported
- support for pagination and bookpagination fields (predefined: page, column, line, verse, section, paragraph)
- Convert to and from data.frame for use with plyr
- xdata entry supported
- toBiblatex and toBibtex functions [S/O thread explaining how to do this](http://tex.stackexchange.com/questions/114787/converting-from-biblatex-to-bibtex-format-using-biber)  
- field = "!search.term" to negate match for search.term
=============================================================================================================

JSS Doc Outline
================================
1. **Intro**
2. **Importing References**
  1. ReadBib
  2. ReadText
  2. ReadZotero
  3. ReadGS
  4. ReadCrossRef
  5. ReadPDFMeta
  7. ReadPubMed
3. **Manipulating BibEntry Objects**
  1. Searching and Indexing
  2. Assignment
  2. as.BibEntry
    * unlist, relist.BibEntry
    * unclass
    * as.data.frame.BibEntry
  2. Merging
  3. Other Generics - sort, names, $, levels, 
4. **Printing/Viewing/Saving**
  1. Already available in bibentry
  3. toBiblatex - toBibtex
  98. Opening
  43. WriteBib? or just write.bib from `biblatex`
  23. WriteZotero
5. **Summarizing**
  1. Plot
    1. word diagrams
    2. d3.js directed graph with and without centre
  2. `table`
