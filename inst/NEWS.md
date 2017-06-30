Changes in Version 0.14.12 (2017-06-30)
--------------------------------------------------------
NEW FEATURES

* Package now uses httr, xml2, jsonlite packages instead of RCurl, XML, RJSONIO
for scaffolding
* No more R CMD check NOTE regarding foreign function call to bibtex (h/t Romain
Francois)

BUG FIXES

* fix printing when `BibOptions(style = 'yaml)`
* remove invalid character in inst/Bib/RJC.bib
* correct parsing of interval dates when creating unique labels for authoryear style
citations
* `c.BibEntry` throws an error if not all objects are `bibentry` objects
* fix typos in documentation
* Literal ampersands are now printed correctly (not as '\&') (h/t Yue Hu)
* Ensure BibTeX month macros are processed properly by lubridate
in non-English locales (h/t Sergio Oller)

Changes in Version 0.13.4 (2017-04-25)
--------------------------------------------------------

BUG FIXES

* Unescape special characters in URL fields returned by CrossRef (h/t Michael Schubert) 
* Remove square brackets from custom entry type names (h/t Hugh Parsonage)

Changes in Version 0.13.1 (2016-11-14)
--------------------------------------------------------

BUG FIXES

* Feature involving `LaTeX` macros added in package version 0.12.0 can only be used
for R 3.3.z and higher; this corrects cause of failed checks on R 3.2.z


Changes in Version 0.13.0 (2016-11-09)
--------------------------------------------------------

BUG FIXES

* Updated calls to NCBI Entrez for functions `ReadPubMed`, `GetPubMedByID`, etc.
to use https as now required by NCBI (h/t Dale Steele and Anthony Crane)
* Change reference to www.omegahat.org to www.omegahat.net (h/t Kurt Hornik)
* Documentation for `ReadPubMed` is updated to reflect that the default number of
entries returned (controlled by the argument `retmax`)  is 20 (h/t Dale Steele)

Changes in Version 0.12.0 (2016-09-30)
--------------------------------------------------------

NEW FEATURES

* Some `LaTeX` macros unknown to R are now defined as macros in the package, and will
be parsed using `macros` arg in `tools::parse_Rd` (assuming `getRversion() >= "3.2.0"`
Note: corrected in 0.13.1 to be `getRversion() >= "3.3.0"`)

BUG FIXES

* Parse `LaTeX` macro `\textquotesingle` in author names (h/t Bill Denney)
* Avoid "Request-URI too large" errors in GetPubMedByID if requesting a large number
of IDs (h/t Maurits Evers)

Changes in Version 0.11.0 (2016-09-10)
--------------------------------------------------------

NEW FEATURES

* `ReadCrossRef` now uses the [newer CrossRef API](https://github.com/CrossRef/rest-api-doc/blob/master/rest_api.md) and gains arguments
`filter` and `offset` to use with the new API; an
additional argument `use.old.api` is added if the user wishes to use the old API
(h/t Carl Boettiger)
* `ReadCrossRef` now parses the results returned by CrossRef to
create the `BibEntry` object when using the new API; for the old API (and hence, older
versions of the package) the query only returns DOIs and `ReadCrossRef` would then
use the DOIs to request the corresponding BibTeX entries from CrossRef (i.e. less
HTTP requests when using the new API)

BUG FIXES

* Fix generation of entry keys when the word used from the title for key
generation contains a non-ascii character (h/t Mark Johnson)
* RefManageR will no longer hang due to a bug in `tools::latexToUtf8`
([PR\#17138](https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17138)) that
is occasionally encountered when that function processes an unknown
macro (h/t Eric Bryant)
* Entries with no title field can now be printed without error when
`BibOptions()$check.entries` is *not* set to "error" (default is "error")

Changes in Version 0.10.15 (2016-06-06)
--------------------------------------------------------

BUG FIXES

* Removed unnecessary use of local/parent.frame; fixes execution with
bytecode compiler (h/t Tomas Kalibera)

Changes in Version 0.10.12 (2016-03-25)
--------------------------------------------------------

BUG FIXES

* Fixed broken test involving `ReadPDFs` due to changed URL (h/t Kurt Hornik)
* `as.data.frame.BibEntry` works for length one BibEntry with multiple
authors  (h/t Dale Steele)
* Use `httr::GET` to fix `ReadGS`
* Fixed broken tests in `test-authors.R` owing to changes to `person` class
* Fixed `ReadCrossRef` tests and error message
* Fixed printing for authoryear style (h/t Joseph Casillas)
* Name list fields (author, editor, etc.) provided to the function `BibEntry` are
now properly parsed when specified as they would be in BibTeX/BibLaTeX;
e.g. `author = "Smith, Bob A. and Doe, Jane"`. 

Changes in Version 0.10.5 (2016-01-03)
--------------------------------------------------------

* The 'key' field in `BibEntry` objects is now always enforced to be unique
* `as.data.frame.BibEntry` is faster and now works if duplicate keys are
present; keys in  (h/t Dale Steele)
* Fix for `ReadCrossRef` if downloaded BibTeX had leading whitespace (h/t Carl
Boettiger)
* `useBytes = TRUE` used for all calls to `grep`, `sub`, etc. (h/t HI&RH Lord Ripley of England)
* remove use of deprecated function `lubridate::new_interval`
* updated URL for the BibLaTeX manual
* Fix test in test-search.R that broke because of new year (h/t HI&RH Lord Ripley of England)
* add additional functions from utils and stats to NAMESPACE

Changes in Version 0.9.0 (2015-06-10)
--------------------------------------------------------

* Use `bibtex >= 0.4.0.9000` function `do_read_bib` to avoid `.External` call and
`R check` note (request of HI&RH Lord Ripley of England)

Changes in Version 0.8.63 (2015-06-08)
--------------------------------------------------------

NEW FEATURES

* Improve parsing of dates in `ReadPDFs`
* Citations using `Cite` family of functions can now be `pandoc` style, e.g. `[@abibkey]`
by setting `BibOptions(cite.style = "pandoc")` (h/t Dale Steele)
* Added note about locales when parsing string 'month' fields to `ReadBib` help
page (h/t Dieter Menne)

BUG FIXES

* Fixed merging `BibEntry` objects by multiple fields when no duplicates
* `open.BibEntry` fixed to not use partial matching of field names; e.g. an error would occur if the
specified entry had a 'urldate' field, but no 'url' field
* `open.BibEntry` will `message` and not throw error if entry cannot be opened
* Fixes for `ReadPDFs` when argument `use.metadata` is `FALSE`
* Fix for `ReadPDFs` when when reading *one* file which is a JSTOR pdf
* Fix for sorting by volume (`BibOptions(sorting = "anyvt")` and `BibOptions(sorting = "nyvt")`
* Fix for sorting by label (`BibOptions()sorting` equal to  "anyvt" or "anyt")
* `GetBibEntryWithDOI.R` will not `stop` if an error occurs downloading any of the DOIs (e.g.,
if one entry in the `doi` vector has a typo and the rest are valid)

Changes in Version 0.8.52 (2015-01-26)
--------------------------------------------------------

NEW FEATURES

* `GetPubMedByID`: Now returns some additional fields including 'month' and 'issn' for
articles; will print a warning if PubMed does not return the complete list
of authors; will use the name of a collective if one is available and the individual
authors are missing (h/t Dale Steele)

BUG FIXES

* `ReadBib`: If a name list field in an entry cannot be parsed in the bib file, the
entry will be ignored, but the rest of the file will still be processed and
returned. In the past, this caused an error and no output would be returned.
* 'Book' entries will now be parsed correctly by `GetPubMedByID` (h/t Dale Steele)
* Fix error/warning messages when entry is missing required fields (bug introduced in
Version 0.8.45)
* Name lists containing a comma in braces will now be parsed correctly,
e.g. "Buchalter, Louis and {Murder, Inc.} and Anastasia, Albert"

Changes in Version 0.8.45 (2014-12-29)
--------------------------------------------------------

BUG FIXES

* `ReadCrossRef` now correctly handles the small number of cases where BibTeX information
cannot be obtained for a particular DOI, which resulted in 'stack imbalance' warnings
and no results being returned (h/t Norman L Guinasso Jr).
* `ReadGS` fixed to account for changes to Google "API" (h/t Norman L Guinasso Jr).
* Improved parsing for BibTeX format names ending with a '}' (h/t Henrik Bengtsson).
* Printing references with `style = "html"` would not always add an opening <cite> tag
when `bib.style = "numeric"` or `bib.style = "alphabetic"` (h/t Henrik Bengtsson).
* `format.BibEntry` would ignore the `.style` argument if called directly by the user.
Note, this function should normally not need to be called directly. (h/t Henrik Bengtsson)

Changes in Version 0.8.40 (2014-10-28)
--------------------------------------------------------

NEW FEATURES

* Improved formatting of citation given to CrossRef for increased chances of finding matches with `GetDOIs`
function (h/t Erich Studerus)
* Additional parsing of 'month' field to accomodate days and ranges of days and months.  Example bib entries
that will be parsed correctly include `month = jun # "/" # jul`, `month = "20~" # jan`,
`month = "20--25~" # dec`, `month =  "10~" # jan # "/" # feb` (request of Stephen Eglen)
* Added argument 'group' to `ReadZotero` for specifying a groupID to query a group library instead of a
user library (h/t Greg Blomquist).

BUG FIXES

* DOI's hyperlinks in Markdown format are now correct (h/t Stephen Eglen)
* `print.BibEntry` with `BibOptions(style = "Biblatex")` fixed (h/t Artem Klevtsov)
* `unlist.BibEntry` and `RelistBibEntry` now retains `@strings` and `mheader` and
`mfooter` attributes (see ?BibEntry) if they are present

Changes in Version 0.8.34 (2014-08-18)
--------------------------------------------------------

NEW FEATURES

* Added function `GetDOIs` which searches CrossRef for DOIs for the citations stored in
a `BibEntry` object

BUG FIXES

* `ReadCrossRef` fixed to account for change to CrossRef API endpoint. (h/t Carl Boettiger)
* Abstracts returned by NCBI Entrez can be multiple parts.  This is now handled correctly and the complete
abstract will be returned in the 'abstract' field. (h/t Erich Studerus)
* DOI's were too naively extracted from NCBI Entrez results, resulting in some entries having 'doi' fields with
length greater than one.  Now fixed. (h/t Erich Studerus)

Changes in Version 0.8.32 (2014-08-14)
--------------------------------------------------------

NEW FEATURES

* Functions for interacting with NCBI Entrez return abstract of each article (request of Erich Studerus)

BUG FIXES

* `print.BibEntry` with `BibOptions(style = "citation")` now works properly
* Examples calling web resources should no longer upset the check farm (h/t HI&RH Lord Ripley of England)


Changes in Version 0.8.3 (2013-07-30)
---------------------------------------------------------

NEW FEATURES

* `as.BibEntry` will create entry key if given a `bibentry` object with no key.  Useful when citing
packages with `citation`.
* PrintBibliography and Cite functions (Cite, Citet, etc.) accept `bibentry` objects in addition to
`BibEntry` objects.
* `$<-.BibEntry` will now accept a single person object, so that a single author in a multi-author entry
may be updated.  An example may be found at `help("$<-.BibEntry")`.  (h/t Carl Boettiger)

BUG FIXES

* validated html
* changed example for `WriteBib` that occasionally failed check

Changes in Version 0.8.2 (2013-06-01)
---------------------------------------------------------

BUG FIXES

* Cite functions work if a specified entry has no key.  Note that keys should always be provided for all entries as they  are required for all entries in a BibLaTeX bib file (h/t Carl Boettiger)
* Entries returned by Crossref that have entry type 'Data' which is not supported by default in BibLaTeX are converted to type 'Online' (h/t Carl Boettiger)
* Fix for `ReadGS` when argument `check.entries` is FALSE or "warn" (h/t Francisco Rodriguez Sanchez)
* Family names from Scholar in all caps are handled correctly in ReadGS

NEW FEATURES

* Functions for interacting with PubMed return language of each article (h/t Dale Steele)
* Added CITATION file
* Updated License to explicitly include GPL-2 and GPL-3

Changes in Version 0.8.1 (2013-03-09)
---------------------------------------------------------

BUG FIXES

* Fix for `names<-.BibEntry`
* Fix for `print.BibEntry` when entry has urldate field but no url field
* Corrections for some documentation typos
* Fix pmidrelated field when `batch.mode = FALSE` in `GetPubMedRelated`
* Fix for `LookupPubMedID` when `index` argument specified
* `open.BibEntry` now works properly
* Fix for converting thesis entries in `toBibtex.BibEntry`
* Fix for `WriteBib` with `biblatex` argument

NEW FEATURES

* Added Vignettes including user manual and Rmd citation examples
* Added NEWS
* Added HTML output of Rmd and RHTML citation examples to doc/
