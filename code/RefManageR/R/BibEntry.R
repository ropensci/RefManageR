#' Enhanced Bibliographic Entries
#' 
#' Provides a new class \code{BibEntry} which builds on \code{\link{bibentry}} to provide enhanced 
#'   functionalilty for representing, manipulating, importing, etc. bibliographic information in BibTeX or
#'   BibLaTeX style.
#' @param bibtype a character string with a BibTeX entry type. See Entry Types for details.
#' textVersion  a character string with a text representation of the reference to optionally be employed for printing.
#' @param header a character string with optional header text.
#' @param footer a character string with optional footer text.
#' @param key	a character string giving the citation key for the entry.
#' @param ...	arguments of the form \code{tag = value} giving the fields of the entry, with \code{tag} and 
#'   \code{value} the name and value of the field, respectively. Arguments with empty values are dropped. See 
#'   Entry Fields for details.
#' @return an object of class BibEntry
#' @seealso \code{\link{bibentry}}
#' @export
#' @section Entry Types: 
#'   bibentry creates "bibentry" objects, which are modeled after BibLaTeX and BibTeX entries. The entry should 
#'   be a valid BibLaTeX or BibTeX entry type.  For a list of valid BibTeX entry types, see 
#'   \code{\link{bibentry}}.  BibLaTeX supports all entry types from BibTeX for backwards compatibility.  
#'   BibLaTeX defines following entry types
#'   '  \itemize{
#'    \item article - An article in a journal, magazine, newspaper, or other periodical which forms a
#'   self-contained unit with its own title.  Required fields: author, title, journal/journaltitle, year/date.
#'    \item book - A single-volume book with one or more authors where the authors share credit for
#'   the work as a whole.  Required fields: author, title, year/date. (Also covers BibTeX @@inbook).
#'    \item mvbook - A multi-volume @@book. For backwards compatibility, multi-volume books are also
#'   supported by the entry type @@book.  Required fields: author, title, year/date.
#'    \item inbook - A part of a book which forms a self-contained unit with its own title. Note that the
#'   profile of this entry type is different from standard BibTeX.  Required fields: author, title, 
#'   booktitle, year/date
#'    \item bookinbook This type is similar to @@inbook but intended for works originally published as a
#'   stand-alone book.
#'    \item suppbook - Supplemental material in a @@book. This type is closely related to the @@inbook entry
#'   type.
#'    \item booklet - A book-like work without a formal publisher or sponsoring institution.  Required fields: 
#'    author/editor, title, year/date.
#'    \item collection - A single-volume collection with multiple, self-contained contributions by distinct
#'    authors which have their own title.  Required fields: editor, title, year/date.
#'    \item mvcollection - A multi-volume @@collection.  Also supported by @@collection.  Required fields: editor, 
#'    title, year/date.
#'    \item incollection - A contribution to a collection which forms a self-contained unit with a distinct 
#'    author and title.  Required fields: author, editor, title, booktitle, year/date.
#'    \item suppcollection - Supplemental material in a @@collection.
#'    \item manual - Technical or other documentation, not necessarily in printed form. Required fields: 
#'      author/editor, title, year/date
#'    \item misc - A fallback type for entries which do not fit into any other category.  Required fields: 
#'    author/editor, title, year/date
#'    \item online - An online resource.  Required fields: author, title, number, year/date.
#'    \item patent - A patent or patent request.  Required fields: author, title, number, year/date.
#'    \item periodical - A complete issue of a periodical, such as a special issue of a journal.  Required 
#'    fields: editor, title, year/date
#'    \item suppperiodical - Supplemental material in a @@periodical.
#'    \item proceedings - A single-volume conference proceedings.  Required fields: editor, title, year/date.
#'    \item mvproceedings - A multi-volume @@proceedings entry. Required fields: editor, title, year/date.
#'    \item inproceedings - An article in a conference proceedings.  Required fields: author, editor, title, 
#'    booktitle, year/date.
#'    \item reference - A single-volume work of reference such as an encyclopedia or a dictionary.  Alias 
#'          for @@collection in standard styles.
#'    \item mvreference - A multi-volume @@reference entry.
#'    \item inreference - An article in a work of reference.  Alias for @@incollection in most styles.
#'    \item report - A technical report, research report, or white paper published by a university or some
#'    other institution.  Required fields: author, title, type, institution, year/date.
#'    \item set - An entry set. This entry type is special, see BibLaTeX manual.
#'    \item thesis - A thesis written for an educational institution to satisfy the requirements for a degree.
#'    Use the type field to specify the type of thesis.  Required fields: author, title, type, institution, 
#'    year/date.
#'    \item unpublished A work with an author and a title which has not been formally published, such as a
#'    manuscript or the script of a talk.  Required fields: author, title, year/date.
#'    \item xdata - This entry type is special. @@xdata entries hold data which may be inherited by other 
#'    entries. (Biber only.)
#'    \item custom[a-f] Custom types (up to five) for special bibliography styles. Not used by the standard
#'     styles.
#'  }
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @references BibLaTeX manual \url{http://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/biblatex/doc/biblatex.pdf}
#' @details The BibEntry objects created by BibEntry can represent an arbitrary positive number of references,
#'   as with \code{bibentry}, but many addition methods are defined for building and manipulating a database
#'   of references.
BibEntry <- function (bibtype, textVersion = NULL, header = NULL, footer = NULL, 
                      key = NULL, ..., other = list(), mheader = NULL, mfooter = NULL){
  BibTeX_names <- names(BibLaTeX_entry_field_db)
  args <- c(list(...), other)
  if (!length(args)) 
    return(structure(list(), class = "bibentry"))
  if (any(sapply(names(args), .is_not_nonempty_text))) 
    stop("all fields have to be named")
  args <- c(list(bibtype = bibtype, textVersion = textVersion, 
                 header = header, footer = footer, key = key), list(...))
  args <- lapply(args, .listify)
  other <- lapply(other, .listify)
  max_length <- max(sapply(c(args, other), length))
  args_length <- sapply(args, length)
  if (!all(args_length_ok <- args_length %in% c(1L, max_length))) 
    warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s", 
                     paste(names(args)[!args_length_ok], collapse = ", ")), 
            domain = NA)
  args <- lapply(args, function(x) rep(x, length.out = max_length))
  other_length <- sapply(other, length)
  if (!all(other_length_ok <- other_length %in% c(1L, max_length))) 
    warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s", 
                     paste(names(other)[!other_length_ok], collapse = ", ")), 
            domain = NA)
  other <- lapply(other, function(x) rep(x, length.out = max_length))
  bibentry1 <- function(bibtype, textVersion, header = NULL, 
                        footer = NULL, key = NULL, ..., other = list()) {
    bibtype <- as.character(bibtype)
    stopifnot(length(bibtype) == 1L)
    pos <- match(tolower(bibtype), tolower(BibTeX_names))
    if (is.na(pos)) 
      stop(gettextf("%s has to be one of %s", sQuote("bibtype"), 
                    paste(BibTeX_names, collapse = ", ")), domain = NA)
    bibtype <- BibTeX_names[pos]
    rval <- c(list(...), other)
    rval <- rval[!sapply(rval, .is_not_nonempty_text)]
    fields <- tolower(names(rval))
    names(rval) <- fields
    attr(rval, "bibtype") <- bibtype
    .BibEntryCheckBibEntry1(rval)
    pos <- fields %in% .BibEntryNameList
    if (any(pos)) {
      for (i in which(pos)) 
        rval[[i]] <- as.person(rval[[i]])
    }
    pos <- fields %in% c("dateobj") | pos
    if (any(!pos)) {
      for (i in which(!pos)) 
        rval[[i]] <- as.character(rval[[i]])
    }
    attr(rval, "key") <- if (is.null(key)) 
      NULL
    else as.character(key)
    if (is.null(rval[['dateobj']])){
      tdate <- try(ProcessDates(rval), TRUE)
      if (!inherits(tdate, "try-error"))
        attr(rval, 'dateobj') <- tdate
    }else{
      attr(rval, 'dateobj') <- rval[['dateobj']]
      rval[['dateobj']] <- NULL  
    }
    
    if (!is.null(textVersion)) 
      attr(rval, "textVersion") <- as.character(textVersion)
    if (!.is_not_nonempty_text(header)) 
      attr(rval, "header") <- paste(header, collapse = "\n")
    if (!.is_not_nonempty_text(footer)) 
      attr(rval, "footer") <- paste(footer, collapse = "\n")
    return(rval)
  }
  
  rval <- lapply(seq_along(args$bibtype), function(i) do.call("bibentry1", 
                                                              c(lapply(args, "[[", i), 
                                                                list(other = lapply(other, "[[", i)))))
  
  if (!.is_not_nonempty_text(mheader)) 
    attr(rval, "mheader") <- paste(mheader, collapse = "\n")
  if (!.is_not_nonempty_text(mfooter)) 
    attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")
  class(rval) <- c("BibEntry", "bibentry")
  rval
}