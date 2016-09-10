#' Enhanced Bibliographic Entries
#'
#' Provides a new class \code{BibEntry} which builds on \code{\link{bibentry}} to provide enhanced
#'   functionalilty for representing, manipulating, importing, etc. bibliographic information in BibTeX or
#'   BibLaTeX style.
#' @param bibtype a character string with a BibTeX entry type. See Entry Types for details.
#' @param textVersion a character string with a text representation of the reference to optionally be employed for printing.
#' @param header a character string with optional header text.
#' @param footer a character string with optional footer text.
#' @param key	a character string giving the citation key for the entry.
#' @param ...	arguments of the form \code{tag = value} giving the fields of the entry, with \code{tag} and
#'   \code{value} the name and value of the field, respectively. Arguments with empty values are dropped. See
#'   Entry Fields for details.
#' @param other list; additional way to specify fields and their values
#' @param mheader string; optional \dQuote{outer} header text
#' @param mfooter string; optional \dQuote{outer} footer text
#' @return an object of class BibEntry
#' @export
#' @seealso \code{\link{bibentry}}
#' @keywords database
#' @import methods
#' @importFrom utils bibentry
#' @section Entry Types:
#'   bibentry creates "bibentry" objects, which are modeled after BibLaTeX and BibTeX entries. The entry should
#'   be a valid BibLaTeX or BibTeX entry type.  For a list of valid BibTeX entry types, see
#'   \code{\link{bibentry}}.  BibLaTeX supports all entry types from BibTeX for backwards compatibility.
#'   BibLaTeX defines following entry types
#'   '  \itemize{
#'    \item \emph{article} - An article in a journal, magazine, newspaper, or other periodical which forms a
#'   self-contained unit with its own title.  Required fields: author, title, journal/journaltitle, year/date.
#'    \item \emph{book} - A single-volume book with one or more authors where the authors share credit for
#'   the work as a whole.  Required fields: author, title, year/date. (Also covers BibTeX @@inbook).
#'    \item \emph{mvbook} - A multi-volume \emph{book}. For backwards compatibility, multi-volume books are also
#'   supported by the entry type @@book.  Required fields: author, title, year/date.
#'    \item \emph{inbook} - A part of a book which forms a self-contained unit with its own title. Note that the
#'   profile of this entry type is different from standard BibTeX.  Required fields: author, title,
#'   booktitle, year/date
#'    \item \emph{bookinbook} This type is similar to \emph{inbook} but intended for works originally published as a
#'   stand-alone book.
#'    \item \emph{suppbook} - Supplemental material in a \emph{book}. This type is closely related to the \emph{inbook} entry
#'   type.
#'    \item \emph{booklet} - A book-like work without a formal publisher or sponsoring institution.  Required fields:
#'    author/editor, title, year/date.
#'    \item \emph{collection} - A single-volume collection with multiple, self-contained contributions by distinct
#'    authors which have their own title.  Required fields: editor, title, year/date.
#'    \item \emph{mvcollection} - A multi-volume \emph{collection}.  Also supported by \emph{collection}.
#'    Required fields: editor, title, year/date.
#'    \item \emph{incollection} - A contribution to a collection which forms a self-contained unit with a distinct
#'    author and title.  Required fields: author, editor, title, booktitle, year/date.
#'    \item \emph{suppcollection} - Supplemental material in a \emph{collection}.
#'    \item \emph{manual} - Technical or other documentation, not necessarily in printed form. Required fields:
#'      author/editor, title, year/date
#'    \item \emph{misc} - A fallback type for entries which do not fit into any other category.  Required fields:
#'    author/editor, title, year/date
#'    \item \emph{online} - An online resource.  Required fields: author, title, number, year/date.
#'    \item \emph{patent} - A patent or patent request.  Required fields: author, title, number, year/date.
#'    \item \emph{periodical} - A complete issue of a periodical, such as a special issue of a journal.  Required
#'    fields: editor, title, year/date
#'    \item \emph{suppperiodical} - Supplemental material in a \emph{periodical}.
#'    \item \emph{proceedings} - A single-volume conference proceedings.  Required fields: editor, title, year/date.
#'    \item \emph{mvproceedings} - A multi-volume @@proceedings entry. Required fields: editor, title, year/date.
#'    \item \emph{inproceedings} - An article in a conference proceedings.  Required fields: author, editor, title,
#'    booktitle, year/date.
#'    \item \emph{reference} - A single-volume work of reference such as an encyclopedia or a dictionary.  Alias
#'          for \emph{collection} in standard styles.
#'    \item \emph{mvreference} - A multi-volume \emph{reference} entry.
#'    \item \emph{inreference} - An article in a work of reference.  Alias for \emph{incollection} in most styles.
#'    \item \emph{report} - A technical report, research report, or white paper published by a university or some
#'    other institution.  Required fields: author, title, type, institution, year/date.
#'    \item \emph{set} - An entry set. This entry type is special, see BibLaTeX manual.
#'    \item \emph{thesis} - A thesis written for an educational institution to satisfy the requirements for a degree.
#'    Use the type field to specify the type of thesis.  Required fields: author, title, type, institution,
#'    year/date.
#'    \item \emph{unpublished} A work with an author and a title which has not been formally published, such as a
#'    manuscript or the script of a talk.  Required fields: author, title, year/date.
#'    \item \emph{xdata} - This entry type is special. \emph{xdata} entries hold data which may be inherited by other
#'    entries. (Biber only.)
#'    \item \emph{custom[a-f]} Custom types (up to five) for special bibliography styles. Not used by the standard
#'     styles.
#'  }
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @note Date fields are parsed using the locale specified by `Sys.getlocale("LC_TIME")` (relevant
#' when specifying a character \sQuote{month} field, instead of the recommended integer format)
#'
#' Name list fields (author, editor, etc.) should be specified as they would be for
#' BibTeX/BibLaTeX; e.g. \code{author = "Doe, Jane and Smith, Bob A."}.
#' @references BibLaTeX manual \url{http://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/biblatex/doc/biblatex.pdf}
#' @details The BibEntry objects created by BibEntry can represent an arbitrary positive number of references,
#'   as with \code{bibentry}, but many additional methods are defined for building and manipulating a database
#'   of references.
#' @examples
#' BibEntry(bibtype = "Article", key = "mclean2014", title = "An Article Title",
#'   author = " McLean, Mathew W. and Wand, Matt P.", journaltitle = "The Journal Title",
#'   date = "2014-02-06", pubstate = "forthcoming")
#' bib <- BibEntry(bibtype = "XData", key = "arxiv_data", eprinttype = "arxiv",
#' eprintclass = "stat.ME", year = 2013, urldate = "2014-02-01", pubstate = "submitted")
#' bib <- c(bib, BibEntry(bibtype = "Misc", key = "mclean2014b",
#'   title = "Something On the {arXiv}", author = "Mathew W. McLean", eprint = "1312.9999",
#'   xdata = "arxiv_data", url = "http://arxiv.org/abs/1310.5811"))
#' bib
#' toBiblatex(bib)
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
          if (!inherits(rval[[i]], "person"))
              rval[[i]] <- ArrangeAuthors(rval[[i]])
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
