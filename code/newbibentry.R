# Mathew McLean
# October 25, 2013
# Make bibentry class compatible with BibLaTeX

source('~/biblatex/biblatex/code/BibLaTeX_entry_field_db.R')
#oldbibentry <- bibentry
# old.bibentry_Check_bibentry1 <- utils:::.bibentry_check_bibentry1

test <- ReadBib('~/biblatex/biblatex/code/biblatexTestBib.bib')
# test <- ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))

BibEntry <- function (bibtype, textVersion = NULL, header = NULL, footer = NULL, 
                      key = NULL, ..., other = list(), mheader = NULL, mfooter = NULL) 
{
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
    .BibentryCheckBibEntry1(rval)
    pos <- fields %in% c("author", "editor")
    if (any(pos)) {
      for (i in which(pos)) rval[[i]] <- as.person(rval[[i]])
    }
    if (any(!pos)) {
      for (i in which(!pos)) rval[[i]] <- as.character(rval[[i]])
    }
    attr(rval, "key") <- if (is.null(key)) 
      NULL
    else as.character(key)
    if (!is.null(textVersion)) 
      attr(rval, "textVersion") <- as.character(textVersion)
    if (!.is_not_nonempty_text(header)) 
      attr(rval, "header") <- paste(header, collapse = "\n")
    if (!.is_not_nonempty_text(footer)) 
      attr(rval, "footer") <- paste(footer, collapse = "\n")
    return(rval)
  }
  rval <- lapply(seq_along(args$bibtype), function(i) do.call("bibentry1", 
                                                              c(lapply(args, "[[", i), list(other = lapply(other, "[[", 
                                                                                                           i)))))
  if (!.is_not_nonempty_text(mheader)) 
    attr(rval, "mheader") <- paste(mheader, collapse = "\n")
  if (!.is_not_nonempty_text(mfooter)) 
    attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")
  class(rval) <- c("BibEntry", "bibentry")
  rval
}

.BibentryCheckBibEntry1 <- function (x, force = FALSE) {
  fields <- names(x)
  if (!force && !.is_not_nonempty_text(x$crossref)) 
    return(NULL)
  bibtype <- attr(x, "bibtype")
  rfields <- strsplit(BibLaTeX_entry_field_db[[bibtype]], 
                      "|", fixed = TRUE)
  if (length(rfields) > 0L) {
    ok <- sapply(rfields, function(f) any(f %in% fields))
    if (any(!ok)) 
      stop(sprintf(ngettext(sum(!ok), "A bibentry of bibtype %s has to specify the field: %s", 
                            "A bibentry of bibtype %s has to specify the fields: %s"), 
                   sQuote(bibtype), paste(rfields[!ok], collapse = ", ")), 
           domain = NA)
  }
}

.Bibentry_expand_crossrefs <- function (x, more = list()) {
  y <- if (length(more)) 
    do.call(c, c(list(x), more))
  else x
  x <- unclass(x)
  y <- unclass(y)
  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .bibentry_get_key(y))
    ok <- !is.na(pk)
    x[pc[ok]] <- Map(function(u, v) {
      add <- setdiff(names(v), names(u))
      u[add] <- v[add]
      if (!is.na(match(tolower(attr(u, "bibtype")), c("incollection", 
                                                      "inproceedings"))) && is.null(u$booktitle)) 
        u$booktitle <- v$title
      u
    }, x[pc[ok]], y[pk[ok]])
    status <- lapply(x[pc], function(e) tryCatch(.BibentryCheckBibEntry1(e, 
                                                                           TRUE), error = identity))
    bad <- which(sapply(status, inherits, "error"))
    if (length(bad)) {
      for (b in bad) {
        warning(gettextf("Dropping invalid entry %d:\n%s", 
                         pc[b], conditionMessage(status[[b]])))
      }
      x[pc[bad]] <- NULL
    }
  }
  class(x) <- C("BibEntry", "bibentry")
  x
}

WriteBib <- function (entry, file = "Rpackages.bib", append = FALSE, verbose = TRUE) {
  bibs <- if (inherits(entry, "bibentry")) 
    entry
  else if (is.character(entry)) {
    if (length(entry) == 0) {
      if (verbose) 
        message("Empty package list: nothing to be done.")
      return(invisible())
    }
    pkgs <- entry
    if (is.null(pkgs)) 
      pkgs <- unique(installed.packages()[, 1])
    bibs <- sapply(pkgs, function(x) try(citation(x)), simplify = FALSE)
    n.installed <- length(bibs)
    ok <- sapply(bibs, inherits, "bibentry")
    pkgs <- pkgs[ok]
    bibs <- bibs[ok]
    n.converted <- sum(ok)
    pkgs <- lapply(seq_along(pkgs), function(i) if (length(bibs[[i]]) > 
                                                      1) 
      paste(pkgs[i], 1:length(bibs[[i]]), sep = "")
                   else pkgs[i])
    pkgs <- do.call("c", pkgs)
    bibs <- do.call("c", bibs)
    as.bibkey <- function(x) {
      i <- grep("[.]", x)
      if (length(i) > 0) 
        x[i] <- paste("{", x[i], "}", sep = "")
      x
    }
    bibs <- mapply(function(b, k) {
      b$key <- k
      b
    }, bibs, pkgs, SIMPLIFY = FALSE)
    bibs <- do.call("c", bibs)
    if (verbose) 
      message("Converted ", n.converted, " of ", n.installed, 
              " package citations to BibTeX")
    bibs
  }
  else stop("Invalid argument 'entry': expected a bibentry object or a character vector of package names.")
  if (length(bibs) == 0) {
    if (verbose) 
      message("Empty bibentry list: nothing to be done.")
    return(invisible())
  }
  if (is.null(file)) 
    file <- stdout()
  else if (is.character(file)) {
    if (!grepl("\\.bib$", file)) 
      file <- paste(file, ".bib", sep = "")
  }
  fh <- file(file, open = if (append) 
    "a+"
             else "w+")
  on.exit(if (isOpen(fh)) close(fh))
  if (verbose) 
    message("Writing ", length(bibs), " Bibtex entries ... ", 
            appendLF = FALSE)
  writeLines(toBibtex(bibs), fh)
  if (verbose) 
    message("OK\nResults written to file '", file, "'")
  invisible(bibs)
}

ReadBib <- function (file = findBibFile(package), package = "bibtex", encoding = "unknown", 
          header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
          footer = "") 
{
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(encoding, unknown = srcfile(file), srcfile(file, 
                                                               encoding = encoding))
  out <- .External("do_read_bib", file = file, encoding = encoding, 
                   srcfile = srcfile)
  at <- attributes(out)
  if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
    out <- lapply(out, MakeBibEntry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- MakeCitationList(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  out
}

MakeBibEntry <- function (x) {
  type <- attr(x, "entry")
  key <- attr(x, "key")
  y <- as.list(x)
  names(y) <- tolower(names(y))
  if ("author" %in% names(y)) {
    y[["author"]] <- ArrangeAuthors(y[["author"]])
  }
  if ("editor" %in% names(y)) {
    y[["editor"]] <- ArrangeAuthors(y[["editor"]])
  }
  tryCatch(BibEntry(bibtype = type, key = key, other = y), 
           error = function(e) {
             message(sprintf("ignoring entry '%s' (line %d) because :\n\t%s\n", 
                             key, attr(x, "srcref")[1], conditionMessage(e)))
             NULL
           })
}

format.Bibentry <- function (x, style = "text", .bibstyle = NULL, citation.bibtex.max = getOption("citation.bibtex.max", 
                                                                               1), sort = FALSE, ...) {
  style <- .bibentry_match_format_style(style)
  if (sort) 
    x <- sort(x, .bibstyle = .bibstyle)
  x$.index <- as.list(seq_along(x))
  .format_bibentry_via_Rd <- function(f) {
    out <- file()
    saveopt <- tools::Rd2txt_options(width = getOption("width"))
    on.exit({
      tools::Rd2txt_options(saveopt)
      close(out)
    })
    sapply(.bibentry_expand_crossrefs(x), function(y) {
      rd <- tools::toRd(y, style = .bibstyle)
      con <- textConnection(rd)
      on.exit(close(con))
      f(con, fragment = TRUE, out = out, ...)
      paste(readLines(out), collapse = "\n")
    })
  }
  .format_bibentry_as_citation <- function(x) {
    bibtex <- length(x) <= citation.bibtex.max
    c(paste(strwrap(attr(x, "mheader")), collapse = "\n"), 
      unlist(lapply(x, function(y) {
        paste(c(if (!is.null(y$header)) c(strwrap(y$header), 
                                          ""), if (!is.null(y$textVersion)) {
                                            strwrap(y$textVersion, prefix = "  ")
                                          } else {
                                            format(y)
                                          }, if (bibtex) {
                                            c(gettext("\nA BibTeX entry for LaTeX users is\n"), 
                                              paste0("  ", unclass(toBibtex(y))))
                                          }, if (!is.null(y$footer)) c("", strwrap(y$footer))), 
              collapse = "\n")
      })), paste(strwrap(attr(x, "mfooter")), collapse = "\n"))
  }
  out <- switch(style, text = .format_bibentry_via_Rd(tools::Rd2txt), 
                html = .format_bibentry_via_Rd(tools::Rd2HTML), latex = .format_bibentry_via_Rd(tools::Rd2latex), 
                Bibtex = {
                  unlist(lapply(x, function(y) paste(toBibtex(y), collapse = "\n")))
                }, textVersion = {
                  out <- lapply(unclass(x), attr, "textVersion")
                  out[!sapply(out, length)] <- ""
                  unlist(out)
                }, citation = .format_bibentry_as_citation(x), R = .format_bibentry_as_R_code(x, 
                                                                                              ...))
  as.character(out)
}

.bibentry_expand_crossrefs <- function (x, more = list()) {
  y <- if (length(more)) 
    do.call(c, c(list(x), more))
  else x
  x <- unclass(x)
  y <- unclass(y)
  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .bibentry_get_key(y))
    ok <- !is.na(pk)
    x[pc[ok]] <- Map(function(u, v) {
      add <- setdiff(names(v), names(u))
      u[add] <- v[add]
      if (!is.na(match(tolower(attr(u, "bibtype")), c("incollection", 
                                                      "inproceedings"))) && is.null(u$booktitle)) 
        u$booktitle <- v$title
      u
    }, x[pc[ok]], y[pk[ok]])
    status <- lapply(x[pc], function(e) tryCatch(.BibentryCheckBibEntry1(e, 
                                                                           TRUE), error = identity))
    bad <- which(sapply(status, inherits, "error"))
    if (length(bad)) {
      for (b in bad) {
        warning(gettextf("Dropping invalid entry %d:\n%s", 
                         pc[b], conditionMessage(status[[b]])))
      }
      x[pc[bad]] <- NULL
    }
  }
  class(x) <- "bibentry"
  x
}

ArrangeAuthors <- function (x) {
  rx <- "[[:space:]]+and[[:space:]]+"
  authors <- lapply(strsplit(x, rx)[[1]], ArrangeSingleAuthor)
  as.personList(authors)
}

ArrangeSingleAuthor <- function(y)
  {
    if( grepl( ",", y) ) {
      y <- sub( "^([^,]+)[[:space:]]*,[[:space:]]*(.*?)$", "\\2 \\1", y , perl = TRUE )
    }
    rx <-  "^[{](.*)[}]$"
    rx2 <- "^([^]]*)[{]([^]]*)[}]$"
    if( grepl( rx, y ) ) {
      person( sub( rx, "\\1", y ) )
    } else if( grepl( rx2, y ) ) {
      person( 
        sub( rx2, "\\1", y ), 
        sub( rx2, "\\2", y )
      )
    } else {
      as.person( y )
    }
  } 

MakeCitationList <- function( x, header, footer)
  {
    rval <- list()
    for( i in seq_along(x) ){
      if( !is.null(x[[i]] ) )
        rval <- c( rval, x[[i]] )
    }
    class(rval) <- c("BibEntry", "bibentry" )
    rval
  }

.is_not_nonempty_text <- function (x) {
  is.null(x) || any(is.na(x)) || all(grepl("^[[:space:]]*$", x))
}

.listify <- function (x) {
  if (inherits(x, "list")) x else list(x)
}