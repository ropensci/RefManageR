#' @keywords internal
#' @noRd
.BibEntryCheckBibEntry1 <- function (x, force = FALSE,
                                     check = .BibOptions$check.entries){
  if (identical(check, FALSE))
    return(NULL)
  fields <- names(x)
  if (!force && (!.is_not_nonempty_text(x$crossref) ||
                 !.is_not_nonempty_text(x$xdata)))
    return(NULL)
  bibtype <- attr(x, "bibtype")
  rfields <- strsplit(BibLaTeX_entry_field_db[[bibtype]],
                      "|", fixed = TRUE)
  if (length(rfields) > 0L) {
    ok <- vapply(rfields, function(f) any(f %in% fields), FALSE)
    if (any(!ok)){
      key <- attr(x, "key")
      if (is.null(key))
          key <- ""
      else
          key <- paste0(key, ": ")
      msg <- sprintf(ngettext(sum(!ok),
                      "%sA bibentry of bibtype %s has to specify the field: %s",
                      "%sA bibentry of bibtype %s has to specify the fields: %s"),
                      key, sQuote(bibtype), paste(rfields[!ok], collapse = ", "))
      if (check == 'warn'){
          warning(msg, domain = NA, call. = FALSE)
        return(NULL)
      }else
          stop(msg, domain = NA)
    }
  }
}

#' @keywords internal
#' @noRd
.BibEntry_match_format_style <- function (style){
    ind <- pmatch(tolower(style), tolower(bibentry_format_styles),
        nomatch = 0L)
    if (all(ind == 0L))
        stop(gettextf("%s should be one of %s", sQuote("style"),
            paste(dQuote(bibentry_format_styles), collapse = ", ")),
            domain = NA)
    bibentry_format_styles[ind]
}

#' @keywords internal
#' @noRd
.BibEntry_expand_crossrefs <- function (x, more = list(), to.bibtex = FALSE){
  if (!length(x))
    return(NULL)
  y <- if (length(more))
    c(x, more)  # do.call(c, c(list(x), more))
  else x
  x <- unclass(x)
  y <- unclass(y)
  xrefs <- lapply(x, '[[', "xdata")
  px <- which(vapply(xrefs, length, 0L) > 0L)
  if (length(px)){
    xk <- lapply(xrefs[px], strsplit, ",")
    # xdata field can be comma-separated list of keys
    x[px] <- Map(function(entry, xdat.keys, full.bib){
      pos <- match(xdat.keys, .BibEntry_get_key(full.bib))
      ok <- !is.na(pos)
      if (any(ok)){
        for (i in pos[ok]){
          xdat <- full.bib[[i]]
          add <- setdiff(names(xdat), names(entry))

          entry[add] <- xdat[add]
          if (any(add %in% .BibEntryDateField))
            attr(entry, 'dateobj') <- ProcessDates(entry)
        }
      }
      entry
    }, x[px], xk, MoreArgs = list(full.bib = y))
  }

  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .BibEntry_get_key(y))
    ok <- !is.na(pk)

    if (to.bibtex){
      x[pc[ok]] <- lapply(x[pc[ok]], function(bib){
          if (attr(bib, 'bibtype') %in% c("InBook", "InCollection",
                                          "InProceedings") &&
                                           is.null(bib$subtitle))
          bib$subtitle <- ''
        bib
      })
      x[pk[ok]] <- lapply(x[pk[ok]], function(bib){
          if (attr(bib, 'bibtype') %in% c("Book", "Proceedings") &&
                                            is.null(bib$subtitle))
          bib$booktitle <- bib$title
        bib
      })
    }else{
      x[pc[ok]] <- Map(ResolveBibLaTeXCrossRef, x[pc[ok]], y[pk[ok]])
    }
    status <- lapply(x[pc], .BibEntryCheckBibEntry1, force = TRUE)
  }
  class(x) <- c("BibEntry", "bibentry")
  x
}

#' @keywords internal
#' @noRd
ResolveBibLaTeXCrossRef <- function(chi, par){
  add <- setdiff(names(par), names(chi))

  # titleaddon and subtitle in parent have special fields for child
  # ensure child with no subtitle, titleaddon don't inherit them incorrectly
  chi.type <- tolower(attr(chi, "bibtype"))
  par.type <- tolower(attr(par, "bibtype"))
  if (!is.na(match(chi.type, c("incollection", "suppcollection", "collection",
                               "reference", "inreference", "inbook", "suppbook",
                               "bookinbook", "book", "inproceedings",
                               "proceedings", "article", "suppperiodical"))))
    add <- add[!add %in% c("subtitle", "titleaddon")]
  chi[add] <- par[add]
  if (any(add %in% .BibEntryDateField))
    attr(chi, 'dateobj') <- ProcessDates(chi)
  ## special handling for bookauthor, maintitle, mainsubtitle, maintitleaddon,
  ## booktitle, booktitleaddon, booksubtitle, journaltitle, journalsubtitle;
  ## see Appendix B of biblatex manual
  if (par.type %in% c('mvbook', 'book') &&
      chi.type %in% c('inbook', 'bookinbook', 'suppbook') &&
      is.null(chi$bookauthor))
      chi$bookauthor <- par$author
  child.table <- list(mvbook = c("book", "inbook", "bookinbook", "suppbook"),
                      mvcollection = c("collection", "reference",
                                       "incollection"),
                      mvreference = c("inreference", "suppcollection"),
                      mvproceedings = c("proceedings", "inproceedings"),
                      book = c("inbook", "bookinbook", "suppbook"),
                      collection = c("incollection", "inreference"),
                      reference = "suppcollection",
                      proceedings = "inproceedings",
                      periodical = c("article", "suppperiodical"))


  if (chi.type %in% child.table[[par.type]])
      chi <- ProcessCrossref(par.type, par, chi)
  chi
}

#' @noRd
ProcessCrossref <- function(par.type, par, chi){
    switch(par.type, mvbook = {
      if (is.null(chi$maintitle))
        chi$maintitle <- par$title
      if (is.null(chi$mainsubtitle))
        chi$mainsubtitle <- par$subtitle
      if (is.null(chi$maintitleaddon))
        chi$maintitleaddon <- par$titleaddon

    }, mvreference = {
      if (is.null(chi$mainsubtitle))
        chi$mainsubtitle <- par$subtitle
      if (is.null(chi$maintitleaddon))
        chi$maintitleaddon <- par$titleaddon
    }, mvcollection = {
      if (is.null(chi$maintitle))
        chi$maintitle <- par$title
    }, mvproceedings = {
      if (is.null(chi$maintitle))
        chi$maintitle <- par$title
      if (is.null(chi$mainsubtitle))
        chi$mainsubtitle <- par$subtitle
      if (is.null(chi$maintitleaddon))
        chi$maintitleaddon <- par$titleaddon
    }, book = {
      if (is.null(chi$booktitle))
        chi$booktitle <- par$title
      if (is.null(chi$booksubtitle))
        chi$booksubtitle <- par$subtitle
      if (is.null(chi$booktitleaddon))
        chi$booktitleaddon <- par$titleaddon
    }, collection = {
      if (is.null(chi$booktitle))
        chi$booktitle <- par$title
    }, reference = {
      if (is.null(chi$booksubtitle))
        chi$booksubtitle <- par$subtitle
      if (is.null(chi$booktitleaddon))
        chi$booktitleaddon <- par$titleaddon
    }, proceedings = {
      if (is.null(chi$booktitle))
        chi$booktitle <- par$title
      if (is.null(chi$booksubtitle))
        chi$booksubtitle <- par$subtitle
      if (is.null(chi$booktitleaddon))
        chi$booktitleaddon <- par$titleaddon
    }, periodical = {
      if (is.null(chi$journaltitle) && is.null(chi$journal))
        chi$journaltitle <- par$title
      if (is.null(chi$journalsubtitle))
        chi$journalsubtitle <- par$subtitle
    })
    chi
}


#' @keywords internal
#' @noRd
ArrangeAuthors <- function (x){
  rx <- "[[:space:]]+and[[:space:]]+"
  x <- gsub('[[:space:]]{2,}', ' ', x, useBytes = TRUE)
  authors <- lapply(strsplit(x, rx)[[1]], ArrangeSingleAuthor)
  do.call("c", authors)
}

#' @keywords internal
#' @importFrom utils person
#' @noRd
ArrangeSingleAuthor <- function(y){
  ## To ensure names are parsed as accurately as possible,
  ##  LaTeX macros are removed with gsub in calls to cleanupLatexSearch
  ##  and braces are removed by UnlistSplitClean
  ##  this behaviour is not the same as for other fields, which only have
  ##  the LaTeX stripped when using SearchBib
  if (grepl('[\\]', y, useBytes = TRUE)){
    tmp <- try(parseLatex(y), TRUE)
    if (!inherits(tmp, 'try-error')){
        tmp <- tryCatch({
            setTimeLimit(cpu = .5, elapsed = .5, transient = TRUE)
            on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
            tools::latexToUtf8(tmp)
        }, error = function(e){
            message(gettextf("Unrecognized macro in %s", y))
                    tmp
                })
      y <- deparseLatex(tmp)
    }
  }
  ## split on commas not in braces
  parts <- unlist(strsplit(y, ", ?(?![^{}]*})", perl = TRUE))
  len.parts <- length(parts)
  if (len.parts == 1L){
    #     parts <- "{Barnes} {and} {Noble,} {Inc.}"
    if (grepl("[^{][[:print:]][}]$", parts, useBytes = TRUE)){
      s <- unlist(strsplit(parts, ''))
      i <- length(s) - 1L
      paren <- 1
      while (paren > 0 && i > 0){
        if (s[i] == '{'){
          paren <- paren - 1L
        }else if (s[i] == '}'){
          paren <- paren + 1L
        }
        i <- i - 1L
      }
      last <- paste0(s[(i+2):(length(s)-1)], collapse = '')
      first <- NULL
      if (i > 0)
          first <- paste0(s[seq_len(i-1)], collapse = '')
      ## Mathew {McLean IX}
      person(UnlistSplitClean(first), cleanupLatexSearch(last))
    }else{
      vonrx <- "(^|[[:space:]])([[:lower:]+[:space:]?]+)[[:space:]]"
      m <- regexec(vonrx, parts, useBytes = TRUE)
      von <- unlist(regmatches(parts, m))[3L]
      if (!is.na(von)){
        name <- unlist(strsplit(parts, vonrx))
        if (length(name) == 1L){  # von Bommel
          person(family=c(cleanupLatexSearch(von), cleanupLatexSearch(name)))
        }else{  # Mark von Bommel
            person(given = UnlistSplitClean(name[1L]),
                   family = c(cleanupLatexSearch(von),
                              cleanupLatexSearch(name[2L])))
        }
      }else{  # George Bernard Shaw
        name <- UnlistSplitClean(parts)
        len.name <- length(name)
        if (len.name <= 1L){
          person(family = name)
        }else{
          person(given = name[seq_len(len.name - 1L)], family = name[len.name])
        }
      }
    }
  }else if (len.parts == 2L){
    if (grepl('^[{]', parts[1L], useBytes = TRUE)){  # e.g. {de Gama}, Vasco
      person(UnlistSplitClean(parts[2L]), UnlistSplitClean(parts[1L]))
    }else{
      vonrx <- "^([[:lower:]+[:space:]?]+)[[:space:]]"
      m <- regexec(vonrx, parts[1L], useBytes = TRUE)
      von <- unlist(regmatches(parts[1L], m))[2]
      if (is.na(von)){  # e.g. Smith, John Paul
        person(UnlistSplitClean(parts[2L]), cleanupLatexSearch(parts[1L]))
      }else{  # e.g. de la Soul, John
          person(UnlistSplitClean(parts[2L]),
                 c(cleanupLatexSearch(von),
                   cleanupLatexSearch(sub(vonrx, '', parts[1L],
                                          useBytes = TRUE))))
      }
    }
  }else if (len.parts == 3L){
    vonrx <- "^([[:lower:]+[:space:]?]+)[[:space:]]"
    m <- regexec(vonrx, parts[1L], useBytes = TRUE)
    von <- unlist(regmatches(parts[1L], m))[2]
    if (is.na(von)){  # e.g. White, Jr., Walter
        person(UnlistSplitClean(parts[3L]), c(cleanupLatexSearch(parts[1L]),
                                              cleanupLatexSearch(parts[2L])))
    }else{  # e.g. des White, Jr., Walter
      person(UnlistSplitClean(parts[3L]),
             c(cleanupLatexSearch(von),
               cleanupLatexSearch(sub(vonrx, '', parts[1L], useBytes = TRUE)),
               cleanupLatexSearch(parts[2L])))
    }
  }else{
    stop('Invalid format.')
  }
}

#' @keywords internal
#' @noRd
UnlistSplitClean <- function(s){
  ## cleanupLatex(str_trim(s))
  unlist(strsplit(gsub("[{}]", "", str_trim(s), useBytes = TRUE), " "))
}

#' @keywords internal
#' @noRd
cleanupLatex <- function (x){
  if (!length(x))
    return(x)
  if (getRversion() < "3.3.0"){
    if (any(grepl('mkbib', x, useBytes = TRUE))){
      x <- gsub('mkbibquote', 'dQuote', x, useBytes = TRUE)
      x <- gsub('mkbibemph', 'emph', x, useBytes = TRUE)
      x <- gsub('mkbibbold', 'bold', x, useBytes = TRUE)
    }
    x <- gsub('\\\\hyphen', '-', x, useBytes = TRUE)
    x <- gsub("\\\\textquotesingle", "'", x, useBytes = TRUE)
  }
  x <- gsub("\\\\&", "&", x, useBytes = TRUE)

  latex <- try(tools::parseLatex(x), silent = TRUE)
  if (inherits(latex, "try-error")) {
    x
  }else {
    latex <- tryCatch({
            on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
            setTimeLimit(cpu = .5, elapsed = .5, transient = TRUE)
            tools::latexToUtf8(latex)
        }, error = function(e){
            ## message(gettextf("Unrecognized macro in %s", x))
                    latex
                })
    x <- tools::deparseLatex(latex, dropBraces = FALSE)
    if (getRversion() < "3.3.0"){
      if (grepl("\\\\[[:punct:]]", x, useBytes = TRUE)){
        x <- gsub("\\\\'I", '\u00cd', x, useBytes = TRUE)
        x <- gsub("\\\\'i", '\u00ed', x, useBytes = TRUE)
        x <- gsub('\\\\"I', '\u00cf', x, useBytes = TRUE)
        x <- gsub('\\\\"i', '\u00ef', x, useBytes = TRUE)
        x <- gsub("\\\\\\^I", '\u00ce', x, useBytes = TRUE)
        x <- gsub("\\\\\\^i", '\u00ee', x, useBytes = TRUE)
        x <- gsub("\\\\`I", '\u00cc', x, useBytes = TRUE)
        x <- gsub("\\\\`i", '\u00ec', x, useBytes = TRUE)
        Encoding(x) <- 'UTF-8'
      }
    }
    x
  }
}

#' @keywords internal
#' @noRd
MakeCitationList <- function( x, header, footer){
    rval <- list()
    for (i in seq_along(x)){
      if (!is.null(x[[i]]))
        rval <- c(rval, unclass(x[[i]]))
    }
    class(rval) <- c("BibEntry", "bibentry" )
    rval
}

#' @keywords internal
#' @noRd
.is_not_nonempty_text <- function(x){
    is.null(x) || any(is.na(x)) || all(grepl("^[[:space:]]*$", x,
                                             useBytes = TRUE))
}

#' @keywords internal
#' @noRd
.listify <- function (x){
  if (inherits(x, "list"))
    x
  else list(x)
}

#' @keywords internal
#' @noRd
.format_BibEntry_as_R_code <- function(x, collapse = FALSE){
  if (!length(x))
    return("bibentry()")
  x$.index <- NULL
  x$dateobj <- NULL
  anames <- bibentry_attribute_names
  manames <- c("mheader", "mfooter")
  .blanks <- function(n) paste(rep.int(" ", n), collapse = "")
  .format_call_RR <- function(cname, cargs){
    cargs <- as.list(cargs)
    n <- length(cargs)
    lens <- vapply(cargs, length, 0L)
    sums <- cumsum(lens)
    starters <- c(sprintf("%s(", cname), rep.int(.blanks(nchar(cname) +
                                                           1L), sums[n] - 1L))
    trailers <- c(rep.int("", sums[n] - 1L), ")")
    trailers[sums[-n]] <- ","
    sprintf("%s%s%s", starters, unlist(cargs), trailers)
  }
  .format_person_as_R_code <- function(x){
    s <- lapply(unclass(x), function(e){
      e <- e[!vapply(e, is.null, FALSE)]
      cargs <- sprintf("%s = %s", names(e), vapply(e, deparse, ""))
      .format_call_RR("person", cargs)
    })
    if (length(s) > 1L)
      .format_call_RR("c", s)
    else unlist(s, use.names = FALSE)
  }
  f <- function(e){
    if (inherits(e, "person"))
      .format_person_as_R_code(e)
    else deparse(e)
  }
  g <- function(u, v){
    prefix <- sprintf("%s = ", u)
    n <- length(v)
    if (n > 1L)
      prefix <- c(prefix, rep.int(.blanks(nchar(prefix)),
                                  n - 1L))
    sprintf("%s%s", prefix, v)
  }
  s <- lapply(unclass(x), function(e){
    a <- Filter(length, attributes(e)[anames])
    e <- e[!vapply(e, is.null, FALSE)]
    ind <- !is.na(match(names(e), c(anames, manames, "other")))
    if (any(ind)) {
      other <- paste(names(e[ind]), vapply(e[ind], f, ""),
                     sep = " = ")
      other <- Map(g, names(e[ind]), vapply(e[ind], f, ""))
      other <- .format_call_RR("list", other)
      e <- e[!ind]
    }
    else {
      other <- NULL
    }
    c(Map(g, names(a), vapply(a, deparse, "")), Map(g, names(e),
                                                    lapply(e, f)),
      if (length(other))
          list(g("other", other)))
  })
  if (!is.null(mheader <- attr(x, "mheader")))
    s[[1L]] <- c(s[[1L]], paste("mheader = ", deparse(mheader)))
  if (!is.null(mfooter <- attr(x, "mfooter")))
    s[[1L]] <- c(s[[1L]], paste("mfooter = ", deparse(mfooter)))
  s <- Map(.format_call_RR, "bibentry", s)
  if (collapse && (length(s) > 1L))
    paste(.format_call_RR("c", s), collapse = "\n")
  else unlist(lapply(s, paste, collapse = "\n"), use.names = FALSE)
}

.format_BibEntry_as_yaml <- function(x, collapse = FALSE){
  if (!length(x))
    return("bibentry()")
  x$.index <- NULL
  x$dateobj <- NULL
  anames <- bibentry_attribute_names
  manames <- c("mheader", "mfooter")
  .clean <- cleanupLatex
  .collapse <- GetFormatFunctions("text", I)$collapse
  .blanks <- function(n) paste(rep.int(" ", n), collapse = "")
  .format_call_RR <- function(cname, cargs){
    cargs <- as.list(cargs)
    # cargs <- lapply(cargs, function(x) collapse(clean(x)))
    n <- length(cargs)
    lens <- vapply(cargs, length, 0L)
    sums <- cumsum(lens)
    starters <- c(sprintf("%s", cname), rep.int(.blanks(nchar(cname) +
                                                           1L), sums[n] - 1L))
    # trailers <- c(rep.int("", sums[n] - 1L), ")")
    trailers <- c(rep.int("", sums[n] - 1L), "")
    # trailers[sums[-n]] <- ","
    sprintf("%s%s%s", starters, unlist(cargs), trailers)
  }
  .format_person_as_yaml <- function(x){
    s <- lapply(unclass(x), function(e){
      e <- e[!vapply(e, is.null, FALSE)]
      cargs <- sprintf("%s: %s", names(e), vapply(e, deparse, ""))
      .format_call_RR("\n    - ", cargs)
    })
    ## if (length(s))
    ##   s <- paste0("\n", s, collapse = "")
    ## if (length(s) > 1L)
    ##  .format_call_RR("\n", s)
    ## else unlist(s, use.names = FALSE)
    ## c("\n", unlist(s, use.names = FALSE))
    ## browser()
    unlist(s, use.names = FALSE)
  }
  f <- function(e){
    if (inherits(e, "person"))
      .format_person_as_yaml(e)
    else deparse(e)
  }
  g <- function(u, v){
    if (u == "bibtype")
      u <- "type"
    if (u == "key")
      u <- "id"
    prefix <- sprintf("%s: ", u)
    v <- .collapse(.clean(v))

    n <- length(v)
    if (n > 1L)
      prefix <- c(prefix, rep.int(.blanks(nchar(prefix)),
                                  n - 1L))
    sprintf("%s%s", prefix, v)
  }
  s <- lapply(unclass(x), function(e){
    a <- Filter(length, attributes(e)[anames])
    e <- e[!vapply(e, is.null, FALSE)]
    ind <- !is.na(match(names(e), c(anames, manames, "other")))
    if (any(ind)) {
      other <- paste(names(e[ind]), vapply(e[ind], f, ""),
                     sep = " = ")
      other <- Map(g, names(e[ind]), vapply(e[ind], f, ""))
      other <- .format_call_RR("list", other)
      e <- e[!ind]
    }
    else {
      other <- NULL
    }
    c(Map(g, names(a), vapply(a, deparse, "")), Map(g, names(e),
                                                    lapply(e, f)),
      if (length(other))
          list(g("other", other)))
  })
  if (!is.null(mheader <- attr(x, "mheader")))
    s[[1L]] <- c(s[[1L]], paste("mheader = ", deparse(mheader)))
  if (!is.null(mfooter <- attr(x, "mfooter")))
    s[[1L]] <- c(s[[1L]], paste("mfooter = ", deparse(mfooter)))
  s <- Map(.format_call_RR, "- ", s)
  if (collapse && (length(s) > 1L))
    paste(.format_call_RR("", s), collapse = "\n")
  else unlist(lapply(s, paste, collapse = "\n"), use.names = FALSE)
}

bibentry_attribute_names <- c("bibtype", "textVersion", "header", "footer",
                              "key", "dateobj")
bibentry_format_styles <- c("text", "Bibtex", "citation", "html", "latex",
                            "textVersion", "R", "Biblatex", "markdown", "yaml")

# from utils:::toBibtex, good for matching by given name initials only
#' @keywords internal
#' @noRd
format_author <- function(author) paste(vapply(author, function(p) {
  fnms <- p$family
  only_given_or_family <- is.null(fnms) || is.null(p$given)
  fbrc <- if (length(fnms) > 1L || any(grepl("[[:space:]]",
                                             fnms, useBytes = TRUE)) ||
              only_given_or_family)
    c("{", "}")
  else ""
  gbrc <- if (only_given_or_family)
    c("{", "}")
  else ""
  format(p, include = c("given", "family"), braces = list(given = gbrc,
                                                          family = fbrc))
}, ""), collapse = " and ")

bibentry_list_attribute_names <- c("mheader", "mfooter", "strings")

#' @keywords internal
#' @noRd
.BibEntry_get_key <- function (x){
  if (!length(x))
    return(character())
  keys <- lapply(unclass(x), attr, "key")
  keys[!vapply(keys, length, 0L)] <- ""
  unlist(keys)
}

## @keywords internal
## @importFrom XML xmlValue
## @importFrom stringr str_sub str_trim
## ParseGSCites <- function(l, encoding,
##                   check.entries=.BibOptions$check.entries){
##   if (!length(l))
##     return(list())
##   td <- l[[1L]]

##   title <- xmlValue(td[[1L]], encoding)
##   author <- xmlValue(td[[3L]], encoding)
##   cited_by <- as.numeric(xmlValue(l[[2L]][[1L]], encoding))
##   if (is.na(cited_by))  # no citation yet
##     cited_by <- "0"
##   year <- as.numeric(xmlValue(l[[4L]], encoding))
##   src <- xmlValue(td[[5L]])
##   first_digit <- as.numeric(regexpr("[\\[\\(]?\\d",
##                                     src)) - 1L
##   ids <- which(first_digit < 0L)
##   first_digit <- replace(first_digit, ids, str_length(src)[ids])
##   journal <- str_trim(str_sub(src, 1L, first_digit))
##   trailing_commas <- as.numeric(regexpr(",$", journal)) - 1L
##   ids <- which(trailing_commas < 0L)
##   trailing_commas <- replace(trailing_commas, ids,
##                              str_length(journal)[ids])
##   journal <- str_sub(journal, 1L, trailing_commas)
##   numbers <- str_trim(str_sub(src, first_digit + 1L,
##                               str_length(src)))

##   # handle '...' in title, journal, or authors
##   if (!identical(check.entries, FALSE)){
##     if (is.null(title <- CheckGSDots(title, title, check.entries)) ||
##           is.null(author <- CheckGSDots(author, title, check.entries)) ||
##           is.null(journal <- CheckGSDots(journal, title, check.entries)))
##       return(NA)
##   }

##   res <- list(title = title, author = author,
##               journal = journal, number = numbers, cites = cited_by,
##               year = year)
##   if (is.na(res$number) || res$number==''){  # assume book entry if no number
##     if (as.numeric(cited_by) < 10L){
##       attr(res, 'entry') <- "report"
##       res$institution <- res$journal
##       res$type <- "techreport"
##     }else{
##       attr(res, 'entry') <- "book"
##       res$publisher <- res$journal
##     }
##     res$number <- NULL
##     res$journal <- NULL
##   }else{
##     attr(res, 'entry') <- 'article'
##     numbers <- ProcessGSNumbers(res$number)
##     res$number <- numbers$number
##     res$pages <- numbers$pages
##     res$volume <- numbers$volume
##   }

##   res$author <- ProcessGSAuthors(res$author)  # format authors for MakeBibEntry
##   # create key
##   attr(res, "key") <- CreateBibKey(res$title, res$author, res$year)

##   return(res)
## }


#' @keywords internal
#' @noRd
ProcessArxiv <- function(arxinfo){
  res <- list(eprinttype = 'arxiv')
  # need to check date since arXiv identifier format changed in Apr-07
  m <- regexpr('[0-9]{1,2}[[:space:]][A-Z][a-z]{2}[[:space:]][0-9]{4}',
               arxinfo, useBytes = TRUE)

  adate <- strptime(regmatches(arxinfo, m), format='%d %b %Y')
  if (length(adate) && adate >= strptime('01 Apr 2007', format='%d %b %Y')){
    p <- 'arXiv:([0-9]{4}[\\.][0-9]{4}v[0-9])[[:space:]]\\[([[:graph:]]+)\\]'
    m <- regexec(p, arxinfo, useBytes = TRUE)
    regm <- regmatches(arxinfo, m)
    res$eprintclass <- regm[[1]][3]
    res$eprint <- regm[[1]][2]
  }else{
    m <- regexec('arXiv:([[:graph:]]+)\\s', arxinfo, useBytes = TRUE)
    regm <- regmatches(arxinfo, m)
    res$eprint <- regm[[1]][2]
  }
  res$url <- paste0('https://arxiv.org/abs/', res$eprint)
  res
}

## @keywords internal
## @importFrom stringr str_sub str_trim
## ParseGSCites2 <- function(l, encoding,
##                           check.entries=.BibOptions$check.entries){
##   if (!length(l))
##     return(list())
##   td <- l[[1L]]

##   title <- td[[1L]][[1L]]  # xmlValue(td[[1L]], encoding)
##   author <- td[[2L]][[1L]]  # xmlValue(td[[2L]], encoding)
##   cited_by <- as.numeric(l[[2L]][[1L]][[1]])
##   if (is.na(cited_by))  # no citation yet
##     cited_by <- "0"
##   src <- td[[3L]][[1L]]  # xmlValue(td[[3L]], encoding)

##   year <- as.numeric(regmatches(td[[3L]][[2L]][[1]],
##                      regexpr("([12][0-9]{3}$)", src, useBytes = TRUE)))
##   first_digit <- as.numeric(regexpr("[\\[\\(]?\\d",
##                                     src, useBytes = TRUE)) - 1L
##   ids <- which(first_digit < 0L)
##   first_digit <- replace(first_digit, ids, str_length(src)[ids])
##   journal <- str_trim(str_sub(src, 1L, first_digit))
##   trailing_commas <- as.numeric(regexpr(",$", journal, useBytes = TRUE)) - 1L
##   ids <- which(trailing_commas < 0L)
##   trailing_commas <- replace(trailing_commas, ids,
##                              str_length(journal)[ids])
##   journal <- str_sub(journal, 1L, trailing_commas)
##   numbers <- str_trim(str_sub(src, first_digit + 1L,
##                               str_length(src)))
##   # handle '...' in title, journal, or authors
##   if (!identical(check.entries, FALSE)){
##     if (is.null(title <- CheckGSDots(title, title, check.entries)) ||
##           is.null(author <- CheckGSDots(author, title, check.entries)) ||
##           is.null(journal <- CheckGSDots(journal, title, check.entries)))
##       return(NA)
##   }

##   res <- list(title = title, author = author, cites = cited_by,
##               year = year)
##   if (!is.na(eprint <- regmatches(src, regexec("arXiv:([0-9.]*)", src,
##                                                useBytes = TRUE))[[1]][2])){
##     res$eprinttype <- "arxiv"
##     res$eprint <- eprint
##     res$url <- paste0("https://arxiv.org/abs/", eprint)
##     attr(res, "entry") <- "misc"
##   }else{
##     if (is.na(numbers) || numbers == "" || as.character(year) == numbers){
##       if (as.numeric(cited_by) < 10L){
##         attr(res, "entry") <- "report"
##         res$institution <- journal
##         res$type <- "techreport"
##       }else{
##         attr(res, "entry") <- "book"
##         res$publisher <- res$journal
##       }
##     }else{
##       res$journal <- journal
##       res$number <- numbers
##       attr(res, 'entry') <- 'article'
##       numbers <- ProcessGSNumbers(res$number)
##       res$number <- numbers$number
##       res$pages <- numbers$pages
##       res$volume <- numbers$volume
##     }
##   }

##   res$author <- ProcessGSAuthors(res$author)  # format authors for MakeBibEntry
##   # create key
##   attr(res, "key") <- CreateBibKey(res$title, res$author, res$year)

##   return(res)
## }

#' @keywords internal
#' @noRd
#' @importFrom stringr str_sub str_trim
ParseGSCitesNew <- function(title, author, year, src, cited_by, encoding,
                            check.entries=.BibOptions$check.entries){
  ## if (!length(l))
  ##   return(list())
  ## td <- l[[1L]]

  ## title <- td[[1L]][[1L]]  # xmlValue(td[[1L]], encoding)
  ## author <- td[[2L]][[1L]]  # xmlValue(td[[2L]], encoding)
  ## cited_by <- as.numeric(l[[2L]][[1L]][[1]])
  if (is.na(as.numeric(cited_by)))  # no citation yet
    cited_by <- "0"
  ## src <- td[[3L]][[1L]]  # xmlValue(td[[3L]], encoding)

    ## year <- as.numeric(regmatches(td[[3L]][[2L]][[1]],
    ##                    regexpr("([12][0-9]{3}$)", src, useBytes = TRUE)))
  first_digit <- as.numeric(regexpr("[\\[\\(]?\\d",
                                    src, useBytes = TRUE)) - 1L
  ids <- which(first_digit < 0L)
  first_digit <- replace(first_digit, ids, str_length(src)[ids])
  journal <- str_trim(str_sub(src, 1L, first_digit))
  trailing_commas <- as.numeric(regexpr(",$", journal, useBytes = TRUE)) - 1L
  ids <- which(trailing_commas < 0L)
  trailing_commas <- replace(trailing_commas, ids,
                             str_length(journal)[ids])
  journal <- str_sub(journal, 1L, trailing_commas)
  numbers <- str_trim(str_sub(src, first_digit + 1L,
                              str_length(src)))
  # handle '...' in title, journal, or authors
  if (!identical(check.entries, FALSE)){
    if (is.null(title <- CheckGSDots(title, title, check.entries)) ||
          is.null(author <- CheckGSDots(author, title, check.entries)) ||
          is.null(journal <- CheckGSDots(journal, title, check.entries)))
      return(NA)
  }

  res <- list(title = title, author = author, cites = cited_by,
              year = year)
  if (!is.na(eprint <- regmatches(src, regexec("arXiv:([0-9.]*)", src,
                                               useBytes = TRUE))[[1]][2])){
    res$eprinttype <- "arxiv"
    res$eprint <- eprint
    res$url <- paste0("https://arxiv.org/abs/", eprint)
    attr(res, "entry") <- "misc"
  }else{
    if (is.na(numbers) || numbers == "" || as.character(year) == numbers){
        if (.is_not_nonempty_text(journal))
            attr(res, "entry") <- "misc"
        else{
          if (as.numeric(cited_by) < 10L){
            attr(res, "entry") <- "report"
            res$institution <- journal
            res$type <- "techreport"
          }else{
            attr(res, "entry") <- "book"
            res$publisher <- res$journal
          }
        }
    }else{
      res$journal <- journal
      res$number <- numbers
      attr(res, 'entry') <- 'article'
      numbers <- ProcessGSNumbers(res$number)
      res$number <- numbers$number
      res$pages <- numbers$pages
      res$volume <- numbers$volume
    }
  }

  res$author <- ProcessGSAuthors(res$author)  # format authors for MakeBibEntry
  # create key
  attr(res, "key") <- CreateBibKey(res$title, res$author, res$year)

  return(res)
}

#' @keywords internal
#' @noRd
#' @importFrom utils as.person
ProcessGSAuthors <- function(authors){
  # authors <- gsub(',', ', and', authors)  # add "and" to separate authors
  ## add space between given name initials
  # authors <- gsub('([A-Z])([A-Z])', '\\1 \\2', authors)
  authors <- gsub(", [.]{3}$", "", authors, useBytes = TRUE)
  authors <- strsplit(authors, ", ")[[1]]

  ## need to ensure given name initials are processed correctly,
  ## GS returns them without spaces
  m <- regexec("^([[:alpha:]]*)[[:space:]](.*)", authors, useBytes = TRUE)
  autList <- regmatches(authors, m)
  autList <- lapply(seq_along(authors), function(i){
    if (length(name <- autList[[i]]))
      paste0(gsub("(.)", "\\1 ", name[2]), name[3])
    else authors[[i]]
  } )

  ## autList <- lapply(regmatches(authors, m), function(name)
  ##                   paste0(gsub("(.)", "\\1 ", name[2]), name[3]))
  authors <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", autList, perl = TRUE)

  return(as.person(authors))
}

#' @keywords internal
#' @noRd
ProcessGSNumbers <- function(numbers){
  pages <- volume <- number <- NULL

  m <- regexpr('([0-9]+)', numbers, useBytes = TRUE)
  if(m != -1)
    volume <- regmatches(numbers, m)

  m <- regexpr('[(]([0-9]+)[)]', numbers, useBytes = TRUE)
  if(m != -1){
    number <- regmatches(numbers, m)
    number <- substr(number, 2, nchar(number)-1)  # remove ( )
  }

  m <- regexpr('[0-9]+[\\-][0-9]+', numbers, useBytes = TRUE)
  if(m != -1){
    pages <- regmatches(numbers, m)
    pages <- gsub('-', '--', pages)  # '-' --> '--'
  }

  return(list(pages = pages, number = number, volume = volume))
}

#' @keywords internal
CheckGSDots <- function(x, title, check){
  tx <- gsub(' [.]{3,}$', '', x)
  if(is.na(x) || tx != x){
    entry <- deparse(substitute(x))
    if (check == 'warn'){
        warning(paste0('Incomplete ', entry, ' information for entry \"',
                       title, '\" adding anyway'))
      return(tx)
    }else{
        message(paste0('Incomplete ', entry, ' information for entry \"',
                       title, '\" it will NOT be added'))
      return()
    }
  }else{
    return(tx)
  }
}

#' @keywords internal
MakeBibEntry <- function(x, to.person = TRUE){
  type <- attr(x, "entry")
  key <- attr(x, "key")
  y <- as.list(x)
  names(y) <- tolower(names(y))
  fun <- ifelse(to.person, "ArrangeAuthors", "as.person")
  name.fields <- intersect(names(y), .BibEntryNameList)
  line.no <- if (is.null(attr(x, "srcref")))
               " "
             else
               paste0(" (line", attr(x, "srcref")[1], ") ")
  lapply(name.fields, function(fld)
         y[[fld]] <<- tryCatch(do.call(fun, list(y[[fld]])),
                        error = function(e){
                            message("Ignoring entry ", sQuote(key), line.no,
                                    "because: \n\tThe name list field ",
                                    fld, " cannot be parsed\n")
                            NA
                            }))
  # Check if any were invalid, if so don't add entry
  if (any(is.na(y)))
       return(NULL)
  ## if (to.person){
  ##   lapply(intersect(names(y), .BibEntryNameList), function(fld){
  ##                if (fld %in% names(y))
  ##                   y[[fld]] <<- ArrangeAuthors(y[[fld]])
  ##            })
  ## }else{
  ##   lapply(.BibEntryNameList, function(fld){
  ##            if (fld %in% names(y))
  ##               y[[fld]] <<- as.person(y[[fld]])
  ##        })
  ## }

  tdate <- NULL
  if (type != 'set')
    tdate <- ProcessDates(y)

  withCallingHandlers(tryCatch(BibEntry(bibtype = type, key = key,
                                        dateobj = tdate, other = y),
            error = function(e){
                message(sprintf("Ignoring entry '%s' %sbecause:\n\t%s",
                         key,
                         line.no,
                         conditionMessage(e)), domain = NA)
                NULL
            }),
           warning = function(w){
                warning(sprintf("%s %s:\n\t%s",
                         key,
                         line.no,
                         conditionMessage(w)), domain = NA, call. = FALSE)
                invokeRestart("muffleWarning")
            })
}

#' @keywords internal
ProcessDates <- function(bib){
  tdate <- try(ProcessDate(bib[['date']], NULL), TRUE)

  if (inherits(tdate, 'try-error') || is.null(tdate)){
    tdate <- try(ProcessDate(bib[['year']], bib[['month']]), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))
      tdate <- try(ProcessDate(bib[['eventdate']], NULL), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))
      tdate <- try(ProcessDate(bib[['origdate']], NULL), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))
      tdate <- try(ProcessDate(bib[['urldate']], NULL), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))
      tdate <- NULL
  }

  return(tdate)
}

#' @keywords internal
#' @importFrom lubridate interval parse_date_time
ProcessDate <- function(dat, mon, searching = FALSE){
  if (!length(dat))
    return()

  .day <- FALSE  # does entry contain valid day?
  .mon <- FALSE  # does entry contain valid month?
  if (grepl("^(1|2)[0-9]{3}((-?-|/)(1|2)[0-9]{3})?$", dat, useBytes = TRUE)){
    ## e.g. 1991, 1991--1992, or 1991/1992
    ## check for year specified as range
    dats <- strsplit(dat, "-?-|/")[[1]]
    if (length(dats) == 1L)
      dats <- c(dat, dat)
    if (!.is_not_nonempty_text(mon)){
      ## Some Bibtex users, e.g. paperpile.com, format month as
      ## `"day~" # month` in bib entry;
      ##   attempt to handle this
      ## examples: "2~" # dec, "4--6~" # aug, jan # "/" # feb,
      res <- try(if (grepl("~", mon, useBytes = TRUE)){
               days <- strsplit(sub("~.*", "", mon), "-?-")[[1L]]
               mons <- strsplit(sub(".*~", "", mon), "/")[[1L]]
               .day <- TRUE
               if (length(days) == 2){
                  if (length(mons) == 1)
                   interval(parse_date_time(paste0(dats[1], "-", mons, "-",
                                                   days[1]),
                                            c("%Y-%m-%d", "%Y-%b-%d"),
                                            locale = "C"),
                            parse_date_time(paste0(dats[2], "-", mons, "-",
                                                   days[2]),
                                            c("%Y-%m-%d", "%Y-%b-%d"),
                                            locale = "C"))
                  else if(length(mons) == 2)
                      interval(parse_date_time(paste0(dats[1], "-", mons[1],
                                                      "-", days[1]),
                                               c("%Y-%m-%d", "%Y-%b-%d"),
                                               locale = "C"),
                               parse_date_time(paste0(dats[2], "-", mons[2],
                                                      "-", days[2]),
                                               c("%Y-%m-%d", "%Y-%b-%d"),
                                               locale = "C"))
                  else NA
              }else{
                if (length(mons) == 1){
                  if (dats[1] == dats[2])
                    parse_date_time(paste0(dat, "-", mons, "-", days),
                                    c("%Y-%m-%d", "%Y-%b-%d"), locale = "C")
                  else
                      interval(parse_date_time(paste0(dats[1], "-", mons, "-",
                                                      days),
                                               c("%Y-%m-%d", "%Y-%b-%d"),
                                               locale = "C"),
                               parse_date_time(paste0(dats[2], "-", mons,
                                                      "-", days),
                                               c("%Y-%m-%d", "%Y-%b-%d"),
                                               locale = "C"))
                }else if (length(mons) == 2)
                    interval(parse_date_time(paste0(dats[1], "-", mons[1], "-",
                                                    days),
                                             c("%Y-%m-%d", "%Y-%b-%d"),
                                             locale = "C"),
                             parse_date_time(paste0(dats[2], "-", mons[2],
                                                    "-", days),
                                             c("%Y-%m-%d", "%Y-%b-%d"),
                                             locale = "C"))
                else NA
              }
          }else if (grepl("/", mon, useBytes = TRUE)){  # feb/mar
            mons <- strsplit(mon, "/")[[1L]]
            interval(parse_date_time(paste0(dats[1], "-", mons[1], "-01"),
                                     c("%Y-%m-%d", "%Y-%b-%d"), locale = "C"),
                     parse_date_time(paste0(dats[2], "-", mons[2], "-01"),
                                     c("%Y-%m-%d", "%Y-%b-%d"), locale = "C"))
          }else{
            if (dats[1] == dats[2])
                parse_date_time(paste0(dat, '-', mon, '-01'),
                                c("%Y-%m-%d", "%Y-%b-%d"), locale = "C")
            else
                interval(parse_date_time(paste0(dats[1], '-', mon, '-01'),
                                         c("%Y-%m-%d", "%Y-%b-%d"),
                                         locale = "C"),
                         parse_date_time(paste0(dats[2], '-', mon, '-01'),
                                         c("%Y-%m-%d", "%Y-%b-%d"),
                                         locale = "C"))
          }, TRUE)
      if (inherits(res, "try-error") || is.na(res)){
        warning(gettextf("Failed to parse month: %s%s", mon,
                       ". Ignoring and using year only."))
       .day <- FALSE
      }else
        .mon <- TRUE
    }
    if (!.mon)  # mon is missing or failed to parse
      res <- if (dats[1] == dats[2])
               as.POSIXct(paste0(dat, '-01-01'))
             else
               interval(as.POSIXct(paste0(dats[1], '-01-01')),
                        as.POSIXct(paste0(dats[2], '-01-01')))

  }else if (grepl('^(1|2)[0-9]{3}/$', dat, useBytes = TRUE)){
    if (!is.null(mon)){
        res <- interval(parse_date_time(paste0(substring(dat, 1, 4), '-',
                                               mon, '-01'),
                                        c("%Y-%m-%d", "%Y-%b-%d"), locale="C"),
                          Sys.Date())
      .mon <- TRUE
    }else{
      res <- interval(paste0(substring(dat, 1, 4), '-01-01'), Sys.Date())
    }
  }else if (grepl('^(1|2)[0-9]{3}-[01][0-9]/$', dat, useBytes = TRUE)){
    res <- interval(paste0(substring(dat, 1, 7), '-01'), Sys.Date())
    .mon <- TRUE
  }else if (grepl('^(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]/$', dat,
                  useBytes = TRUE)){
    res <- interval(substring(dat, 1, 10), Sys.Date())
    .mon <- .day <- TRUE
  }else if (grepl('^(1|2)[0-9]{3}-[01][0-9]$', dat, useBytes = TRUE)){
    res <- as.POSIXct(paste0(dat, '-01'))
    .mon <- TRUE
  }else if (grepl('^(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]$', dat, useBytes=TRUE)){
    res <- as.POSIXct(dat)
    .day <- .mon <- TRUE
    ## already handled above:
    ## }else if (grepl('^(1|2)[0-9]{3}/(1|2)[0-9]{3}$', dat)){
    ##  res <- interval(paste0(substring(dat, 1, 4), '-01-01'),
    ## paste0(substring(dat, 6, 9), '-01-01'))
  }else if (grepl('^(1|2)[0-9]{3}-[01][0-9]/(1|2)[0-9]{3}-[01][0-9]$', dat,
                  useBytes = TRUE)){
    res <- interval(paste0(substring(dat, 1, 7), '-01'),
                    paste0(substring(dat, 9, 15), '-01'))
    .mon <- TRUE
  }else if (grepl(paste0("^(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]/(1|2)[0-9]{3}",
                         "-[01][0-9]-[0-3][0-9]$"), dat, useBytes = TRUE)){
    res <- interval(substring(dat, 1, 10), substring(dat, 12, 21))
    .day <- .mon <- TRUE
  }else if (searching){
    if (grepl('^/(1|2)[0-9]{3}$', dat, useBytes = TRUE)){
      res <- interval('0001-01-01', paste0(substring(dat, 2, 5), '-01-01'))
    }else if (grepl('^/(1|2)[0-9]{3}-[01][0-9]$', dat, useBytes = TRUE)){
      res <- interval('0001-01-01', paste0(substring(dat, 2, 8), '-01'))
    }else if (grepl('^/(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]$', dat,
                    useBytes = TRUE)){
      res <- interval('0001-01-01', substring(dat, 2, 11))
    }else{
      stop('No valid date format available.')
    }
  }else{
    stop('No valid date format available.')
  }
  attr(res, 'day.mon') <- .day + .mon
  return(res)
}

#' @keywords internal
CreateBibKey <- function(ti, au, yr){
  ## useBytes = TRUE can cause error to be thrown by tolower if non-ASCII
  ## character match because regmatches will return string with "bytes" encoding

  ## will be character(0) if no matches or if ti is NULL
  key.title <- try({
        m <- regexpr("\\w{4,}", ti, useBytes = FALSE, perl = FALSE)
        tolower(regmatches(ti, m))
  }, TRUE)
  if (inherits(key.title, "try-error"))
      key.title <- ""
  if (inherits(au, 'person')){
    au <- gsub('[^a-z]', '', tolower(au[1L]$family[1L]))
    res <- paste0(au, yr, key.title)
  }else{
    res <- paste0(key.title, yr)
  }
  if (!length(res))
    return()

  return(res)
}

.BibEntryNameList <- c('author', 'editor', 'editora', 'editorb', 'editorc',
                       'translator', 'commentator', 'annotator',
                       'introduction', 'foreword', 'afterword', 'bookauthor',
                       'holder')

.BibEntryDateField <- c('date', 'year', 'month', 'eventdate', 'origdate',
                        'urldate')
.BibEntryTypeField <- c(mathesis = 'MA Thesis', phdthesis = 'PhD thesis',
                        datacd = 'CD-ROM', candthesis = 'Cand. thesis',
                        techreport = 'Tech. rep.', resreport = 'Research rep.',
                        software = 'Comp. software', audiocd = 'Audio CD')


#' @keywords internal
MakeKeysUnique <- function(bib){
    if (length(bib)){
        keys <- make.unique(names(bib), sep = ":")
        bib <- unclass(bib)
        for (i in seq_along(bib))
            attr(bib[[i]], "key") <- keys[i]
        class(bib) <- c("BibEntry", "bibentry")
    }
    bib
}
