# Mathew McLean
# October 25, 2013
# Make bibentry class compatible with BibLaTeX

source('~/biblatex/code/BibLaTeX_entry_field_db.R')
oldbibentry <- bibentry
old.bibentry_Check_bibentry1 <- utils:::.bibentry_check_bibentry1

test <- ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))

bibentry <- function (bibtype, textVersion = NULL, header = NULL, footer = NULL, 
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
    .bibentry_check_bibentry1(rval)
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
  class(rval) <- "bibentry"
  rval
}

.bibentry_check_bibentry1 <- function (x, force = FALSE) 
{
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