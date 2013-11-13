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