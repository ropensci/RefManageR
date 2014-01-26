print.BibEntry <- function (x, style = "text", .bibstyle = .BibOptions$bib.style, 
                            sorting = .BibOptions$sorting, no.print.fields = NULL, 
                            max.names = .BibOptions$max.names, first.inits = .BibOptions$first.inits, ...){
  opts <- .BibOptions$copy()
  .BibOptions$max.names <- max.names
  .BibOptions$first.inits <- first.inits
  .BibOptions$return.ind <- FALSE
  if (!length(sorting))
    sorting <- switch(.bibstyle, authoryear = 'nyt', alphabetic = 'anyt', draft = 'debug', 'nty')
  style <- .BibEntry_match_format_style(style)
  if (length(x) && length(no.print.fields)){
    for (i in seq_along(no.print.fields))
      x <- do.call(`$<-`, list(x = x, name = tolower(no.print.fields[i]), value = NULL))
  }
  if (style == "R") {
      writeLines(format(x, "R", collapse = TRUE, ...))
  }
  else if (length(x)) {
      y <- format(x, style, .bibstyle, .sorting = sorting, ...)
      if (style == "citation") {
          n <- length(y)
          if (nzchar(header <- y[1L])) 
              header <- c("", header, "")
          if (nzchar(footer <- y[n])) 
              footer <- c("", footer, "")
          writeLines(c(header, paste(y[-c(1L, n)], collapse = "\n\n"), 
              footer))
      }
      else {
          writeLines(paste(y, collapse = "\n\n"))
      }
  }
  .BibOptions <- opts$copy()
  invisible(x)
}