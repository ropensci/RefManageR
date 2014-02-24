#' Encode in a common format
#' 
#' Format a BibEntry object in a pretty format.
#' 
#' @param x - an object of class BibEntry
#' @param style
#' @return character vector containing formatted BibEntry object.
#' @importFrom tools Rd2txt_options Rd2txt Rd2HTML Rd2latex
#' @S3method format BibEntry
#' @keywords internal
#' @seealso \code{\link{print.BibEntry}}, \code{\link{BibEntry}}
format.BibEntry <- function(x, style = .BibOptions$style, .bibstyle = .BibOptions$bib.style, 
                             citation.bibtex.max = getOption("citation.bibtex.max", 1), .sort = TRUE, 
                            .sorting = 'nty', enc = 'UTF-8', ...){
    style <- .BibEntry_match_format_style(style)
    ret.ind <- .BibOptions$return.ind
    .BibOptions$return.ind <- FALSE
    on.exit(.BibOptions$return.ind <- ret.ind)
    if (.sort && !style %in% c('html', 'text', 'latex', "markdown")) 
      x <- sort(x, .bibstyle = .bibstyle, sorting = .sorting, return.labs = TRUE)

    .format_bibentry_via_Rd <- function(f){
        out <- file()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({
            tools::Rd2txt_options(saveopt)
            close(out)
        })
        x <- .BibEntry_expand_crossrefs(x)
        if (.sort) 
          x <- sort(x, .bibstyle = .bibstyle, sorting = .sorting, return.labs = TRUE)
        res <- sapply(x, function(y) {
          rd <- toRd.BibEntry(y, .style = .bibstyle, .sorting = 'none')
          con <- textConnection(rd)
          on.exit(close(con))
          f(con, fragment = TRUE, out = out, outputEncoding = 'UTF-8', ...)
          paste(readLines(out, encoding = 'UTF-8'), collapse = "\n")
        })
        if (style == "html")  
          res <- sub("<code>([[:print:]]*)</code>", "<a id='bib-\\1'></a>", res)
        res
#         res <- sub("LINK&lt;a id='([[:print:]]*)'&lt;/a&gt;", "<a id='\\1'></a>", res)
#         if (style %in% c("html", "markdown") && length(.cites$indices)){
#           if (style == "markdown"){
#             patt <- switch(.bibstyle, alphabetic = "(^\\[.*\\])", numeric = "(^\\[.*\\])",
#                          authoryear = "(^.*)([[:space:]]\\()", 
#                          authortitle = paste0("(^.*)([[:space:]]\u201c|[[:space:]]_)",
#                                               "|(^_.*_)|(^\u201c.*\u201d)"),
#                          draft = "(^\\*.*\\*)", 
#                          "(^.*)([[:space:]]\u201c|[[:space:]]_)")
#           }else{
#             patt <- switch(.bibstyle, alphabetic = "^(<p>)(\\[.*\\])", 
#                            numeric = "^(<p>)(\\[.*\\])",
#                            authoryear = "^(<p>)(.*\\()",
#                            draft = "^(<p>)(<B>[[:print:]]*</B>)", 
#                            paste0("^(<p>)(.*)(\\.[[:space:]]&ldquo;|\\.[[:space:]]<EM>)",
#                            "|(<EM>.*</EM>)|(&ldquo;.*&rdquo;)"))  # missing author
#           }
#           repl <- switch(style, html = expression(paste0("\\1<a id='", key, 
#                                             "'></a><a href='#cite-", key, "'>", "\\2\\4\\5</a>\\3")),
#                          markdown = expression(paste0("<a name=", 
#                                         key, "></a>[", "\\1\\3\\4", "](#cite-", key, ")", "\\2")))
#           #browser()
#           res <- mapply(function(entry, key){
#             ind <- .cites$indices[key]
#             if (!is.na(ind) && ind){
#               key <- gsub("[^_a-zA-Z0-9-]", "", key)
#               sub(patt, eval(repl), entry)
#             }else entry
#           }, res, x$key, USE.NAMES = FALSE)
# #           if (.bibstyle %in% c("alphabetic", "numeric")){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^\\[.*\\])", paste0("<a href=#", key, ">[", "\\1", "](#cite-", key, ")"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)
# #           }else if (.bibstyle %in% "authoryear"){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^.*)( \\()", 
# #                     paste0("<a href=#", key, ">[", "\\1", "](#cite-", key, ")", "\\2"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)                                    
# #           }else if (bibstyle %in% "authortitle"){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^.*)( \\u201c| _)", 
# #                     paste0("<a href=#", key, ">[", "\\1", "](#cite-", key, ")", "\\2"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)                        
# #           }else if (bibstyle %in% "draft"){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^\\*.*\\*)", paste0("<a href=#", key, "><a href=#cite-", key, ">",
# #                                           "\\1<\a>"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)            
# #           }          
# #         }else if (style == "markdown" && length(.cites$indices)){
# #           if (.bibstyle %in% c("alphabetic", "numeric")){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^\\[.*\\])", paste0("<a name=#", key, ">[", "\\1", "](#cite-", key, ")"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)
# #           }else if (.bibstyle %in% "authoryear"){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^.*)( \\()", 
# #                     paste0("<a name=#", key, ">[", "\\1", "](#cite-", key, ")", "\\2"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)                                    
# #           }else if (bibstyle %in% "authortitle"){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^.*)( \\u201c| _)", 
# #                      paste0("<a name=#", key, ">[", "\\1", "](#cite-", key, ")", "\\2"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)                        
# #           }else if (bibstyle %in% "draft"){
# #             out <- mapply(function(entry, key){
# #               if (.cites$indices[key])
# #                 sub("(^\\*.*\\*)", paste0("<a name=#", key, ">[", "\\1", "](#cite-", key, ")"), entry)
# #               else x
# #             }, out, x$key, USE.NAMES = FALSE)            
# #           }
#           if (style == "markdown")
#             res <- sub("\u2014\u2013\u2014", "\\-\\-\\-", res)  
#           else res <- sub("\u2014\u2013\u2014", "&#45;&#45;&#45;", res)  
#           
#           # res <- sub("<URL:[[:space:]](.*)>", "[\\1](\\1)", res)
#          # out <- 
#         }
#         res
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
                  markdown = .format_bibentry_via_Rd(tools::Rd2txt), 
        html = .format_bibentry_via_Rd(tools::Rd2HTML), 
        latex = .format_bibentry_via_Rd(tools::Rd2latex), 
        Bibtex = {
           x$.duplicated <- NULL
           unlist(lapply(x, function(y) paste(toBiblatex(y), collapse = "\n")))
        }, Bibtex = {
            x$.duplicated <- NULL
            unlist(lapply(x, function(y) paste(toBibtex(y), collapse = "\n")))
        }, textVersion = {
            out <- lapply(unclass(x), attr, "textVersion")
            out[!sapply(out, length)] <- ""
            unlist(out)
        }, citation = .format_bibentry_as_citation(x), R = {
          x$.duplicated <- NULL
          .format_BibEntry_as_R_code(x, 
            ...)
          })
    as.character(out)
}


Rd2HTMLplus <- function(Rd, out = "", package = "", defines = .Platform$OS.type, 
                         Links = NULL, Links2 = NULL, stages = "render", outputEncoding = "UTF-8", 
                         dynamic = FALSE, no_links = FALSE, fragment = FALSE, stylesheet = "R.css", 
                         ...){
  kall <- match.call()
  kall[[1L]] <- as.name("Rd2HTML")
  eval
} 