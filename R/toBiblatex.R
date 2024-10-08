#' Convert BibEntry objects to BibTeX or BibLaTeX
#'
#' toBiblatex converts a BibEntry object to character vectors with BibLaTeX markup.  toBibtex will convert a BibEntry object
#' to character vectors with BibTeX markup, converting some BibLaTeX fields and all entry types that are not supported
#' by BibTeX to ones that are supported.
#' @param object an object of class BibEntry to be converted
#' @param note.replace.field a character vector of BibLaTeX fields.  When converting an entry to BibTeX, the first field in the
#' entry that matches one specified in this vector will be added to the note field, \emph{if} the note field is not already
#' present
#' @param extra.fields character vector; fields that are not supported in standard BibTeX styles are by default dropped
#' in the result return by the toBibtex function.
#' Any fields specified in extra.fields will \emph{not} be dropped if present in an entry.
#' @param encoded.names.to.latex if \code{TRUE} (the default) then name list fields
#' such as \sQuote{author} and \sQuote{editor} will have non-ASCII characters
#' translated to LaTeX escape sequences.
#' @param ... ignored
#' @export
#' @return an object of class \dQuote{Bibtex} - character vectors where each element holds one line of a BibTeX or BibLaTeX file
#' @details toBiblatex converts the BibEntry object to a vector containing the corresponding BibLaTeX file, it ensures the name
#' list fields (e.g. author and editor) are formatted properly to be read by bibtex and biber and otherwise prints all fields
#' as is, thus it is similar to \code{\link{toBibtex}}.
#'
#' toBibtex will attempt to convert BibLaTeX entries to a format that can be read by bibtex.  Any fields not supported by
#' bibtex are dropped unless they are specified in \code{extra.fields}.  The fields below, if they are present, are converted
#' as described and added to a bibtex supported field, unless that field is already present.
#' \itemize{
#' \item date - The \code{date} field, if present will be truncated
#' to a year and added to the \code{year} field, if it is not already present. If a month is specified with the date, it will
#' be added to the \code{month} field.
#' \item journaltitle - Will be changed to journal, if it is not already present
#' \item location - Will be changed to address
#' \item institution - Converted to \code{school} for thesis entries
#' \item sortkey - Converted to \code{key}
#' \item maintitle - Converted to \code{series}
#' \item issuetitle - Converted to \code{booktitle}
#' \item eventtitle - Converted to \code{booktitle}
#' \item eprinttype - Converted to \code{archiveprefix} (for arXiv references)
#' \item eprintclass - Converted to \code{primaryclass} (for arXiv references)
#' }
#'
#' If no \code{note} field is present, the note.replace.field can be used to specified BibLaTeX fields that can be looked for
#' and added to the note field if they are present.
#'
#' BibLaTeX entry types that are not supported by bibtex are converted by toBibtex as follows
#' "mvbook" = "Book", "bookinbook" = "InBook", "suppbook" = "InBook",
#' \itemize{
#' \item MvBook,Collection,MvCollection,Reference,MvReference,Proceedings,MvProceedings,Periodical - to Book
#' \item BookInBook,SuppBook,InReference,SuppPeriodical - to InBook
#' \item report,patent - to TechReport
#' \item SuppCollection - to InCollection
#' \item thesis - to MastersThesis if \code{type = mathesis}, else to PhdThesis
#' \item \emph{rest} - to Misc
#' }
#' @seealso \code{\link{toBibtex}}, \code{\link{BibEntry}}, \code{\link{print.BibEntry}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}, Andy Bunn for code for
#' translating non-ASCII characters to LaTeX.
#' @importFrom tools parseLatex deparseLatex latexToUtf8
#' @keywords database IO utilities
#' @aliases toBibtex.BibEntry toBibtex
#' @examples
#' if (requireNamespace("bibtex")) {
#'     file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#'     bib <- suppressMessages(ReadBib(file.name))
#'     toBiblatex(bib[70:72])
#'     toBibtex(bib[70:72])
#' }
toBiblatex <- function(object, encoded.names.to.latex = TRUE, ...){
    format_bibentry1 <- function(object) {
      object <- unclass(object)[[1L]]
      rval <- paste0("@", attr(object, "bibtype"), "{", attr(object,
          "key"), ",")
      if (encoded.names.to.latex) {
        nl.ind <- which(names(object) %in% .BibEntryNameList)
        for (i in nl.ind)
          object[i] <- EncodedNameListToLaTeX(object[[i]])
      }
      rval <- c(rval, vapply(names(object), function(n) paste0("  ",
          n, " = {", object[[n]], "},"), ""), "}", "")
      return(rval)
    }
    if (length(object)) {
        object$.index <- NULL
        rval <- head(unlist(lapply(object, format_bibentry1)),
            -1L)
    }
    else rval <- character()
    class(rval) <- "Bibtex"
    rval
}

#' Wrapper for dplr::latexify that returns original
#' text if translation to LaTeX fails
#' @noRd
#' @seealso  \url{https://github.com/ropensci/RefManageR/issues/106}
EncodedNameListToLaTeX <- function(name.list, encoding = "UTF-8")
{
  formatted.text <- format_author(name.list)
  out <- latexify(formatted.text, encoding, doublebackslash = FALSE)
  if (grepl("^[{]?[?]", out))
    return(formatted.text)
  return(out)
}

#' Modified version of dplR::latexify
#' @importFrom stringi stri_trans_nfc stri_trans_nfd stri_unescape_unicode
#' @importFrom R.utils captureOutput
#' @author Andy Bunn
#' @noRd
latexify <- function(x, 
                     doublebackslash = FALSE, 
                     dashdash = TRUE, 
                     quotes = c("straight", "curved"), 
                     packages = c("fontenc", "textcomp")) 
{
  y <- as.character(x)
  encBytes <- Encoding(y) == "bytes"
  if (any(encBytes)) {
    y[encBytes] <- captureOutput(cat(y[encBytes], sep = "\n"))
  }
  y <- stri_trans_nfd(y)
  Letters <- paste0(c(LETTERS, letters), collapse = "")
  fontenc <- "fontenc" %in% packages
  textcomp <- "textcomp" %in% packages
  eurosym <- "eurosym" %in% packages
  straightQuotes <- match.arg(quotes) == "straight"
  y <- gsub("(?![[:space:]])[[:cntrl:]]", "", y, perl = TRUE)
  y <- gsub("[[:space:]]+", " ", y)
  substitutions <- list(c("\\\\^", "\\\\textasciicircum{}"),
                        c("~", "\\\\textasciitilde{}"), c("<", "\\\\textless{}"),
                        c(">", "\\\\textgreater{}"), c("\\\\|", "\\\\textbar{}"),
                        c("([#$%&_])", "\\\\\\1"), if (isTRUE(dashdash)) {
                          c("-", "\\\\mbox{-}")
                        }, if (textcomp && straightQuotes) {
                          c("'", "\\\\textquotesingle{}")
                        }, if (textcomp && straightQuotes) {
                          c("`", "\\\\textasciigrave{}")
                        }, c("\"", if (fontenc && straightQuotes) {
                          "\\\\textquotedbl{}"
                        } else {
                          "\\\\textquotedblright{}"
                        }), c("/", "\\\\slash{}"))
  substitutions <- substitutions[!vapply(substitutions, is.null, 
                                         logical(1))]
  substitutions <- c(substitutions, list(c("\\u0132", "\\\\IJ{}"),
                                         c("\\u0133", "\\\\ij{}"),
                                         c("\\u01f1", "DZ"), c("\\u01f2", "Dz"), 
                                         c("\\u01f3", "dz"), c("\\u01c4", "DZ\\u033c"),
                                         c("\\u01c5", "Dz\\u030c"),
                                         c("\\u01c6", "dz\\u030c"),
                                         c("\\u01c7", "LJ"), c("\\u01c8", "Lj"),
                                         c("\\u01c9", "lj"), c("\\u01ca", "NJ"),
                                         c("\\u01cb", "Nj"), c("\\u01cc", "nj"),
                                         c("\\ufb00", "ff"), c("\\ufb01", "fi"),
                                         c("\\ufb02", "fl"), c("\\ufb03", "ffi"),
                                         c("\\ufb04", "ffl"), c("\\ufb05", "\\u017ft"),
                                         c("\\ufb06", "st")))
  above <- list(diaeresis = c("\\u0308", "\""), acute = c("\\u0301", "'"),
                dotabove = c("\\u0307", "."), macron = c("\\u0304", "="),
  circumflex = c("\\u0302", "^"), grave = c("\\u0300", "`"),
  tilde = c("\\u0303", "~"), doubleacute = c("\\u030b", "H"),
  ringabove = c("\\u030a", "r"), breve = c("\\u0306", "u"),
  caron = c("\\u030c", "v"), invbreve = c("\\u0311", "newtie"))

  below <- list(macronbelow = c("\\u0331", "b"), cedilla = c("\\u0327", "c"),
                dotbelow = c("\\u0323", "d"), tie = c("\\u0361", "t"),
                ogonek = c("\\u0328", "k"))
  accents <- c(above, below)
  command <- paste0("\\\\[", Letters, "]+|\\\\.")
  combining <- paste0(vapply(accents, "[", character(1), 1), 
                      collapse = "")
  accPre <- paste0("(", command, "|.)({})?(?<![", combining, 
                   "])")
  accPost <- paste0("(?![", combining, "])")
  aboveInCode <- vapply(above, "[", character(1), 1)
  ijPattern <- paste0("([ij])", aboveInCode, accPost)
  otherPattern <- paste0(accPre, aboveInCode, accPost)
  aboveOutCode <- vapply(above, "[", character(1), 2)
  ijReplacement <- paste0("\\\\", aboveOutCode, "{\\\\\\1}")
  otherReplacement <- paste0("\\\\", aboveOutCode, "{\\1}")
  belowInCode <- vapply(below, "[", character(1), 1)
  belowPattern <- paste0(accPre, belowInCode, accPost)
  belowOutCode <- vapply(below, "[", character(1), 2)
  belowReplacement <- paste0("\\\\", belowOutCode, "{\\1}")
  circPre <- paste0("(", command, "({([^}]|\\\\})+})?|.)({})?")
  circPattern <- paste0(circPre, "\\u20dd", accPost)
  circReplacement <- "\\\\textcircled{\\1}"
  substitutions <- c(substitutions, lapply(lapply(mapply(list, 
                                                         list(as.name("c")), 
                                                         c(ijPattern, otherPattern, 
                                                           belowPattern, circPattern), 
                                                         c(ijReplacement, otherReplacement, 
                                                           belowReplacement, circReplacement), 
                                                         SIMPLIFY = FALSE), 
                                                  as.call), eval))
  substitutions <- c(substitutions, list(c("\\u00a1", "\\\\textexclamdown{}"),
                                         c("\\u00a3", "\\\\pounds{}"),
                                         c("\\u00a7", "\\\\S{}"),
                                         c("\\u00a9", "\\\\copyright{}"),
  c("\\u00aa", "\\\\textordfeminine{}"), c("\\u00ae", "\\\\textregistered{}"),
  c("\\u00b6", "\\\\P{}"), c("\\u00b7", "\\\\textperiodcentered{}"),
  c("\\u00ba", "\\\\textordmasculine{}"), c("\\u00bf", "\\\\textquestiondown{}"),
  c("\\u2013", "\\\\textendash{}"), c("\\u2014", "\\\\textemdash{}"),
  c("\\u2018", "\\\\textquoteleft{}"), c("\\u2019", "\\\\textquoteright{}"),
  c("\\u201c", "\\\\textquotedblleft{}"), c("\\u201d", "\\\\textquotedblright{}"),
  c("\\u2020", "\\\\dag{}"), c("\\u2021", "\\\\ddag{}"),
  c("\\u2022", "\\\\textbullet{}"), c("\\u2026", "\\\\dots{}"),
  c("\\u2122", "\\\\texttrademark{}"), c("\\u2423", "\\\\textvisiblespace{}"),
  c("\\u00c6", "\\\\AE{}"), c("\\u00e6", "\\\\ae{}"), c("\\u0152", "\\\\OE{}"),
  c("\\u0153", "\\\\oe{}"), c("\\u00d8", "\\\\O{}"), c("\\u00f8", "\\\\o{}"),
  c("\\u0141", "\\\\L{}"), c("\\u0142", "\\\\l{}"),
  c("\\u1e9e", "\\\\ifdefined\\\\XeTeXrevision\\\\iffontchar\\\\font\"1E9E\\\\symbol{\"1E9E}\\\\else\\\\SS\\\\fi\\\\else\\\\ifdefined\\\\directlua\\\\iffontchar\\\\font\"1E9E\\\\symbol{\"1E9E}\\\\else\\\\SS\\\\fi\\\\else\\\\SS\\\\fi\\\\fi{}"),
  c("\\u00df", "\\\\ss{}"), c("\\u017f", "\\\\ifdefined\\\\XeTeXrevision\\\\symbol{\"017F}\\\\else\\\\ifdefined\\\\directlua\\\\symbol{\"017F}\\\\else{\\\\fontencoding{TS1}\\\\selectfont s}\\\\fi\\\\fi{}")))
  substitutions <- c(substitutions, list(c("\\u00d0", "\\\\DH{}"), c("\\u00f0", "\\\\dh{}"),
                                         c("\\u0110", "\\\\DJ{}"), c("\\u0111", "\\\\dj{}"),
                                         c("\\u014a", "\\\\NG{}"),
                                         c("\\u014b", "\\\\ng{}"), c("\\u00de", "\\\\TH{}"),
                                         c("\\u00fe", "\\\\th{}"),
                                         c("\\u00ab", "\\\\guillemotleft{}"),
                                         c("\\u00bb", "\\\\guillemotright{}"),
                                         c("\\u201a", "\\\\quotesinglbase{}"),
                                         c("\\u201e", "\\\\quotedblbase{}"),
                                         c("\\u2039", "\\\\guilsinglleft{}"),
                                         c("\\u203a", "\\\\guilsinglright{}")))
  substitutions <- c(substitutions, list(c("\\u00ad", "\\\\-"), c("\\u200b", "\\\\hspace{0pt}"),
                                         c("\\u2217", "\\\\textasteriskcentered{}"),
                                         c("\\u2016", "\\\\textbardbl{}"),
                                         c("\\u25ef", "\\\\textbigcircle{}"),
                                         c("\\u2422", "\\\\textblank{}"),
                                         c("\\u00a6", "\\\\textbrokenbar{}"),
                                         c("\\u2052", "\\\\textdiscount{}"),
                                         c("\\u212e", "\\\\textestimated{}"),
                                         c("\\u203d", "\\\\textinterrobang{}"),
                                         c("\\u2e18", "\\\\textinterrobangdown{}"),
                                         c("\\u2116", "\\\\textnumero{}"),
                                         c("\\u25e6", "\\\\textopenbullet{}"),
                                         c("\\u2030", "\\\\textperthousand{}"),
                                         c("\\u2031", "\\\\textpertenthousand{}"),
                                         c("\\u211e", "\\\\textrecipe{}"),
                                         c("\\u203b", "\\\\textreferencemark{}"),
                                         c("\\u02f7", "\\\\texttildelow{}"),
                                         c("\\u2190", "\\\\textleftarrow{}"),
                                         c("\\u2191", "\\\\textuparrow{}"),
                                         c("\\u2192", "\\\\textrightarrow{}"),
                                         c("\\u2193", "\\\\textdownarrow{}"),
                                         c("\\u3008", "\\\\textlangle{}"),
                                         c("\\u3009", "\\\\textrangle{}"),
                                         c("\\u301a", "\\\\textlbrackdbl{}"),
                                         c("\\u301b", "\\\\textrbrackdbl{}"),
                                         c("\\u2045", "\\\\textlquill{}"),
                                         c("\\u2046", "\\\\textrquill{}"),
                                         c("\\u2117", "\\\\textcircledP{}"),
                                         c("\\u2120", "\\\\textservicemark{}"),
                                         c("\\u2103", "\\\\textcelsius{}"),
                                         c("\\u2127", "\\\\textmho{}"),
                                         c("\\u00b5", "\\\\textmu{}"),
                                         c("\\u03a9", "\\\\textohm{}"),
                                         c("\\u0e3f", "\\\\textbaht{}"),
                                         c("\\u00a2", "\\\\textcent{}"),
                                         c("\\u20a1", "\\\\textcolonmonetary{}"),
                                         c("\\u00a4", "\\\\textcurrency{}"),
                                         c("\\u20ab", "\\\\textdong{}"),
                                         c("\\u20ac", "\\\\texteuro{}"),
                                         c("\\u20b2", "\\\\textguarani{}"),
                                         c("\\u20a4", "\\\\textlira{}"),
                                         c("\\u20a6", "\\\\textnaira{}"),
                                         c("\\u20b1", "\\\\textpeso{}"),
                                         c("\\u20a9", "\\\\textwon{}"),
                                         c("\\u00a5", "\\\\textyen{}"),
                                         c("\\u02dd", "\\\\textacutedbl{}"),
                                         c("\\u00b4", "\\\\textasciiacute{}"),
                                         c("\\u00b8", "\\\\c{}"),
                                         c("\\u02d8", "\\\\textasciibreve{}"),
                                         c("\\u02c7", "\\\\textasciicaron{}"),
                                         c("\\u00a8", "\\\\textasciidieresis{}"),
                                         c("\\u00af", "\\\\textasciimacron{}"),
                                         c("\\u00b0", "\\\\textdegree{}"),
                                         c("\\u00f7", "\\\\textdiv{}"),
                                         c("\\u00bc", "\\\\textonequarter{}"),
                                         c("\\u00bd", "\\\\textonehalf{}"),
                                         c("\\u00be", "\\\\textthreequarters{}"),
                                         c("\\u00d7", "\\\\texttimes{}"),
                                         c("\\u00b1", "\\\\textpm{}"),
                                         c("\\u00b9", "\\\\textonesuperior{}"),
                                         c("\\u00b2", "\\\\texttwosuperior{}"),
                                         c("\\u00b3", "\\\\textthreesuperior{}"),
                                         c("\\u2044", "\\\\textfractionsolidus{}"),
                                         c("\\u221a", "\\\\textsurd{}"),
                                         c("\\u00ac", "\\\\textlnot{}"),
                                         c("\\u2212", "\\\\textminus{}")))
  tmp <- paste0("(\\\\[", Letters, "]+){}")
  substitutions <- c(substitutions, list(c(paste0(tmp, "(?=$|[[:digit:],.?!;:\\\\}+*/-])"),
                                           "\\1"), c(paste0(tmp, "(?! )"), "\\1 ")))
  for (subst in substitutions) {
    y <- gsub(stri_unescape_unicode(subst[1]), subst[2], y, perl = TRUE)
  }
  if (isTRUE(doublebackslash)) {
    y <- gsub("\\", "\\\\", y, fixed = TRUE)
  }
  stri_trans_nfc(y)
}

