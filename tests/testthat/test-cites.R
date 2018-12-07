context("Cite functions")

## rm(list = ls(all=TRUE))
## unloadNamespace("RefManageR")
## library("RefManageR")
clear.cites <- function(){
    env <- getNamespace("RefManageR")
    unlockBinding(".cites", env)
    assign(".cites", new.env(), env)
    assign("indices", logical(0), get(".cites", envir = env))
    assign("labs", character(0), get(".cites", envir = env))
    assign("sty", "authoryear", get(".cites", envir = env))
}

clear.cites()
BibOptions(restore.defaults = TRUE)
old.opts <- BibOptions(check.entries = FALSE, style = "markdown",
                       bib.style = "numeric", cite.style = "numeric")
test_that("BibOptions returns changed options correctly", {
  expect_identical(old.opts, list(check.entries = "error", style = "text",
                                  bib.style = "numeric",
                                  cite.style = "authoryear"))
})

bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                           package = "RefManageR"), check = FALSE)

test_that("citation with markdown link", {
    expect_identical(Citet(bib, 11, .opts = list(longnamesfirst = FALSE)),
                     "<a name=cite-glashow></a>[Glashow [1]](#bib-glashow)")
})

test_that("citation with url link", {
    expect_identical(AutoCite(bib, "baez/online", before = "e.g., "),
                     paste0("<a name=cite-baezonline></a>[e.g., [2]",
                     "(https://arxiv.org/abs/math/0307200v3)]"))
})

test_that("citing twice uses correct number", {
    expect_identical(AutoCite(bib, "baez/online", before = "e.g., "),
                     "[e.g., [2](https://arxiv.org/abs/math/0307200v3)]")
})

test_that("citing entry with a shorthand field works", {
    expect_identical(AutoCite(bib, author = "kant"),
                     paste0("<a name=cite-kantkpv></a><a name=cite-kantku></a>",
                            "[[KpV](#bib-kantkpv); [KU](#bib-kantku)]"))
})

test_that("hyperlink = 'to.doc'", {
    expect_identical(Citet(bib, bibtype = "Report",
                           .opts = list(hyperlink = "to.doc", super = TRUE)),
     paste0("<a name=cite-chiu></a><a name=cite-padhye></a>Chiu and Chow^[[3]]",
            "(#bib-chiu); Padhye, Firoiu, and Towsley^[[4]](#bib-padhye)"))
})

test_that("citation with link to url", {
    expect_identical(AutoCite(bib, "markey"),
         paste0("<a name=cite-markey></a>[[5](http://tug.ctan.org/tex-archive/",
                "info/bibtex/tamethebeast/ttb_en.pdf)]"))
})

test_that("hyperlink = FALSE", {
    expect_identical(AutoCite(bib, location = "Uppsala",
                              .opts = list(hyperlink = FALSE)), "[6]")
})

test_that("NoCite doesn't print citation", {
    nc <- NoCite(bib = bib, title = "CTAN")
    expect_false(inherits(nc, "character"))
})

test_that("citation with several references", {
    expect_identical(AutoCite(bib, author = "Aristotle"),
                     paste0("<a name=cite-aristotleanima></a>",
                            "<a name=cite-aristotlephysics></a>",
                            "<a name=cite-aristotlepoetics></a>",
                            "<a name=cite-aristotlerhetoric></a>",
                            "[[8](#bib-aristotleanima); ",
                            "[9](#bib-aristotlephysics); [10]",
                            "(#bib-aristotlepoetics); ",
                            "[11](#bib-aristotlerhetoric)]"))
})

test_that("citation with several references with urls", {
  expect_identical(TextCite(bib, eprinttype = "arxiv"),
                   paste0("<a name=cite-baezarticle></a>",
                          "<a name=cite-itzhaki></a>",
                          "<a name=cite-wassenberg></a>",
                          "[Baez and Lauda [12]]",
                          "(https://arxiv.org/abs/math/0307200v3); ",
                          "[Baez and Lauda [2]]",
                          "(https://arxiv.org/abs/math/0307200v3); ",
                          "[Itzhaki [13]]",
                          "(https://arxiv.org/abs/hep-th/9603067); ",
                          "[Wassenberg and Sanders",
                          " [14]](https://arxiv.org/abs/1008.2849v1)"))
})

test_that("PrintBibliography", {
  test <- capture.output(PrintBibliography(bib,
                                         .opts = list(check.entries = FALSE)))
})

test_that("cite than switch to numeric citation", {
    BibOptions(cite.style = "authoryear", style = "text",
               check.entries = FALSE)
    Cite(bib, 12)
    BibOptions(cite.style = "numeric")
    expect_false(grepl("NA", Cite(bib, 12)))
    expect_true(grepl("\\[KU\\]", Citet(bib, author = "Kant")))
    expect_true(grepl("\\[2\\]", Citep(bib, 14)))
    expect_true(grepl("\\[1\\]", Citet(bib, 12)))
})

test_that("switching cite.style and citing reference again", {
    ## unloadNamespace("RefManageR")
    ## library(RefManageR)
    ## BibOptions(check.entries = FALSE, style = "markdown",
    ##            bib.style = "numeric", cite.style = "numeric")
    clear.cites()
    BibOptions(check.entries = FALSE) #, cite.style = "alphabetic")
    bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                               package = "RefManageR"), check = FALSE)
    AutoCite(bib, author = "kant")
    ## BibOptions(cite.style = "authoryear", check.entries = FALSE)
    ## Cite(bib, 12)
    BibOptions(cite.style = "numeric", style = "html")
    expect_false(grepl("NA", Cite(bib, 12)))
    expect_true(grepl("\\[KU\\]", Citet(bib, author = "Kant")))
})

bib2 <- ReadBib(system.file("Bib", "RJC.bib",
                            package = "RefManageR"))[seq_len(20)]

test_that("PrintBibliography when multiple BibEntry objects cited", {
    ##    rm(list=ls(all=TRUE))
    ## unloadNamespace("RefManageR")
    ## library(RefManageR)
    ## unlockBinding(".cites", getNamespace("RefManageR"))
    ## assign(".cites", new.env(), getNamespace("RefManageR"))
    clear.cites()
    ## BibOptions(check.entries = FALSE, style = "markdown",
    ##            bib.style = "numeric", cite.style = "numeric")
    ## BibOptions(check.entries = FALSE) #, cite.style = "alphabetic")
    bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                                package = "RefManageR"), check = FALSE)
    AutoCite(bib, author = "kant")

    if (!length(bib2))
        skip("Couldn't load RCJ.bib'")
    AutoCite(bib2, title = "binary longitudinal data")
    bibtext <- capture.output(PrintBibliography(bib2,
                                       .opts = list(cite.style = "numeric")))
    ## only one entry is printed (so that no blank lines separating entries)
    expect_false(any(grepl("^$", bibtext)))
})

test_that("PrintBibliography start and stop parameters", {
    clear.cites()

    if (!length(bib2))
        skip("Couldn't load RCJ.bib'")
    Citet(bib2, author = "Xun")
    AutoCite(bib2, title = "binary longitudinal data")
    bibtext <- capture.output(PrintBibliography(bib2, end = 1,
                                       .opts = list(cite.style = "numeric")))
    ## only one entry is printed (so that no blank lines separating entries)
    expect_false(any(grepl("^$", bibtext)))
    bibtext <- capture.output(PrintBibliography(bib2, start = 2,
                                       .opts = list(cite.style = "numeric")))
    expect_false(any(grepl("^$", bibtext)))
})

test_that("#54 DOI with parentheses works with markdown", {
   bibtext <- "@article{Mendenhall:1984,
      Author = {Mendenhall, William M. and Million, Rodney R. and Sharkey, Daniel E. and Cassisi, Nicholas J.},
      Date-Added = {2018-06-04 03:03:17 +0000},
      Date-Modified = {2018-06-04 03:03:17 +0000},
      Doi = {10.1016/0360-3016(84)90054-3},
      Isbn = {0360-3016},
      Journal = {International Journal of Radiation Oncology *Biology *Physics},
      Number = {3},
      Pages = {357--363},
      Title = {Stage T3 squamous cell carcinoma of the glottic larynx treated with surgery and/or radiation therapy},
      Volume = {10},
      Year = {1984},
      Bdsk-Url-1 = {https://doi.org/10.1016/0360-3016%5C%252884%5C%252990054-3}}"
   tfile <- tempfile()
   writeLines(bibtext, tfile)
   bib <- ReadBib(tfile)
   expect_length(grep("[(]https://doi.org/10.1016%2F0360-3016%2884%2990054-3[)]",
                      capture.output(print(bib, .opts = list(style = "markdown")))), 1L)
   unlink(tfile)
})

test_that("#58 thesis and report with missing type arg", {
    BibOptions(restore.defaults = TRUE)
    test <- bibentry("phdthesis", key = "test00", title = "My Wonderful Thesis", author = "Hugo Grunson",
                     school = "The U", year = "2018")

    Cite(test, "test00")

    out <- capture.output(PrintBibliography(test, .opts = list(bib.style = "authoryear")))
    expect_true(grepl("^Grunson, H", out))
})
