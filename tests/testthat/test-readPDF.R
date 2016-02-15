context("Reading PDFs")

tmpdir <- tempdir()
## tmpfile <- tempfile(fileext = ".zip", tmpdir = tmpdir)
## poppler.fail <- download.file("http://dl.dropbox.com/u/3291828/Poppler/poppler.0.22.0_win32.zip",
##                            tmpfile, quiet = TRUE)
## poppler.fail <- download.file("http://poppler.freedesktop.org/poppler-0.32.0.tar.xz", tmpfile, quiet = TRUE)

## poppler.fail <- download.file(
##     "ftp://ftp.gnome.org/Public/GNOME/binaries/win32/dependencies/poppler_0.12.0-1_win32.zip", tmpfile,
##      quiet = TRUE)
## unzip(tmpfile, exdir = tmpdir)

## curdir <- getwd()
exe.path <- tmpdir  # file.path(tmpdir, "bin", fsep = "\\")
## Sys.setenv(PATH = paste(Sys.getenv("PATH"), exe.path, sep = ":"))
poppler.fail <- !nzchar(Sys.which("pdfinfo"))
jss.fail <- download.file("https://www.jstatsoft.org/index.php/jss/article/view/v056i11/v56i11.pdf",
                          file.path(exe.path, "jss.pdf"), mode = "wb")
arxiv1.fail <- download.file("http://arxiv.org/pdf/math/0703791",
              destfile = file.path(exe.path, "FIZaop.pdf"), mode = "wb")
arxiv2.fail <- download.file("http://arxiv.org/pdf/math/0703858",
              destfile = file.path(exe.path, "PBHTaos.pdf"), mode = "wb")
biomet.fail <- download.file("http://biomet.oxfordjournals.org/content/83/4/715.full.pdf",
                          destfile = file.path(exe.path, "ADVb.pdf"), mode = "wb")

jstor.fail <- !file.copy(system.file("pdf", "jstor.pdf", package = "RefManageR"), exe.path)
## jstor.fail <- if (capabilities("libcurl"))
##                download.file("https://dl.dropboxusercontent.com/u/107936614/25050155.pdf",  # 25645718.pdf
## #                             destfile = "jstor.pdf", mode = "wb", method = "libcurl")
##            else
##                TRUE

test_that("Recognizes DOI", {
    skip_on_cran()
    if (poppler.fail || arxiv1.fail)
        skip("Couldn't download arxiv1")
    expect_message(ReadPDFs(file.path(exe.path, "FIZaop.pdf"), use.metadata = FALSE),
                   "Getting 1 BibTeX entries from CrossRef...")
})

test_that("Creates a BibEntry object", {
    skip_on_cran()
    if (poppler.fail || all(jstor.fail, biomet.fail, arxiv1.fail, arxiv2.fail, jss.fail))
        skip("Couldn't download Poppler or a single PDF")
    bib <- ReadPDFs(exe.path, progress = TRUE, use.crossref = TRUE)
    expect_is(bib, "BibEntry")
})

test_that("Add file field", {
    skip_on_cran()
    if (poppler.fail || all(jstor.fail, biomet.fail, arxiv1.fail, arxiv2.fail, jss.fail))
        skip("Couldn't download Poppler or a single PDF")
    bib <- ReadPDFs(exe.path, progress = TRUE, use.crossref = FALSE)
    expect_is(unlist(bib$file), "character")
})

test_that("Reading one PDF file name", {
    skip_on_cran()
    if (poppler.fail || jstor.fail)
        skip("Couldn't copy jstor.pdf")
    bib1 <- ReadPDFs(file.path(exe.path, "jstor.pdf"), use.metadata = FALSE)
    expect_is(bib1, "BibEntry")
    expect_equal(length(bib1), 1L)
})

test_that("Recognizes JSTOR", {
    skip_on_cran()
    if (poppler.fail || jstor.fail)
        skip("Couldn't download Poppler or JSTOR PDF")
    bib <- ReadPDFs(exe.path, progress = FALSE, use.crossref = FALSE)
    expect_equal(bib[author = "carrol"]$eprinttype, "jstor")
    expect_equal(bib[author = "carrol"]$url, "http://www.jstor.org/stable/25050155")
})

test_that("Recognizes arxiv", {
    skip_on_cran()
    if (poppler.fail || arxiv2.fail)
        skip("Couldn't download arxiv2 reference")
    bib <- ReadPDFs(exe.path, progress = FALSE, use.crossref = FALSE)
    expect_equal(bib[author = "paul"]$eprinttype, "arxiv")
    expect_equal(bib[author = "paul"]$url, "http://arxiv.org/abs/math/0703858v1")
})

test_that("Can parse arXiv date", {
    skip_on_cran()
    if (poppler.fail || arxiv2.fail)
        skip("Couldn't download arxiv2 reference")
    ## Entry misses year/date if use.metadata is FALSE
    bib <- ReadPDFs(exe.path, progress = FALSE, use.crossref = FALSE)
    expect_equal(bib[author = "paul"]$date, "2007-03-28")
})


test_that("Reading year and author", {
    skip_on_cran()
    if (poppler.fail || jss.fail)
        skip("Couldn't download JSS reference")
    bib <- ReadPDFs(exe.path, progress = FALSE, use.crossref = FALSE)
    expect_equal(unlist(bib[author = "luo"]$author$family), c("Luo", "Chen", "Su", "Chu"))
    expect_equal(bib[author = "luo"]$year, "2014")
})

test_that("Reading journal and title", {
    skip_on_cran()
    if (poppler.fail || biomet.fail)
        skip("Couldn't download Biometrika reference")
    bib <- ReadPDFs(exe.path, progress = FALSE, use.crossref = FALSE)
    expect_match(bib[year = "1996"]$journal, "Biometrika")
    expect_equal(bib[year = "1996"]$title, "The Multivariate Skew-normal Distribution")
    expect_equal(bib[year = "1996"]$bibtype, "Article")
})

test_that("use.metadata = FALSE", {
    skip_on_cran()
    if (poppler.fail || all(jstor.fail, biomet.fail, arxiv1.fail, arxiv2.fail, jss.fail))
        skip("Couldn't download Poppler or a single pdf")
    bib <- ReadPDFs(exe.path, use.metadata = FALSE, use.crossref = FALSE)
    expect_is(bib, "BibEntry")
    bib <- ReadPDFs(exe.path, use.metadata = FALSE, use.crossref = TRUE)
    expect_is(bib, "BibEntry")
})

# setwd(curdir)
unlink(tmpdir, force = TRUE)
