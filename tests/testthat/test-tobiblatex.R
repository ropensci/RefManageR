bib = BibEntry(bibtype = "article", 
                 key = "shiotsuki2011kasai", 
                 title = "葛西賢太著,『現代瞑想論-変性意識がひらく世界-』",
                 author = "塩,亮子 and 葛西,賢太", 
                 journal = "宗教と社会",
                 volume = 17,
                 pages = "67--69",
                 year = 2011, 
                 publisher = "「宗教と社会」学会")

bib2 = BibEntry(bibtype = "article", 
               key = "shiotsuki2011kasai", 
               title = "Hello",
               author = "Jane Doe", 
               journal = "no",
               volume = 17,
               pages = "67--69",
               year = 2011, 
               publisher = "yes")

test_that("toBiblatex doesn't replace CJK character name lists with ???? (#106)",
{
  out <- toBiblatex(bib)
  expect_false(any(grepl("[?]", out)))
})

test_that("toBibtex doesn't replace CJK character name lists with ???? (#106)",
{
  out <- toBibtex(bib)
  expect_false(any(grepl("[?]", out)))
})


bib <- BibEntry(bibtype = "Article", key = "smith2014", title = "An Article Title",
                author = "Smith, Joé", journaltitle = "The Journal Title",
                date = "2014-02-06", pubstate = "forthcoming")
test_that("toBiblatex can toggle encoding names to LaTeX (#105)",
{
  out <- toBiblatex(bib, encoded.names.to.latex = TRUE)
  out.author = out[3]
  expect_true(grepl("{\\a'e}", out.author, fixed = TRUE))
  out <- toBiblatex(bib, encoded.names.to.latex = FALSE)
  out.author = out[3]
  expect_false(grepl("{\\a'e}", out.author, fixed = TRUE))
})

test_that("toBibtex can toggle encoding names to LaTeX (#105)",
{
  out <- toBibtex(bib, encoded.names.to.latex = TRUE)
  out.author = out[3]
  expect_true(grepl("{\\a'e}", out.author, fixed = TRUE))
  out <- toBibtex(bib, encoded.names.to.latex = FALSE)
  out.author = out[3]
  expect_false(grepl("{\\a'e}", out.author, fixed = TRUE))
})
