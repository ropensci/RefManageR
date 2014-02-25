Add Citations to an RMarkdown Document and Print Bibliography
========================================================



  This is an R Markdown document. This is an example of a citation in the text <a name=cite-loh></a>[Loh (1992)](#bib-loh). Now we cite in parentheses <a name=cite-baezonline></a>(e.g., [Baez and Lauda, 2004b](http://arxiv.org/abs/math/0307200v3)).  Notice the useful 'b' beside the year in the citation.  You can change the default options in a setup chunk at the start of the document or at any other point using the <code>BibOptions</code> function or by specifying options as a list in the `.opts` argument to the cite functions.  In this example we mix `"authoyear"` citation style with `"numeric"` bibliography style.

Note that I do not only have to cite by key, and may use all the features of the `SearchBib` function to index into the BibEntry object.  Here are all the entries of type `Report` in my bibliography <a name=cite-chiu></a><a name=cite-padhye></a>[Chiu and Chow (1978)](#bib-chiu); [Padhye, Firoiu, and Towsley (1999)](#bib-padhye).  The hyperlinks will take you to their entry in the bibliography.  The link for <a name=cite-markey></a>[Markey (2005)](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf) will open the document in a new window; this is the default behaviour, if a link is available (see `?open.BibEntry`). The following citation has no hyperlink (de
Geer, 1985).  You can also embed plots, to make the page longer: 
  

```r
plot(cars)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

I've added a reference to CTAN without citing it using the `NoCite` function.  Now I'm adding a reference from another bibliography (a second `BibEntry` object) <a name=cite-serban2013multilevel></a>([Serban, Staicu, and Carroll, 2013](#bib-serban2013multilevel)).  Look at all my Aristotle: <a name=cite-aristotleanima></a><a name=cite-aristotlephysics></a><a name=cite-aristotlepoetics></a><a name=cite-aristotlerhetoric></a>[Aristotle (1907)](#bib-aristotleanima); [Aristotle (1929)](#bib-aristotlephysics); [Aristotle (1968)](#bib-aristotlepoetics); [Aristotle (1877)](#bib-aristotlerhetoric).  


```r
plot(cars)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Some papers on the arXiv are <a name=cite-baezarticle></a><a name=cite-itzhaki></a><a name=cite-wassenberg></a>[Baez and Lauda (2004a)](http://arxiv.org/abs/math/0307200v3); [Baez and Lauda (2004b)](http://arxiv.org/abs/math/0307200v3); [Itzhaki (1996)](http://arxiv.org/abs/hep-th/9603067); [Wassenberg and Sanders (2010)](http://arxiv.org/abs/1008.2849v1).

**References**

<a name=bib-aristotlerhetoric></a>[[1]](#cite-aristotlerhetoric)
Aristotle. _The Rhetoric of Aristotle with a commentary by the
late Edward Meredith Cope_. Ed. by E. M. Cope. With a comment. by
E. M. Cope. Vol. 3. 3 vols. Cambridge University Press, 1877.

<a name=bib-aristotleanima></a>[[2]](#cite-aristotleanima)
Aristotle. _De Anima_. Ed. by R. D. Hicks. Cambridge: Cambridge
University Press, 1907.

<a name=bib-aristotlephysics></a>[[3]](#cite-aristotlephysics)
Aristotle. _Physics_. Trans.  by P. H. Wicksteed and F. M.
Cornford. New York: G. P. Putnam, 1929.

<a name=bib-aristotlepoetics></a>[[4]](#cite-aristotlepoetics)
Aristotle. _Poetics_. Ed. by D. W. Lucas. Clarendon Aristotle.
Oxford: Clarendon Press, 1968.

<a name=bib-chiu></a>[[5]](#cite-chiu) W. W. Chiu and W. M. Chow.
_A Hybrid Hierarchical Model of a Multiple Virtual Storage (MVS)
Operating System_. Research rep. RC-6947. IBM, 1978.

[6] I. de Geer. "Earl, Saint, Bishop, Skald~- and Music. The
Orkney Earldom of the Twelfth Century. A Musicological Study". PhD
thesis. Uppsala: Uppsala Universitet, 1985.

<a name=bib-loh></a>[[7]](#cite-loh) N. C. Loh. "High-Resolution
Micromachined Interferometric Accelerometer". MA Thesis.
Cambridge, Mass.: Massachusetts Institute of Technology, 1992.

<a name=bib-itzhaki></a>[[8]](#cite-itzhaki) N. Itzhaki. _Some
remarks on 't Hooft's S-matrix for black holes_. Mar. 11, 1996.
arXiv: [hep-th/9603067](http://arxiv.org/abs/hep-th/9603067).

<a name=bib-padhye></a>[[9]](#cite-padhye) J. Padhye, V. Firoiu
and D. Towsley. _A Stochastic Model of TCP Reno Congestion
Avoidance and Control_. Tech. rep. 99-02. Amherst, Mass.:
University of Massachusetts, 1999.

<a name=bib-baezarticle></a>[[10]](#cite-baezarticle) J. C. Baez
and A. D. Lauda. "Higher-Dimensional Algebra V: 2-Groups". Version
3. In: _Theory and Applications of Categories_ 12 (2004), pp.
423-491. arXiv:
[math/0307200v3](http://arxiv.org/abs/math/0307200v3).

<a name=bib-baezonline></a>[[11]](#cite-baezonline) J. C. Baez and
A. D. Lauda. _Higher-Dimensional Algebra V: 2-Groups_. Oct. 27,
2004. arXiv:
[math/0307200v3](http://arxiv.org/abs/math/0307200v3).

<a name=bib-markey></a>[[12]](#cite-markey) N. Markey. _Tame the
BeaST. The B to X of BibTeX_. Oct. 16, 2005. URL:
[http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf)
(visited on 10/01/2006).

<a name=bib-ctan></a>[[13]](#cite-ctan) _CTAN. The Comprehensive
TeX Archive Network_. 2006. URL:
[http://www.ctan.org](http://www.ctan.org) (visited on
10/01/2006).

<a name=bib-wassenberg></a>[[14]](#cite-wassenberg) J. Wassenberg
and P. Sanders. _Faster Radix Sort via Virtual Memory and
Write-Combining_. Aug. 17, 2010. arXiv: [1008.2849v1
[cs.DS]](http://arxiv.org/abs/1008.2849v1).


**More References**

<a
name=bib-serban2013multilevel></a>[[1]](#cite-serban2013multilevel)
N. Serban, A. M. Staicu and R. J. Carroll. "Multilevel
Cross-Dependent Binary Longitudinal Data". In: _Biometrics_ 69.4
(2013), pp. 903-913.

