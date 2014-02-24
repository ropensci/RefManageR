Add Citations to an RMarkdown Document and Print Bibliography
========================================================



  This is an R Markdown document. This is an example of a citation in the text <a name=cite-loh></a>[Loh (1992)](#bib-loh). Now we cite in parentheses <a name=cite-baezonline></a>(e.g., [Baez and Lauda, 2004](http://arxiv.org/abs/math/0307200v3)).  You can change the default options in a setup chunk at the start of the document or at any other point using the <code>BibOptions</code> function or by specifying options as a list in the `.opts` argument to the cite functions.

These are reports <a name=cite-chiu></a><a name=cite-padhye></a>Chiu and Chow^[[87]](#bib-chiu); Padhye, Firoiu, and Towsley^[[88]](file://ftp://gaia.cs.umass.edu/pub/Padhey99-markov.ps).  Their hyperlinks go to their entry in the bibliography.
The link for <a name=cite-markey></a>[Markey (2005)](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf) will take you to the document in a new window; this is the default behaviour, if a link is available (see `?open.BibEntry`). The following citation has no hyperlink [89].  You can also embed plots, for example: 
  

```r
plot(cars)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

I've added a reference to CTAN without citing it.  Now I'm adding a reference from another bibliography (a second `BibEntry` object) <a name=cite-wang1999high></a>([Wang and Carroll, 1999](#bib-wang1999high)).  Look at all my Aristotle: <a name=cite-aristotleanima></a><a name=cite-aristotlephysics></a><a name=cite-aristotlepoetics></a><a name=cite-aristotlerhetoric></a>[Aristotle [21]](#bib-aristotleanima); [Aristotle [22]](#bib-aristotlephysics); [Aristotle [23]](#bib-aristotlepoetics); [Aristotle [24]](#bib-aristotlerhetoric).  


```r
plot(cars)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Some papers on the arXiv are <a name=cite-baezarticle></a><a name=cite-itzhaki></a><a name=cite-wassenberg></a>[Baez and Lauda (2004)](http://arxiv.org/abs/math/0307200v3); [Baez and Lauda (2004)](http://arxiv.org/abs/math/0307200v3); [Itzhaki (1996)](http://arxiv.org/abs/hep-th/9603067); [Wassenberg and Sanders (2010)](http://arxiv.org/abs/1008.2849v1).

**References**

<a name=bib-ctan></a>[CTAN](#cite-ctan) (2006). _CTAN. The
Comprehensive TeX Archive Network_. URL:
[http://www.ctan.org](http://www.ctan.org) (visited on Oct. 01,
2006).

<a
name=bib-aristotlerhetoric></a>[Aristotle](#cite-aristotlerhetoric)
(1877). _The Rhetoric of Aristotle with a commentary by the late
Edward Meredith Cope_. Ed. by E. M. Cope. With a comment. by E. M.
Cope. Vol. 3. 3 vols. Cambridge University Press.

<a name=bib-aristotleanima></a>[\-\-\-](#cite-aristotleanima)
(1907). _De Anima_. Ed. by R. D. Hicks. Cambridge: Cambridge
University Press.

<a name=bib-aristotlephysics></a>[\-\-\-](#cite-aristotlephysics)
(1929). _Physics_. Trans.  by P. H. Wicksteed and F. M. Cornford.
New York: G. P. Putnam.

<a name=bib-aristotlepoetics></a>[\-\-\-](#cite-aristotlepoetics)
(1968). _Poetics_. Ed. by D. W. Lucas. Clarendon Aristotle.
Oxford: Clarendon Press.

<a name=bib-baezarticle></a>[Baez, J. C. and A. D.
Lauda](#cite-baezarticle) (2004a). "Higher-Dimensional Algebra V:
2-Groups". Version 3. In: _Theory and Applications of Categories_
12, pp. 423-491. arXiv:
[math/0307200v3](http://arxiv.org/abs/math/0307200v3).

<a name=bib-baezonline></a>[\-\-\-](#cite-baezonline) (2004b).
_Higher-Dimensional Algebra V: 2-Groups_. arXiv:
[math/0307200v3](http://arxiv.org/abs/math/0307200v3).

<a name=bib-chiu></a>[Chiu, W. W. and W. M. Chow](#cite-chiu)
(1978). _A Hybrid Hierarchical Model of a Multiple Virtual Storage
(MVS) Operating System_. Research rep. RC-6947. IBM.

Geer, I. de (1985). "Earl, Saint, Bishop, Skald~- and Music. The
Orkney Earldom of the Twelfth Century. A Musicological Study". PhD
thesis. Uppsala.

<a name=bib-itzhaki></a>[Itzhaki, N.](#cite-itzhaki) (1996). _Some
remarks on 't Hooft's S-matrix for black holes_. arXiv:
[hep-th/9603067](http://arxiv.org/abs/hep-th/9603067).

<a name=bib-loh></a>[Loh, N. C.](#cite-loh) (1992).
"High-Resolution Micromachined Interferometric Accelerometer". MA
Thesis. Cambridge, Mass.

<a name=bib-markey></a>[Markey, N.](#cite-markey) (2005). _Tame
the BeaST. The B to X of BibTeX_. URL:
[http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf)
(visited on Oct. 01, 2006).

<a name=bib-padhye></a>[Padhye, J., V. Firoiu and D.
Towsley](#cite-padhye) (1999). _A Stochastic Model of TCP Reno
Congestion Avoidance and Control_. Tech. rep. 99-02. Amherst,
Mass.: University of Massachusetts.

<a name=bib-wassenberg></a>[Wassenberg, J. and P.
Sanders](#cite-wassenberg) (2010). _Faster Radix Sort via Virtual
Memory and Write-Combining_. arXiv: [1008.2849v1
[cs.DS]](http://arxiv.org/abs/1008.2849v1).


**More References**

<a name=bib-wang1999high></a>[[1]](#cite-wang1999high) S. Wang and
R. J. Carroll. "High-order accurate methods for retrospective
sampling problems". In: _Biometrika_ 86.4 (1999), pp. 881-897.

