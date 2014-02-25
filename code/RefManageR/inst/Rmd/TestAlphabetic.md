Add Citations to an RMarkdown Document and Print Bibliography
========================================================


```r
library(RefManageR)
bib <- ReadBib(system.file("Bib", "biblatexExamples.bib", package = "RefManageR"), 
    check = FALSE)
BibOptions(check.entries = FALSE, style = "markdown", bib.style = "alphabetic", 
    cite.style = "alphabetic")
```

  This is an R Markdown document. This is an example of a citation in the text <a name=cite-herrmann></a>[Herrmann, Öfele, Schneider, et al. [Her+06]](#bib-herrmann). Now we cite in parentheses <a name=cite-baezonline></a>[e.g., [BL04b](http://arxiv.org/abs/math/0307200v3)].  You can change the default options in a setup chunk at the start of the document or at any other point using the <code>BibOptions</code> function or by specifying options as a list in the `.opts` argument to the cite functions.

These are reports <a name=cite-chiu></a><a name=cite-padhye></a>[Chiu and Chow [CC78]](#bib-chiu); [Padhye, Firoiu, and Towsley [PFT99]](#bib-padhye).  Their hyperlinks go to their entry in the bibliography.
The link for <a name=cite-markey></a>[[Mar05](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf)] will take you to the document in a new window; this is the default behaviour, if a link is available (see `?open.BibEntry`). The following citation has no hyperlink [Gee85].  You can also embed plots, for example: 
  

```r
plot(cars)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

I've added a reference to CTAN without citing it.  Look at all my Aristotle: <a name=cite-aristotleanima></a><a name=cite-aristotlephysics></a><a name=cite-aristotlepoetics></a><a name=cite-aristotlerhetoric></a>[[Ari07](#bib-aristotleanima); [Ari29](#bib-aristotlephysics); [Ari68](#bib-aristotlepoetics); [Ari77](#bib-aristotlerhetoric)].  


```r
plot(cars)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Some papers on the arXiv are <a name=cite-baezarticle></a><a name=cite-itzhaki></a><a name=cite-wassenberg></a>[Baez and Lauda [BL04a]](http://arxiv.org/abs/math/0307200v3); [Baez and Lauda [BL04b]](http://arxiv.org/abs/math/0307200v3); [Itzhaki [Itz96]](http://arxiv.org/abs/hep-th/9603067); [Wassenberg and Sanders [WS10]](http://arxiv.org/abs/1008.2849v1).

**References**

<a name=bib-aristotleanima></a>[[Ari07]](#cite-aristotleanima)
Aristotle. _De Anima_. Ed. by R. D. Hicks. Cambridge: Cambridge
University Press, 1907.

<a name=bib-aristotlephysics></a>[[Ari29]](#cite-aristotlephysics)
Aristotle. _Physics_. Trans.  by P. H. Wicksteed and F. M.
Cornford. New York: G. P. Putnam, 1929.

<a name=bib-aristotlepoetics></a>[[Ari68]](#cite-aristotlepoetics)
Aristotle. _Poetics_. Ed. by D. W. Lucas. Clarendon Aristotle.
Oxford: Clarendon Press, 1968.

<a
name=bib-aristotlerhetoric></a>[[Ari77]](#cite-aristotlerhetoric)
Aristotle. _The Rhetoric of Aristotle with a commentary by the
late Edward Meredith Cope_. Ed. by E. M. Cope. With a comment. by
E. M. Cope. Vol. 3. 3 vols. Cambridge University Press, 1877.

<a name=bib-baezarticle></a>[[BL04a]](#cite-baezarticle) J. C.
Baez and A. D. Lauda. "Higher-Dimensional Algebra V: 2-Groups".
Version 3. In: _Theory and Applications of Categories_ 12 (2004),
pp. 423-491. arXiv:
[math/0307200v3](http://arxiv.org/abs/math/0307200v3).

<a name=bib-baezonline></a>[[BL04b]](#cite-baezonline) J. C. Baez
and A. D. Lauda. _Higher-Dimensional Algebra V: 2-Groups_. Oct.
27, 2004. arXiv:
[math/0307200v3](http://arxiv.org/abs/math/0307200v3).

<a name=bib-chiu></a>[[CC78]](#cite-chiu) W. W. Chiu and W. M.
Chow. _A Hybrid Hierarchical Model of a Multiple Virtual Storage
(MVS) Operating System_. Research rep. RC-6947. IBM, 1978.

<a name=bib-ctan></a>[[CTAN06]](#cite-ctan) _CTAN. The
Comprehensive TeX Archive Network_. 2006. URL:
[http://www.ctan.org](http://www.ctan.org) (visited on
10/01/2006).

[Gee85] I. de Geer. "Earl, Saint, Bishop, Skald~- and Music. The
Orkney Earldom of the Twelfth Century. A Musicological Study". PhD
thesis. Uppsala: Uppsala Universitet, 1985.

<a name=bib-herrmann></a>[[Her+06]](#cite-herrmann) W. A.
Herrmann, K. Öfele, S. K. Schneider, et al. "A carbocyclic carbene
as an efficient catalyst ligand for C-C coupling reactions". In:
_Angew.~Chem. Int.~Ed._ 45.23 (2006), pp. 3859-3862.

<a name=bib-itzhaki></a>[[Itz96]](#cite-itzhaki) N. Itzhaki. _Some
remarks on 't Hooft's S-matrix for black holes_. Mar. 11, 1996.
arXiv: [hep-th/9603067](http://arxiv.org/abs/hep-th/9603067).

<a name=bib-markey></a>[[Mar05]](#cite-markey) N. Markey. _Tame
the BeaST. The B to X of BibTeX_. Oct. 16, 2005. URL:
[http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf)
(visited on 10/01/2006).

<a name=bib-padhye></a>[[PFT99]](#cite-padhye) J. Padhye, V.
Firoiu and D. Towsley. _A Stochastic Model of TCP Reno Congestion
Avoidance and Control_. Tech. rep. 99-02. Amherst, Mass.:
University of Massachusetts, 1999.

<a name=bib-wassenberg></a>[[WS10]](#cite-wassenberg) J.
Wassenberg and P. Sanders. _Faster Radix Sort via Virtual Memory
and Write-Combining_. Aug. 17, 2010. arXiv: [1008.2849v1
[cs.DS]](http://arxiv.org/abs/1008.2849v1).

