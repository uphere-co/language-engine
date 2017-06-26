OntoNotes
=========

This library is a Haskell interface to OntoNotes corpus Release 5.0 [1] for
parsing and analysis.
The corpus has annotations of a large corpus (news, conversational telephone
speech, weblogs, usenet newsgroups, broadcast, talk shows) in three languages
(English, Chinese, and Arabic) with structural information (syntax and
predicate argument structure) and shallow semantics (word sense linked to an
ontology and coreference).

One can download OntoNotes Release 5.0 dataset from Linguistic Data Consortium
(LDC) [2].


Sample analysis
---------------
As of now only statistics application for WSJ section of OntoNotes exists.
To run,
```
$ nix-shell
$ runhaskell app/statistics.hs
```

-----

[1] OntoNotes project homepage: http://ontonotes.github.io/
[2] OntoNotes 5.0 Data from LDC: https://catalog.ldc.upenn.edu/ldc2013t19
