Semantic Role Labeler
=====================

This package is an implementaion of Semantic Role Labeler (SRL).
The SRL approach mainly follows Gildea and Palmer [1] (see Gildea and Jurafskyfor a complete
description [2]).

To install, you need to generate and install a Haskell binding to fastText via `fasttext/fastTextGen.hs`
using fficxx. The generated package name is also `fastText`. 
```
$ nix-shell shell.nix
$ cd fasttext
$ runhaskell fastTextGen.hs
$ cd ..; cabal sandbox init ; cabal sandbox add-source fasttext/fastText
$ cabal install fastText
```
Now you can install this package. You need to set LD_LIBRARY_PATH for fastText C++ library.
```
$ LD_LIBRARY_PATH=(C++ fastText directory)/lib cabal install
```
Currently, this package provides two programs.

## `feature`: extract features from a file of PropBank corpus
```
$ .cabal-sandbox/bin/feature -n (Penn treebank directory) -p (Propbank directory) -f (filename)
```
WSJ files are without lemma_type. For those files, you need to add `-o` option. 

The result looks like the following:
```
$ .cabal-sandbox/bin/feature -n /scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written -p /scratch/wavewave/MASC/Propbank/Propbank-orig/data/written -f wsj_0152 -o 
=================
PropBank

(S    
  (NP-S NNP  Dow NNP  Jones CC   & NNP  Co.) 
  (VP   VBD  said 
    (SBAR -NON 0 
      (S    
        (NP-S PRP  it) 
        (VP   VBD  extended 
          (NP   
            (NP   PRP$ its 
              (ADJP $    $ JJ   18-a-share -NON *U*) NN   offer) 
            (PP   IN   for 
              (NP   NNP  Telerate NNP  Inc. JJ   common NN   stock))) 
          (PP-T IN   until 
            (NP   CD   5 NN   p.m. NNP  EST NNP  Nov. CD   9)))))) 
  (.     .))
-----------------
CoreNLP

(ROOT 
  (S    
    (NP   NNP  Dow NNP  Jones CC   & NNP  Co.) 
    (VP   VBD  said 
      (SBAR 
        (S    
          (NP   PRP  it) 
          (VP   VBD  extended 
            (NP   PRP$ its 
              (ADJP $    $ JJ   18-a-share) NN   offer) 
            (PP   IN   for 
              (NP   NNP  Telerate NNP  Inc. JJ   common NN   stock)) 
            (PP   IN   until 
              (NP   CD   5 NN   p.m. NNP  EST)) 
            (NP-T NNP  Nov. CD   9))))) 
    (.     .)))
-----------------
Dow Jones & Co. said it extended its $ 18-a-share offer for Telerate Inc. common stock until 5 p.m. EST Nov. 9 .
-----------------
         4               say.01                  ARG0      (0,3)                   VBD↑VP↑S↓NP↓ Jones
         4               say.01                  ARG1     (5,21)                   VBD↑VP↓SBAR↓ extended

         6            extend.01                  ARG0      (5,5)                   VBD↑VP↑S↓NP↓ it
         6            extend.01                  ARG1     (7,10)                     VBD↑VP↓NP↓ offer
         6            extend.01            ARG2-until    (16,19)                     VBD↑VP↓PP↓ EST

```


## `training`: create a training set from the entire PropBank corpus
```
$ .cabal-sandbox/bin/training
```

[1] D. Gildea and M. Palmer, The Necessity of Parsing for Predicate Argument Recognition, ACL 2002
[2] D. Gildea and D. Jurafsky, Automatic Labeling of Semantic Roles, ACL 2002



Some testing programs
=======================

svm-train
---------

To run, you need a test file `real-sim`
which can be obtained from [here](http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/real-sim.bz2).

``` 
$ ghc svm-train.hs -lsvm -lgomp
$ ./svm-train
```

