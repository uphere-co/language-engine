PropBank: PropBank/NomBank parser and query engine
==================================================

This package provides a parser for PropBank and NomBank frames and
a query engine for those frames and their rolesets.

To test this, you can follow the instruction below.
```
$ nix-shell shell.nix --argstr uphere-nix-overlay (uphere-nix-overlay directory) 
$ cabal sandbox init
$ cabal install
$ .cabal-sandbox/bin/query -d (PropBank frame directory)
```
PropBank frames are obtained from https://github.com/propbank/propbank-frames
The frame directory is `/frames` in the repo.

Then, you will be prompted to the query interface. If you enter a lemma, then you will get
PropBank rolesets as follows.
```
% take
take.01 take, acquire, come to have, choose, bring with you from somewhere, internalize, ingest
take.02 tolerate
take.03 cause (to be)
take.04 understand to be
take.10 need, requiring
take.14 fixed phrase: take place
take.15 write-off, acknowledge a (financial) loss
take.16 take by surprise, taking by surprise (or some other manner)
take.17 become fond of
take.25 swindle/rob someone of something
take.32 take after; heredity, similarity
take.34 opinion, perspective, position
take.LV light verb
```
You can look into each role set by entering roleset id, which has `lemma.nn` form where `n` is a digit.
```
% take.01
id: take.01
name: take, acquire, come to have, choose, bring with you from somewhere, internalize, ingest
roles:
    - n: 0
      f: pag
      description: Taker
      vnrole:
          - vncls: 10.5
            vntheta: Agent
          - vncls: 11.3
            vntheta: Instrument
    - n: 1
      f: ppt
      description: thing taken
      vnrole:
          - vncls: 10.5
            vntheta: Theme
          - vncls: 11.3
            vntheta: Theme
    - n: 2
      f: dir
      description: taken FROM, SOURCE of thing taken
      vnrole:
          - vncls: 10.5
            vntheta: Source
    - n: 3
      f: gol
      description: destination
      vnrole:
          - vncls: 11.3
            vntheta: Destination
```



