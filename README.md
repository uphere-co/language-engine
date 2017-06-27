This library parses WordNet dict files and construct a query DB.

Sample application `query`:
```
$ nix-shell shell.nix
$ cabal sandbox init
$ cabal install
$ .cabal-sandbox/bin/query -d (wordnet dict directory)
```

Then, you will query synsets for a given lemma like this.
```
$ .cabal-sandbox/bin/query -d /scratch/wavewave/wordnet/WordNet-3.0/dict
% test
test.0
prove.3
try.1
try_out.0
examine.0
essay.1
screen.1
test.1
quiz.0
test.0
test.0
test.0
test.8
test.6
% lend
lend.1
impart.1
bestow.1
contribute.1
add.0
bring.2
lend.0
loan.0
lend.0
```

API Structure
getQueryLemma and getQueryConcept are API version of queryConcept and queryLemma,
which are just IO () functions.
DB of WordNet is loaded by function loadDB. Then, API is
```
wdb <- loadDB <filepath>
(concept :: Maybe ([LexItem],Text)) <- getQueryConcept <ili> <pos> wdb
(sense :: Maybe Int) <- getQuerySense <lex-word> <lex-id> wdb
```


WordNet lexicographer file parsing and serialization to binary format
---------------------------------------------------------------------

We can parse WordNet lexicographer files and transform data into a binary format
file so that we can load the data fast. To parse and encode
```
$ .cabal-sandbox/bin/serialize -d (wordnet lexicographer file dir) (serialized binary file)
```
You can test the file by adding `-t` option in the above command.
