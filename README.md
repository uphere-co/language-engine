## Wiki named entity annotator
### Using nix-shell
```
# Set nlp-types in $NIX_PATH or add `--argstr nlp-types (nlp-types)` option.
nix-shell shell.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20


# Setup Cabal sandbox
cabal sandbox init
cabal install --only-dependencies

# Build & run
cabal configure --enable-tests --builddir=../dists/wiki-ner
cabal test --builddir=../dists/wiki-ner
# or run tests in parallel 
cabal build test --builddir=../dists/wiki-ner
# without --builddir:
./dist/build/test/test  +RTS -N -RTS
# with --builddir:
../dists/wiki-ner/build/test/test  +RTS -N -RTS

# Or run tests in REPL
cabal new-configure --enable-tests --builddir=../dists/wiki-ner
cabal new-repl test --builddir=../dists/wiki-ner

# Clean up
cabal clean
cabal sandbox delete

# Run test app in interactive mode.
cat yago/sample | runhaskell -i./src/ test/testApp.hs 
```

### Using nix-build
```
#Note that test is disabled in default.nix. Needs a test data dir to run it.
nix-build release.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20 -A wiki-ner
```

## Run experiments in REPL with YAGO data.

### Run experiments
```
# To run experiments in REPL
cabal new-repl testRun --builddir=../dists/wiki-ner
# To run the compiled experiments app
cabal build testRun --builddir=../dists/wiki-ner
```



### For profiling

build with profiling option
```
$ cabal install --enable-profiling
# Or, for testing in REPL
$ cabal configure --enable-tests --enable-profiling 
$ cabal new-repl testRun
-- Profiling each lines in REPL
> :set +s
```

default profiling (finding cost center)
```
$ .cabal-sandbox/bin/testApp wnTypes.1M  +RTS -p
```
Find `testApp.prof` file after run.


time dependent memory profiling
```
$ .cabal-sandbox/bin/testApp wnTypes.1M +RTS -p -h
$ hp2ps testApp.hp
$ ps2pdf testApp.ps testApp.pdf
```

### Running WikiEL with disambiguation.
#### Getting Wikipedia interlinks and WordNet synsets from YAGO
```
cabal build yago-bin
# using `yago-bin` with `interWikiLinks`
# Get sub-dataset for speedup
$ lbzcat yago/yago3_entire_tsv.bz2 | grep "<linksTo>" > wikilinks
$ time cat wikilinks | dist/build/yago-bin/yago-bin interlink > interlinks
$ lbzcat yago/yago3_entire_tsv.bz2 | grep "<wordnet_" > yago/wordnet
$ time grep subclassOf yago/wordnet | dist/build/yago-bin/yago-bin synset > wnTypes
$ time grep subclassOf yago/wordnet | dist/build/yago-bin/yago-bin taxonomy > taxonomies
real	0m3.048s

cp enwiki/wnTypes enwiki/synsets
cat enwiki/taxonomies >> enwiki/synsets
```

#### Getting noisy hub nodes to filter out
```
cat enwiki/wnTypes | grep person_ | awk -F '\t' '{print $1}' > persons
cat enwiki/wnTypes | grep organization_ | awk -F '\t' '{print $1}' > orgs

$ time cat enwiki/interlinks | grep -Ff persons > interlinks.persons
real	0m37.966s
$ time cat enwiki/interlinks | grep -Ff orgs > interlinks.orgs
real	0m42.248s
$ time cat enwiki/interlinks | sort -R > interlinks.ran
real	43m7.016s

$ time cat enwiki/interlinks | awk -F '\t' '{print $1 "\n" $2}' > nodes.weighted
real	0m49.166s
$ time cat nodes.weighted | shuf > nodes.weighted.ran
real	0m35.332s
$ time cat nodes.weighted.ran | grep -Fxf persons > nodes.weighted.ran.persons
real	1m49.687s
$ time cat nodes.weighted.ran | grep -Fxf orgs > nodes.weighted.ran.orgs
real	0m58.049s


# Generate filter.page and get filtered interlinks
$ cabal build node-filter
# Put `interlinks` and `nodes.weighted.ran` into the current directory
$ time ../dist/build/node-filter/node-filter interlinks nodes.weighted.ran > filter.page
real	41m12.292s
$ time awk -F"\t" 'NR == FNR { a[$1]; next } !($2 in a) {print}' filter.page interlinks > interlinks.filtered
real	0m47.716s
```

#### Prepare data and run the executable for entity linking
First, See "Wikidata entity annotation" section of `rnn++/README.md` to produce `wikidata.items` and `wikidata.all_entities` files. (Caution : as noted in the rnn++/README.md, must use wavewave's fork of nixpkgs; don't have clue why clang cannot find the standard header files, otherwise.)

Second, follow `ETL_process.md` for remaining steps.

#### Add test datset for entity linking
```
# Read wiki-ner/scripts/WikipediaETL.md
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20

#Follow a 'Run CoreNLP NER' section of `scripts/WikipediaETL.md` to run CoreNLP NER

# Read HCoreNLP/README.md
nix-shell shell.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20
./dist/build/annotate/annotate -f ../wiki-ner/data/bloomberg2.txt -p > a
# copy and paste the POS list in wiki-ner/test/Test/data/POS.hs

# Run WikiEL
# In wiki-ner repo,
cabal build wikiel
./dist/build/wikiel/wikiel
```

