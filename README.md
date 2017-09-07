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
### Convert RDF dumps to Binary
```
# For YAGO
cabal build yago-bin
time ./dist/build/yago-bin/yago-bin
```

### Run experiments
```
# Get test dataset for experiments
-- uses `main1`
cabal build yago-bin
time cat yago/wordnet | dist/build/yago-bin/yago-bin > enwiki/wnTypes
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
$ lbzcat yago/yago3_entire_tsv.bz2 | grep "<linksTo>" > yago/wikilinks
$ time cat yago/wikilinks | runhaskell -i./src/ test/testApp.hs > enwiki/interlinks
real	60m40.005s

-- using `yago-bin` with `wordnetTypes`
cabal build yago-bin --builddir=../dists/wiki-ner
$ time grep subclassOf yago/wordnet | ../dists/wiki-ner/build/yago-bin/yago-bin > enwiki/wnTypes
-- using `yago-bin` with `wordnetTaxonomy`
cabal build yago-bin --builddir=../dists/wiki-ner
$ time grep subclassOf yago/wordnet | ../dists/wiki-ner/build/yago-bin/yago-bin > enwiki/taxonomies
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


# Generate filter.page by running Main.filterGen in test/testApp
$ time awk -F"\t" 'NR == FNR { a[$1]; next } !($2 in a) {print}' filter.page enwiki/interlinks > interlinks.filtered
real	0m47.716s
```

