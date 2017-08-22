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
cabal repl test --builddir=../dists/wiki-ner

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
cabal repl testRun --builddir=../dists/wiki-ner
# To run the compiled experiments app
cabal build testRun --builddir=../dists/wiki-ner
```
