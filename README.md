## Wiki named entity annotator
### Using nix-shell
```
nix-shell shell.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20

cabal configure --enable-tests
cabal test
# or run tests in parallel 
cabal build test
./dist/build/test/test  +RTS -N -RTS
#Or run tests in REPL
cabal repl test

#Clean up
cabal clean
```

### Using nix-build
```
#Note that test is disabled in default.nix. Needs a test data dir to run it.
nix-build release.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20 -A wiki-ner
```

