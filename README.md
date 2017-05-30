## Wiki named entity annotator
```
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

