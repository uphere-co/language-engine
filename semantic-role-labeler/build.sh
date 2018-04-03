g++ -c --std=c++11 fasttext/wrapper.cc
cabal exec -- ghc fasttext/testFastText.hs wrapper.o
