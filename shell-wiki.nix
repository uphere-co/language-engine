{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
                aeson cabal-install
            ]);

in stdenv.mkDerivation {
     name = "shell-wiki";
     buildInputs =  (with python35Packages;
                 [ ipython
                   jupyter
                   pandas numpy
                   beautifulsoup4
                 ]) ++
                 [ hsenv jdk 
                   pigz lbzip2 p7zip
                   jq xidel
                 ] ;
     shellHook = ''
         EDITOR=vim
         PS1="\n\[\033[0;34m\][\u@\h.devel:\w]\$\[\033[0m\] "
         MODEL=/data/groups/uphere/parsers/corenlp/
         CORENLP=/data/groups/uphere/parsers/corenlp/
         PARSER=/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09/
         CLASSPATH=$CORENLP/stanford-corenlp.Oct2016.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$CORENLP/stanford-ner.jar;
     '';
   }
