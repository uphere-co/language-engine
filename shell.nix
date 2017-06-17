{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, textview ? <textview>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };
  config2 =
    self: super: {
      "textview" = self.callPackage (import textview) {};
    };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            containers
            haskeline
            monad-loops
            p.textview
            cabal-install
          ]);

in

stdenv.mkDerivation {
  name = "multi-word-tagger-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

