{ pkgs ? import <nixpkgs> {}
, nlp-types ? <nlp-types>
}:

with pkgs;

let config1 =
      self: super: {
        "nlp-types" = self.callPackage (import nlp-types) {};
      };
    newHaskellPackages = haskellPackages.override {
                         overrides = config1;
                       };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              attoparsec
              cabal-install
              haskeline
              lens
              monad-loops
              optparse-applicative
              split
              text text-format
              p.nlp-types
            ]);
in stdenv.mkDerivation {
  name = "textview-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
