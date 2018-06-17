{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
}:

with pkgs;

let
  hsconfig = import <uphere-nix-overlay/nix/haskell-modules/configuration-ghc-8.0.x.nix> { inherit pkgs; };

  haskellPackages1 = haskellPackages.override { overrides = hsconfig; };

  hsenv = haskellPackages1.ghcWithPackages (p: with p; [
            async
            binary-orphans
            monad-loops
            optparse-applicative
            split
            taggy-lens
            text
          ]);

in

stdenv.mkDerivation {
  name = "HFrameNet-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
