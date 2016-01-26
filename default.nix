{ pkgs ? import <nixpkgs> {} }:

let env = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint lens
  ]);
in pkgs.stdenv.mkDerivation {
  name = "lens-tutorial";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ env pkgs.ihaskell ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    export NIX_GHC_LIBDOCDIR="${env}/share/doc/x86*/"
  '';
}
