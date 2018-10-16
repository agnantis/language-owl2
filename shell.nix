{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, megaparsec, parser-combinators, stdenv, hlint, brittany, ghcid, doctest
      }:
      mkDerivation {
        pname = "owl-parser";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base megaparsec parser-combinators hlint brittany ghcid doctest];
        homepage = "github.com/agnantis/owl-parser";
        description = "OWL2 parser";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
