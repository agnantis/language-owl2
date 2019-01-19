let 
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project =
            let
              devDeps = with haskellPackagesOld; if pkgs.lib.inNixShell then [ hlint brittany ghcid doctest ] else [ ];
              devSystemDeps = if pkgs.lib.inNixShell then [ pkgs.entr ] else [ ];
            in 
              haskellPackagesNew.callPackage ./default.nix { inherit devDeps; inherit devSystemDeps; };
          iri =
            pkgs.haskell.lib.dontCheck haskellPackagesOld.iri;
          ptr =
            pkgs.haskell.lib.dontCheck haskellPackagesOld.ptr;
        };
      }; 
    };
  };
  pkgs = import <nixpkgs> {inherit config;} ;
in
  { project = pkgs.haskellPackages.project;
  }
