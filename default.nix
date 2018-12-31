{ mkDerivation, base, iri, megaparsec_7_0_0, parser-combinators, stdenv, devDeps ? [ ], devSystemDeps ? [ ] }:

mkDerivation {
  pname = "owl-parser";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base iri megaparsec_7_0_0 parser-combinators ] ++ devDeps;
  buildDepends = devSystemDeps;
  homepage = "github.com/agnantis/owl-parser";
  description = "OWL2 parser";
  license = stdenv.lib.licenses.bsd3;
}
