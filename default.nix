{ mkDerivation, base, iri, megaparsec, parser-combinators, stdenv, devDeps ? [ ] }:

mkDerivation {
  pname = "owl-parser";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base iri megaparsec parser-combinators ] ++ devDeps;
  homepage = "github.com/agnantis/owl-parser";
  description = "OWL2 parser";
  license = stdenv.lib.licenses.bsd3;
}
