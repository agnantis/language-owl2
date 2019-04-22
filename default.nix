{ mkDerivation, base, containers, iri, microlens-platform, megaparsec_7_0_0, parser-combinators, prettyprinter, stdenv, text, mtl, devDeps ? [ ], devSystemDeps ? [ ] }:

mkDerivation {
  pname = "language-owl2";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers iri microlens-platform megaparsec_7_0_0 parser-combinators prettyprinter text mtl ] ++ devDeps;
  buildDepends = devSystemDeps;
  homepage = "github.com/agnantis/owl-parser";
  description = "OWL2 Parser and Pretty Printer";
  license = stdenv.lib.licenses.bsd3;
}
