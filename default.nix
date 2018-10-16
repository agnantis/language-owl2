{ mkDerivation, base, megaparsec, parser-combinators, stdenv }:
mkDerivation {
  pname = "owl-parser";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base megaparsec parser-combinators ];
  homepage = "github.com/agnantis/owl-parser";
  description = "OWL2 parser";
  license = stdenv.lib.licenses.bsd3;
}
