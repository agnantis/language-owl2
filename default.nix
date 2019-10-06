{ mkDerivation, stdenv, base, containers, microlens-platform, megaparsec
, parser-combinators, prettyprinter, syb, template-haskell
,text, mtl, devDeps ? [ ] , devSystemDeps ? [ ]
}:

mkDerivation {
  pname = "language-owl2";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = devSystemDeps;
  libraryHaskellDepends = [
    base containers microlens-platform megaparsec parser-combinators
    prettyprinter syb template-haskell text mtl ] ++ devDeps;
  homepage = "github.com/agnantis/owl-parser";
  description = "OWL2 Parser and Pretty Printer";
  license = stdenv.lib.licenses.bsd3;
}
