{-# LANGUAGE OverloadedStrings #-}
module PrettyPrint where

import           Types
import           Data.Text.Prettyprint.Doc 
import qualified Data.List.NonEmpty as NE

instance Pretty IRI where
  pretty (FullIRI i) = enclose "<" ">" (pretty i)
  pretty (AbbreviatedIRI pfx i) = pretty pfx <> ":" <> pretty i
  pretty (SimpleIRI i) = pretty i

instance Pretty PrefixDeclaration where
  pretty (PrefixD pfx i) = "Prefix:" <+> (pretty pfx <> ":") <+> pretty i

instance Pretty ImportDeclaration where
  pretty (ImportD i) = "Import:" <+> pretty i

instance Pretty a => Pretty (AnnotatedList a) where
  pretty (AnnList nelst) = sep . punctuate comma $ xs
    where xs = (\(ma, a) -> mPretty ma <+> pretty a) <$> NE.toList nelst
          mPretty :: Maybe Annotations -> Doc ann
          mPretty Nothing  = mempty
          mPretty (Just z) = "Annotations:" <+> pretty z
          
instance Pretty Annotation where
  pretty (Annotation i t) = pretty i <+> pretty t

instance Pretty AnnotationTarget where
  pretty (NodeAT n)    = pretty n
  pretty (IriAT i)     = pretty i
  pretty (LiteralAT l) = pretty l 

instance Pretty NodeID where
  pretty (NodeID s) = pretty s

instance Pretty Literal where
  pretty (TypedLiteralC tl)      = pretty tl
  pretty (StringLiteralNoLang s) = dquotes $ pretty s
  pretty (StringLiteralLang st)  = pretty st
  pretty (IntegerLiteralC i)     = pretty i
  pretty (DecimalLiteralC d)     = pretty d
  pretty (FloatingLiteralC f)    = pretty f

instance Pretty TypedLiteral where
  pretty (TypedL s dt) = pretty s <> "^^" <> pretty dt

instance Pretty Datatype where
  pretty (IriDT i) = pretty i
  pretty IntegerDT = "integer"
  pretty DecimalDT = "decimal"
  pretty FloatDT   = "float"
  pretty StringDT  = "string"

instance Pretty LiteralWithLang where
  pretty (LiteralWithLang s l) = pretty s <> pretty l

instance Pretty IntegerLiteral where
  pretty (IntegerL i) = pretty i

instance Pretty DecimalLiteral where
  pretty (DecimalL d) = pretty d

instance Pretty FloatPoint where
  pretty (FloatP d me) = pretty d <> pretty pme <> "f"
   where
    pme = ("e" ++) . show <$> me
