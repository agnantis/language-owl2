{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.OWL2.Functional.Pretty where

import           Control.Monad.State
import           Data.List                                ( intersperse, sort )
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Text.Prettyprint.Doc         hiding ( pretty, prettyList )
import qualified Data.Text.Prettyprint.Doc     as PP

import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types

-- | Wrapper typeclass of 'Pretty' typeclass for the Functional format
--
class PrettyF a where
  pretty :: a -> Doc ann
  default pretty :: PP.Pretty a => a -> Doc ann
  pretty = PP.pretty
  prettyList :: [a] -> Doc ann
  prettyList = PP.list . map pretty

instance PrettyF () where
  pretty = mempty

instance PrettyF a => PrettyF (Maybe a) where
  pretty Nothing = mempty
  pretty (Just x) = pretty x

instance PrettyF Char where
  pretty = PP.pretty
  prettyList = PP.prettyList

instance PrettyF a => PrettyF (NonEmpty a) where
    pretty = pretty . NE.toList

instance PrettyF a => PrettyF (AtLeast2List a) where
    pretty = pretty . toList

instance PrettyF Double where
  pretty = PP.pretty

instance PrettyF Integer where
  pretty = PP.pretty

instance PrettyF Int where
  pretty = PP.pretty

instance PrettyF T.Text where
  pretty = PP.pretty

instance {-# OVERLAPS #-} (PrettyF a) => PrettyF [a] where
  pretty [] = mempty
  pretty xs = hsep $ map pretty xs

instance PrettyF IRI where
  pretty (FullIRI i)            = enclose "<" ">" (PP.pretty i)
  pretty (AbbreviatedIRI pfx i) = pretty pfx <> ":" <> pretty i
  pretty (SimpleIRI i)          = ":" <> pretty i -- TODO: does it really require ':'?

instance PrettyF OntologyDocument where
  pretty (OntologyD pds o) = vsep (pretty <$> pds) <> emptyLine <> pretty o

instance PrettyF Ontology where
  pretty (Ontology mvi ims ans axms) = "Ontology" <> parens (nest 2 body)
   where
    body =  emptyLine
         <> pretty mvi
         <> emptyLine
         <> vsep (pretty <$> ims)
         <> emptyLine
         <> vsep (pretty <$> ans)
         <> emptyLine
         <> prettyAxioms axms

instance PrettyF OntologyVersionIRI where
  pretty (OntologyVersionIRI oIri mvIri) = pretty oIri <-> pretty mvIri

instance PrettyF PrefixDeclaration where
  pretty (PrefixD pfx i) = "Prefix" <> parens ((pretty pfx <> ":=") <> pretty i)

instance PrettyF ImportDeclaration where
  pretty (ImportD i) = "Import" <> parens (pretty i)

instance (PrettyF a) => PrettyF (Annotated a) where
  pretty (Annotated (annots, a)) = pretty annots <+> pretty a

instance PrettyF Annotation where
  pretty (Annotation prop val) = "Annotation" <> parens (pretty prop <+> pretty val) 

instance PrettyF AnnotationValue where
  pretty (NodeAT n)    = pretty n
  pretty (IriAT i)     = pretty i
  pretty (LiteralAT l) = pretty l 

instance PrettyF NodeID where
  pretty (NodeID s) = pretty s

instance PrettyF Literal where
  pretty (TypedLiteralC tl)      = pretty tl
  pretty (StringLiteralNoLang s) = dquotes $ pretty s
  pretty (StringLiteralLang st)  = pretty st
  pretty (IntegerLiteralC i)     = pretty i
  pretty (DecimalLiteralC d)     = pretty d
  pretty (FloatingLiteralC f)    = pretty f

instance PrettyF TypedLiteral where
  pretty (TypedL s dt) = dquotes (pretty s) <> "^^" <> pretty dt

instance PrettyF LiteralWithLang where
  pretty (LiteralWithLang s l) = dquotes (pretty s) <> "@" <> pretty l

instance PrettyF IntegerLiteral where
  pretty (IntegerL i) = pretty i

instance PrettyF DecimalLiteral where
  pretty (DecimalL d) = pretty d

instance PrettyF FloatPoint where
  pretty (FloatP d me) = pretty d <> pretty pme <> "f"
   where pme = ("e" ++) . show <$> me

instance PrettyF Datatype where
  pretty (Datatype dt) = pretty dt

instance PrettyF ObjectPropertyCharacteristic where
  pretty FUNCTIONAL         = "Functional"
  pretty INVERSE_FUNCTIONAL = "InverseFunctional"
  pretty REFLEXIVE          = "Reflexive"
  pretty IRREFLEXIVE        = "Irreflexive"
  pretty SYMMETRIC          = "Symmetric"
  pretty ASYMMETRIC         = "Asymmetric"
  pretty TRANSITIVE         = "Transitive"

instance PrettyF DataPropertyCharacteristics where
  pretty FUNCTIONAL_DPE = "Functional"

instance PrettyF ObjectPropertyChain where
  pretty (ObjectPropertyChain os) = "ObjectPropertyChain" <> parens (pretty os)

instance PrettyF ObjectOrDataPE where
  pretty (ObjectPE o) = pretty o
  pretty (DataPE d)   = pretty d

instance PrettyF Axiom where
  pretty (DeclarationAxiom ans e)                           = "Declaration" <> parens (pretty ans <-> pretty e)
  pretty (AnnotationAxiomDomain ans p i)                    = prettyAxiom "AnnotationPropertyDomain" (ans, p, i)
  pretty (AnnotationAxiomRange ans p i)                     = prettyAxiom "AnnotationPropertyRange" (ans, p, i)
  pretty (AnnotationAxiomSubProperty ans p1 p2)             = prettyAxiom "SubAnnotationPropertyOf" (ans, p1, p2)
  pretty (AnnotationAxiomAssertion ans i (Annotation t v))  = "AnnotationAssertion" <> parens (pretty ans <-> pretty t <+> pretty i <+> pretty v)
  pretty (DatatypeAxiomDefinition ans d r)                  = prettyAxiom "DatatypeDefinition" (ans, d, r)
  pretty (ObjectPropAxiomDomain ans o e)                    = prettyAxiom "ObjectPropertyDomain" (ans, o, e)
  pretty (ObjectPropAxiomRange ans o e)                     = prettyAxiom "ObjectPropertyRange" (ans, o, e)
  pretty (ObjectPropAxiomCharacteristics ans o c)           = pretty c <> "ObjectProperty" <> parens (pretty ans <-> pretty o)
  pretty (ObjectPropAxiomSubProperty ans o1 o2)             = prettyAxiom "SubObjectPropertyOf" (ans, o1,  o2)
  pretty (ObjectPropAxiomChainSubProperty ans c o)          = prettyAxiom "SubObjectPropertyOf" (ans, c, o)
  pretty (ObjectPropAxiomEquivalent ans o os)               = prettyAxiom "EquivalentObjectProperties" (ans, o, os)
  pretty (ObjectPropAxiomDisjoint  ans o os)                = prettyAxiom "DisjointObjectProperties" (ans, o, os)
  pretty (ObjectPropAxiomInverse ans o1 o2)                 = prettyAxiom "InverseObjectProperties" (ans, o1, o2)
  pretty (DataPropAxiomDomain ans d c)                      = prettyAxiom "DataPropertyDomain" (ans, d, c)
  pretty (DataPropAxiomRange ans d r)                       = prettyAxiom "DataPropertyRange" (ans, d, r)
  pretty (DataPropAxiomCharacteristics ans d c)             = pretty c <> "DataProperty" <> parens (pretty ans <-> pretty d)
  pretty (DataPropAxiomSubProperty ans d1 d2)               = prettyAxiom "SubDataPropertyOf" (ans, d1, d2)
  pretty (DataPropAxiomEquivalent ans d ds)                 = prettyAxiom "EquivalentDataProperties" (ans, d, ds)
  pretty (DataPropAxiomDisjoint ans d ds)                   = prettyAxiom "DisjointDataPropertiesDataProperties" (ans, d, ds)
  pretty (ClassAxiomSubClassOf ans e1 e2)                   = prettyAxiom "SubClassOf" (ans, e1, e2)
  pretty (ClassAxiomEquivalentClasses ans e es)             = prettyAxiom "EquivalentClasses" (ans, e, es)
  pretty (ClassAxiomDisjointClasses ans e es)               = prettyAxiom "DisjointClasses" (ans, e, es)
  pretty (ClassAxiomDisjointUnion ans i es)                 = prettyAxiom "DisjointUnion" (ans, i, es)
  pretty (ClassAxiomHasKey ans e od)                        = prettyAxiom "HasKey" (ans, e, od)
  pretty (AssertionAxiomSameIndividuals ans i is)           = prettyAxiom "SameIndividual" (ans, i, is)
  pretty (AssertionAxiomDifferentIndividuals ans i is)      = prettyAxiom "DifferentIndividuals" (ans, i, is)
  pretty (AssertionAxiomClass ans i e)                      = prettyAxiom "ClassAssertion" (ans, i, e)
  pretty (AssertionAxiomObjectProperty ans o i1 i2)         = prettyAxiom "ObjectPropertyAssertion" (ans, o, atLeast2List i1 i2 [])
  pretty (AssertionAxiomNegativeObjectProperty ans o i1 i2) = prettyAxiom "NegativeObjectPropertyAssertion" (ans, o, atLeast2List i1 i2 [])
  pretty (AssertionAxiomDataProperty ans d i l)             = "DataPropertyAssertion" <> parens (pretty ans <-> pretty d <+> pretty i <+> pretty l)
  pretty (AssertionAxiomNegativeDataProperty ans d i l)     = "NegativeDataPropertyAssertion" <> parens (pretty ans <-> pretty d <+> pretty i <+> pretty l)

prettyAxiom :: (PrettyF a, PrettyF b, PrettyF c) => Doc ann -> (a, b, c) -> Doc ann
prettyAxiom label (x,y,z) = label <> parens (pretty x <-> pretty y <+> pretty z)

prettyAxiomGroups :: [[Axiom]] -> Doc ann
prettyAxiomGroups xss = vsep . intersperse "" $ docs
 where
   docs = (\xs -> vsep (pretty <$> xs)) <$> xss

prettyAxioms :: [Axiom] -> Doc ann
prettyAxioms as =
  vsep (pretty <$> sort decls)
  <> mkTitle "Object Properties"
  <> prettyAxiomGroups (groupAxiomsOnIRI rst) 
   where
     predicate a = axiomType a == DeclarationAxiomType || axiomType a == AnnotationPropAxiomType
     (decls, rst) = runState (extractAxioms predicate) as

instance PrettyF ObjectPropertyExpression where
  pretty (OPE i)        = pretty i
  pretty (InverseOPE i) = "ObjectInverseOf" <> parens (pretty i)

instance PrettyF ClassExpression where
  pretty (CExpClass i)                       = pretty i
  pretty (CExpObjectIntersectionOf es)       = "ObjectIntersectionOf"   <> parens (pretty es)
  pretty (CExpObjectUnionOf es)              = "ObjectUnionOf"          <> parens (pretty es)
  pretty (CExpObjectComplementOf e)          = "ObjectComplementOf"     <> parens (pretty e)
  pretty (CExpObjectOneOf is)                = "ObjectOneOf"            <> parens (pretty is)
  pretty (CExpObjectSomeValuesFrom o c)      = "ObjectSomeValuesFrom"   <> parens (pretty o <+> pretty c)
  pretty (CExpObjectAllValuesFrom o c)       = "ObjectAllValuesFrom"    <> parens (pretty o <+> pretty c)
  pretty (CExpObjectHasValue o i)            = "ObjectHasValue"         <> parens (pretty o <+> pretty i)
  pretty (CExpObjectHasSelf o)               = "ObjectHasSelf"          <> parens (pretty o)
  pretty (CExpObjectMinCardinality i o mc)   = "ObjectMinCardinality"   <> parens (pretty i <+> pretty o <-> pretty mc)
  pretty (CExpObjectMaxCardinality i o mc)   = "ObjectMaxCardinality"   <> parens (pretty i <+> pretty o <-> pretty mc)
  pretty (CExpObjectExactCardinality i o mc) = "ObjectExactCardinality" <> parens (pretty i <+> pretty o <-> pretty mc)
  pretty (CExpDataSomeValuesFrom ds r)       = "DataSomeValuesFrom"     <> parens (pretty ds <+> pretty r)
  pretty (CExpDataAllValuesFrom  ds r)       = "DataAllValuesFrom"      <> parens (pretty ds <+> pretty r)
  pretty (CExpDataHasValue d l)              = "DataHasValue"           <> parens (pretty d <+> pretty l)
  pretty (CExpDataMinCardinality i d mr)     = "DataMinCardinality"     <> parens (pretty i <+> pretty d <-> pretty mr)
  pretty (CExpDataMaxCardinality i d mr)     = "DataMaxCardinality"     <> parens (pretty i <+> pretty d <-> pretty mr)
  pretty (CExpDataExactCardinality i d mr)   = "DataExactCardinality"   <> parens (pretty i <+> pretty d <-> pretty mr)

instance PrettyF DataRange where
  pretty (DatatypeDR dt)      = pretty dt
  pretty (IntersectionDR drs) = "DataIntersectionOf" <> parens (pretty drs)
  pretty (UnionDR drs)        = "DataUnionOf"        <> parens (pretty drs)
  pretty (ComplementDR dr)    = "DataComplementOf"   <> parens (pretty dr)
  pretty (OneOfDR ls)         = "DataOneOf"          <> parens (pretty ls)
  pretty (RestrictionDR dr)   = pretty dr

instance PrettyF DatatypeRestriction where
  pretty (DatatypeRestriction dt res) = "DatatypeRestriction" <> parens (pretty dt <+> pretty res) 

instance PrettyF RestrictionExp where
  pretty (RestrictionExp f l) = pretty f <+> pretty l

instance PrettyF Facet where
   pretty LENGTH_FACET     = "xsd:length"
   pretty MAX_LENGTH_FACET = "xsd:maxLength"
   pretty MIN_LENGTH_FACET = "xsd:minLength"
   pretty PATTERN_FACET    = "xsd:pattern"
   pretty LANG_RANGE_FACET = "xsd:langRange"
   pretty LE_FACET         = "xsd:maxInclusive"
   pretty L_FACET          = "xsd:maxExclusive"
   pretty GE_FACET         = "xsd:minInclusive"
   pretty G_FACET          = "xsd:minExclusive"

instance PrettyF TotalIRI where
  pretty (NamedIRI i)     = pretty i
  pretty (AnonymousIRI n) = pretty n

instance PrettyF Entity where
  pretty (EntityDatatype t)           = "Datatype"           <> parens (pretty t)
  pretty (EntityClass c)              = "Class"              <> parens (pretty c)
  pretty (EntityObjectProperty o)     = "ObjectProperty"     <> parens (pretty o)
  pretty (EntityDataProperty d)       = "DataProperty"       <> parens (pretty d)
  pretty (EntityAnnotationProperty p) = "AnnotationProperty" <> parens (pretty p)
  pretty (EntityIndividual i)         = "NamedIndividual"    <> parens (pretty i)


-- | Section separator
mkTitle :: Doc ann -> Doc ann
mkTitle title = vsep [ emptyLine
                     , "##############################"
                     , "#" <+> title
                     , "##############################"
                     , line
                     ]

-------------------------
--  Utility functions  --
-------------------------

-- | Prints an empty line
--
emptyLine :: Doc ann
emptyLine = line <> line

-- | Like @(<+>), with bettrn handling of empty representations, in order to avoid 
-- having many spaces (e.g. when you _canncatenate_ Docs where are empty
--
(<->) :: Doc ann -> Doc ann -> Doc ann
d1 <-> d2
  | null (show d1) = d2
  | null (show d2) = d1
  | otherwise = d1 <+> d2
  
