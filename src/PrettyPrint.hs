{-# LANGUAGE OverloadedStrings #-}
module PrettyPrint where

import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                               ( fromMaybe )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Types

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
    where xs = (\(ma, a) -> mPretty ma <> pretty a) <$> NE.toList nelst
          mPretty :: Maybe Annotations -> Doc ann
          mPretty Nothing  = mempty
          mPretty m = "Annotations:" <+> align (prettyM m)
          
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
    where pme = ("e" ++) . show <$> me

instance Pretty OntologyVersionIRI where
  pretty (OntologyVersionIRI oIri mvIri) = pretty oIri <> prettyM' mvIri

instance Pretty a => Pretty (WithNegation a) where
  pretty (Positive a) = pretty a
  pretty (Negative a) = "not" <+> pretty a

instance Pretty a => Pretty (WithInversion a) where
  pretty (Plain a) = pretty a
  pretty (Inverse a) = "inverse" <+> pretty a

instance Pretty Primary where
  pretty (PrimaryR r) = pretty r 
  pretty (PrimaryA a) = pretty a

instance Pretty Atomic where
  pretty (AtomicClass i) = pretty i
  pretty (AtomicIndividuals is) = braces . pretty . NE.toList $ is 
  pretty (AtomicDescription d) = parens . pretty $ d 

instance Pretty Individual where
  pretty (IRIIndividual i) = pretty i
  pretty (NodeIndividual n) = pretty n

instance Pretty Restriction where
  pretty (OPRestriction o) = pretty o
  pretty (DPRestriction d) = pretty d

instance Pretty ObjectPropertyRestriction where
  pretty (OPR e rt) = pretty e <+> pretty rt

instance Pretty DataPropertyRestriction where
  pretty (DPR e rt) = pretty e <+> pretty rt

instance Pretty ObjectPropertyRestrictionType where
  pretty SelfOPR           = "Self"
  pretty (SomeOPR p)       = "some"    <+> pretty p
  pretty (OnlyOPR p)       = "only"    <+> pretty p
  pretty (MinOPR i mp)     = "min"     <+> pretty i <> prettyM' mp 
  pretty (MaxOPR i mp)     = "max"     <+> pretty i <> prettyM' mp 
  pretty (ExactlyOPR i mp) = "exactly" <+> pretty i <> prettyM' mp 

instance Pretty DataPropertyRestrictionType where
  pretty (SomeDPR p)       = "some"    <+> pretty p
  pretty (OnlyDPR p)       = "only"    <+> pretty p
  pretty (ValueDPR l)      = "value"   <+> pretty l
  pretty (MinDPR i mp)     = "min"     <+> pretty i <> prettyM' mp 
  pretty (MaxDPR i mp)     = "max"     <+> pretty i <> prettyM' mp 
  pretty (ExactlyDPR i mp) = "exactly" <+> pretty i <> prettyM' mp 

instance Pretty DataAtomic where
  pretty (DatatypeDA d) = pretty d
  pretty (LiteralListDA ls) = braces $ join "," (NE.toList ls)
  pretty (DatatypeRestrictionDA r) = pretty r
  pretty (DataRangeDA r) = parens (pretty r)

instance Pretty DataRange where
  pretty (DataRange dcs) = join "or" (NE.toList dcs)

instance Pretty DataConjunction where
  pretty (DataConjunction dps) = join "and" (NE.toList dps)

instance Pretty DatatypeRestriction where
  pretty (DatatypeRestriction d re) = pretty d <+> brackets (join "," (NE.toList  re))

instance Pretty RestrictionExp where
  pretty (RestrictionExp f l) = pretty f <+> pretty l

instance Pretty Facet where
  pretty LENGTH_FACET     = "length"
  pretty MIN_LENGTH_FACET = "minLength"
  pretty MAX_LENGTH_FACET = "maxLength"
  pretty PATTERN_FACET    = "pattern"
  pretty LANG_RANGE_FACET = "langRange"
  pretty LE_FACET         = "<="
  pretty L_FACET          = "<"
  pretty GE_FACET         = ">="
  pretty G_FACET          = ">"

instance Pretty Conjunction where
  pretty (ClassConj i rs) = pretty i <+> "that" <+> join "and" (NE.toList rs)
  pretty (PrimConj ps)    = join "and" (NE.toList ps)

instance Pretty DatatypeFrame where
  pretty (DatatypeF dt ma mdr) = "DatatypFrame:" <+> pretty dt <+> pma <> pme
    where pma = if null ma then mempty else "Annotations:" <+> sep (pretty <$> ma)
          pme = prependM "EquivalentTo: " mdr

instance Pretty AnnotDataRange where
  pretty (AnnotDataRange a dr) = pretty a <+> pretty dr

instance Pretty ClassFrame where
  pretty (ClassF i ces) = "Class:" <+> pretty i <+> sep (pretty <$> ces)

instance Pretty ClassElement where
  pretty (AnnotationCE as)          = "Annotations:"     <+> pretty as
  pretty (SubClassOfCE ds)          = "SubClassOf:"      <+> pretty ds
  pretty (EquivalentToCE ds)        = "EquivalentTo:"    <+> pretty ds
  pretty (DisjointToCE ds)          = "DisjointWith:"    <+> pretty ds
  pretty (DisjointUnionOfCE mas ds) = "DisjointUnionOf:" <+> prettyM mas <> pretty (toList ds) 
  pretty (HasKeyCE mas od)          = "HasKey:"          <+> prettyM mas <> pretty (NE.toList od)

instance Pretty ObjectOrDataPE where
  pretty (ObjectPE ope) = pretty ope
  pretty (DataPE dpe)   = pretty dpe

instance Pretty ObjectPropertyFrame where
  pretty (ObjectPropertyF i ops) = "ObjectProperty:" <+> pretty i <+> sep (pretty <$> ops)

instance Pretty ObjectPropertyElement where
  pretty (AnnotationOPE a)           = "Annotations:"      <+> pretty a
  pretty (DomainOPE ds)              = "Domain:"           <+> pretty ds
  pretty (RangeOPE ds)               = "Range:"            <+> pretty ds
  pretty (CharacteristicsOPE ops)    = "Characteristics:"  <+> pretty ops
  pretty (SubPropertyOfOPE ops)      = "SubPropertyOf:"    <+> pretty ops
  pretty (EquivalentToOPE ops)       = "EquivalentTo:"     <+> pretty ops
  pretty (DisjointWithOPE ops)       = "DisjointWith:"     <+> pretty ops
  pretty (InverseOfOPE ops)          = "InverseOf:"        <+> pretty ops
  pretty (SubPropertyChainOPE a ops) = "SubPropertyChain:" <+> pretty a <+> join "o" (toList ops)

instance Pretty ObjectPropertyCharacteristics where
  pretty FUNCTIONAL         = "Functional"
  pretty INVERSE_FUNCTIONAL = "InverseFunctional"
  pretty REFLEXIVE          = "Reflexive"
  pretty IRREFLEXIVE        = "Irreflexive"
  pretty SYMMETRIC          = "Symmetric"
  pretty ASYMMETRIC         = "Asymmetric"
  pretty TRANSITIVE         = "Transitive"


instance Pretty DataPropertyFrame where
  pretty (DataPropertyF i dps) = "DataProperty:" <+> pretty i <+> sep (pretty <$> dps)

instance Pretty DataPropertyElement where
  pretty (AnnotationDPE a)        = "Annotations:"      <+> pretty a
  pretty (DomainDPE ds)           = "Domain:"           <+> pretty ds
  pretty (RangeDPE ds)            = "Range:"            <+> pretty ds
  pretty (CharacteristicsDPE dps) = "Characteristics:"  <+> pretty dps
  pretty (SubPropertyOfDPE dps)   = "SubPropertyOf:"    <+> pretty dps
  pretty (EquivalentToDPE dps)    = "EquivalentTo:"     <+> pretty dps
  pretty (DisjointWithDPE dps)    = "DisjointWith:"     <+> pretty dps

instance Pretty DataPropertyCharacteristics where
  pretty FUNCTIONAL_DPE = "Functional"

instance Pretty AnnotationPropertyFrame where
  pretty (AnnotationPropertyF i aps) = "AnnotationProperty:" <+> pretty i <+> sep (pretty <$> aps)

instance Pretty AnnotationPropertyElement where
  pretty (AnnotationAPE a)        = "Annotations:"      <+> pretty a
  pretty (DomainAPE as)           = "Domain:"           <+> pretty as
  pretty (RangeAPE as)            = "Range:"            <+> pretty as
  pretty (SubPropertyOfAPE aps)   = "SubPropertyOf:"    <+> pretty aps

instance Pretty IndividualFrame where
  pretty (IndividualF i oes) = "Individual:" <+> pretty i <+> sep (pretty <$> oes)

instance Pretty IndividualElement where
  pretty (AnnotationIE a)     = "Annotations:"  <+> pretty a
  pretty (TypeIE ds)          = "Types:"        <+> pretty ds
  pretty (FactIE af)          = "Facts:"        <+> pretty af
  pretty (SameAsIE ai)        = "SameAs:"       <+> pretty ai
  pretty (DifferentFromIE ai) = "DifferentFrom" <+> pretty ai

instance Pretty FactElement where
  pretty (ObjectPropertyFE opf) = pretty opf 
  pretty (DataPropertyFE dpf)   = pretty dpf

instance Pretty ObjectPropertyFact where
  pretty (ObjectPropertyFact i ind) = pretty i <+> pretty ind

instance Pretty DataPropertyFact where
  pretty (DataPropertyFact i l) = pretty i <+> pretty l

instance Pretty Misc where
  pretty (EquivalentClasses as ds)           = "EquivalentClasses:"    <+> pretty as <+> pretty ds
  pretty (DisjointClasses as ds)             = "DisjointClasses:"      <+> pretty as <+> pretty ds
  pretty (EquivalentObjectProperties as ope) = "EquivalentProperties:" <+> pretty as <+> pretty ope
  pretty (DisjointObjectProperties as ope)   = "DisjointProperties:"   <+> pretty as <+> pretty ope
  pretty (EquivalentDataProperties as dpe)   = "EquivalentProperties:" <+> pretty as <+> pretty dpe
  pretty (DisjointDataProperties as dpe)     = "DisjointProperties:"   <+> pretty as <+> pretty dpe
  pretty (SameIndividual as is)              = "SameIndividual:"       <+> pretty as <+> pretty is
  pretty (DifferentIndividual as is)         = "DifferentIndividual:"  <+> pretty as <+> pretty is

instance Pretty a => Pretty (AtLeast2List a) where
  pretty = pretty . toList

instance Pretty Entity where
  pretty (DatatypeEntity de)           = "Datatype"           <+> "(" <+> pretty de <+> ")"
  pretty (ClassEntity ce)              = "Class"              <+> "(" <+> pretty ce <+> ")"
  pretty (ObjectPropertyEntity ie)     = "ObjectProperty"     <+> "(" <+> pretty ie <+> ")"
  pretty (DataPropertyEntity de)       = "DataProperty"       <+> "(" <+> pretty de <+> ")"
  pretty (AnnotationPropertyEntity ae) = "AnnotationProperty" <+> "(" <+> pretty ae <+> ")"
  pretty (IndividualEntity ie)         = "NamedIndividual"    <+> "(" <+> pretty ie <+> ")"

-----------------------
-- Utility functions --
-----------------------

prettyM :: Pretty a => Maybe a -> Doc ann
prettyM Nothing = mempty
prettyM m = pretty m <> space

prettyM' :: Pretty a => Maybe a -> Doc ann
prettyM' Nothing = mempty
prettyM' m = space <> pretty m

-- | Prepends a value when the there is a Just
--
-- >>> prefix = T.pack "name: "
-- >>> show $ prependM prefix (Just "Joe")
-- "name: Joe"
--
-- >>> length . show $ prependM prefix (Nothing :: Maybe ())
-- 0
--
prependM :: Pretty a => T.Text -> Maybe a -> Doc ann
prependM t ma = let pma = (pretty t <>) . pretty <$> ma in fromMaybe mempty pma

join :: Pretty a => T.Text -> [a] -> Doc ann
join s xs = concatWith (surround (space <> pretty s <> space)) (pretty <$> xs)

