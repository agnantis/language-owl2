{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Language.OWL2.Functional.Pretty where

{-
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                               ( fromMaybe )
import           Data.Text.Prettyprint.Doc         hiding ( pretty, prettyList )
import qualified Data.Text.Prettyprint.Doc     as PP

import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types

class PrettyM a where
  pretty :: a -> Doc ann
  default pretty :: PP.Pretty a => a -> Doc ann
  pretty = PP.pretty
  prettyList :: [a] -> Doc ann
  prettyList = PP.list . map pretty

instance PrettyM () where
  pretty = mempty

-- instance (PrettyM a, PP.Pretty a) => PrettyM (Maybe a) where
--   pretty = PP.pretty
instance PrettyM a => PrettyM (Maybe a) where
  pretty Nothing = mempty
  pretty (Just x) = pretty x

instance PrettyM Char where
  pretty = PP.pretty
  prettyList = PP.prettyList

instance PrettyM a => PrettyM [a] where
    pretty = prettyList

instance PrettyM a => PrettyM (NonEmpty a) where
    pretty nel = sep (pretty <$> NE.toList nel)

instance PrettyM Double where
  pretty = PP.pretty

instance PrettyM Integer where
  pretty = PP.pretty

instance PrettyM Int where
  pretty = PP.pretty

instance PrettyM T.Text where
  pretty = PP.pretty

instance PrettyM IRI where
  pretty (FullIRI i) = enclose "<" ">" (PP.pretty i)
  pretty (AbbreviatedIRI pfx i) = pretty pfx <> ":" <> pretty i
  pretty (SimpleIRI i) = ":" <> pretty i -- TODO: does it really require ':'?

instance PrettyM PrefixDeclaration where
  pretty (PrefixD pfx i) = "Prefix" <> parens ((pretty pfx <> ":=") <> pretty i)

instance PrettyM ImportDeclaration where
  pretty (ImportD i) = "Import" <> parens (pretty i)

instance PrettyM a => PrettyM (AnnotatedList a) where
  pretty (AnnList nelst) = sep $ punctuate comma xs
    where xs :: [Doc ann]
          xs = (\(ma, a) -> pretty ma <-> pretty a) <$> NE.toList nelst

instance PrettyM Conjunction where
  pretty (ClassConj i rs) = "ObjectIntersectionOf" <> parens (pretty i <+> pretty rs)
  pretty (PrimConj ps)
    | null (NE.tail ps) = pretty ps
    | otherwise          = "ObjectIntersectionOf" <> parens (pretty ps)
      
instance PrettyM Description where
  pretty (Description nel)
    | null (NE.tail nel) = pretty nel
    | otherwise          = "ObjectUnionOf" <> parens (pretty nel)

instance PrettyM Annotation where
  pretty (Annotation i t) = "Annotation" <> parens (pretty i <+> pretty t)

instance PrettyM AnnotationTarget where
  pretty (NodeAT n)    = pretty n
  pretty (IriAT i)     = pretty i
  pretty (LiteralAT l) = pretty l 

instance PrettyM NodeID where
  pretty (NodeID s) = pretty s

instance PrettyM Literal where
  pretty (TypedLiteralC tl)      = pretty tl
  pretty (StringLiteralNoLang s) = dquotes $ pretty s
  pretty (StringLiteralLang st)  = pretty st
  pretty (IntegerLiteralC i)     = pretty i
  pretty (DecimalLiteralC d)     = pretty d
  pretty (FloatingLiteralC f)    = pretty f

instance PrettyM TypedLiteral where
  pretty (TypedL s dt) = dquotes (pretty s) <> "^^" <> pretty dt

instance PrettyM Datatype where
  pretty (IriDT i) = pretty i
  pretty IntegerDT = "xsd:integer"
  pretty DecimalDT = "xsd:decimal"
  pretty FloatDT   = "xsd:float"
  pretty StringDT  = "xsd:string"

instance PrettyM LiteralWithLang where
  pretty (LiteralWithLang s l) = dquotes (pretty s) <> "@" <> pretty l

instance PrettyM IntegerLiteral where
  pretty (IntegerL i) = pretty i

instance PrettyM DecimalLiteral where
  pretty (DecimalL d) = pretty d

instance PrettyM FloatPoint where
  pretty (FloatP d me) = pretty d <> pretty pme <> "f"
    where pme = ("e" ++) . show <$> me

instance PrettyM OntologyDocument where
  pretty (OntologyD pds o) = vsep (pretty <$> pds) <> emptyLine <> pretty o

instance PrettyM Ontology where
  pretty (Ontology mvi ims ans frs) = "Ontology" <> parens (nest 2 body)
   where
    body =  emptyLine
         <> pretty mvi
         <> emptyLine
         <> vsep (pretty <$> ims)
         <> emptyLine
         <> vsep (pretty <$> ans)
         <> emptyLine
         <> vsep (pretty <$> frs)

instance PrettyM OntologyVersionIRI where
  pretty (OntologyVersionIRI oIri mvIri) = pretty oIri <-> pretty mvIri

instance PrettyM a => PrettyM (WithNegation a) where
  pretty (Positive a) = pretty a
  pretty (Negative a) = "not" <+> pretty a


instance PrettyM ObjectPropertyExpression where
  pretty (OPE o)        = pretty o
  pretty (InverseOPE o) = "InverseObjectProperty" <> parens (pretty o)

instance PrettyM Primary where
  pretty (PrimaryR (Positive r)) = pretty r 
  pretty (PrimaryR (Negative r)) = "ObjectComplementOf" <> parens (pretty r) 
  pretty (PrimaryA (Positive a)) = pretty a
  pretty (PrimaryA (Negative a)) = "ObjectComplementOf" <> parens (pretty a)

-- prettifyWNDataAtomic :: WithNegation DataAtomic -> Doc ann
-- prettifyWNDataAtomic (Positive r) = pretty r
-- prettifyWNDataAtomic (Negative r) = undefined
-- 
-- prettifyWNAtomic :: WithNegation Atomic -> Doc ann
-- prettifyWNAtomic (Positive r) = pretty r
-- prettifyWNAtomic (Negative r) = undefined
-- 
-- prettifyWNRestriction :: WithNegation Restriction -> Doc ann
-- prettifyWNRestriction (Positive r) = pretty r
-- prettifyWNRestriction (Negative r) = undefined
-- 
-- prettifyWIObjPropertyExpr :: WithInversion ObjectPropertyIRI -> Doc ann
-- prettifyWIObjPropertyExpr (Plain o) = pretty o
-- prettifyWIObjPropertyExpr (Inverse o) = "InverseObjectProperty" <> parens (pretty o)

instance PrettyM Atomic where
  pretty (AtomicClass i)        = pretty i
  pretty (AtomicIndividuals is) = "ObjectOneOf" <> parens (sep (pretty <$> NE.toList is))
  pretty (AtomicDescription d)  = pretty d

instance PrettyM Individual where
  pretty (IRIIndividual i) = pretty i
  pretty (NodeIndividual n) = pretty n

instance PrettyM Restriction where
  pretty (OPRestriction o) = pretty o
  pretty (DPRestriction d) = pretty d

instance PrettyM ObjectPropertyRestriction where
  pretty (OPR e SelfOPR)           = "ObjectHasSelf"          <> parens (pretty e)
  pretty (OPR e (SomeOPR p))       = "ObjectSomeValuesFrom"   <> parens (pretty e <+> pretty p)
  pretty (OPR e (OnlyOPR p))       = "ObjectAllValuesFrom"    <> parens (pretty e <+> pretty p)
  pretty (OPR e (ValueOPR i))      = "ObjectHasValue"         <> parens (pretty e <+> pretty i)
  pretty (OPR e (MinOPR i mp))     = "ObjectMinCardinality"   <> parens (pretty e <+> pretty i <-> pretty mp)
  pretty (OPR e (MaxOPR i mp))     = "ObjectMaxCardinality"   <> parens (pretty e <+> pretty i <-> pretty mp)
  pretty (OPR e (ExactlyOPR i mp)) = "ObjectExactCardinality" <> parens (pretty e <+> pretty i <-> pretty mp)
    
instance PrettyM DataPropertyRestriction where
  pretty (DPR e (SomeDPR p))       = "DataSomeValuesFrom"   <> parens (pretty e <+> pretty p)
  pretty (DPR e (OnlyDPR p))       = "DataAllValuesFrom"    <> parens (pretty e <+> pretty p)
  pretty (DPR e (ValueDPR l))      = "DataHasValue"         <> parens (pretty e <+> pretty l)
  pretty (DPR e (MinDPR i mp))     = "DataMinCardinality"   <> parens (pretty e <+> pretty i <-> pretty mp) 
  pretty (DPR e (MaxDPR i mp))     = "DataMaxCardinality"   <> parens (pretty e <+> pretty i <-> pretty mp) 
  pretty (DPR e (ExactlyDPR i mp)) = "DataExactCardinality" <> parens (pretty e <+> pretty i <-> pretty mp)

--instance PrettyM ObjectPropertyRestrictionType where
--  pretty SelfOPR           = "Self"
--  pretty (SomeOPR p)       = "ObjectSomeValuesFrom" <> parens (pretty p)
--  pretty (OnlyOPR p)       = "only"    <+> pretty p
--  pretty (MinOPR i mp)     = "min"     <+> pretty i <-> pretty mp 
--  pretty (MaxOPR i mp)     = "max"     <+> pretty i <-> pretty mp 
--  pretty (ExactlyOPR i mp) = "exactly" <+> pretty i <-> pretty mp 

--instance PrettyM DataPropertyRestrictionType where
--  pretty (SomeDPR p)       = "some"    <+> pretty p
--  pretty (OnlyDPR p)       = "only"    <+> pretty p
--  pretty (ValueDPR l)      = "value"   <+> pretty l
--  pretty (MinDPR i mp)     = "min"     <+> pretty i <-> pretty mp 
--  pretty (MaxDPR i mp)     = "max"     <+> pretty i <-> pretty mp 
--  pretty (ExactlyDPR i mp) = "exactly" <+> pretty i <-> pretty mp

instance PrettyM DataPrimary where
  pretty (DataPrimary (Positive d)) = pretty d
  pretty (DataPrimary (Negative d)) = "DataComplementOf" <> parens (pretty d)

instance PrettyM DataAtomic where
  pretty (DatatypeDA d)            = pretty d
  pretty (LiteralListDA ls)        = "DataOneOf" <> parens (join "," (NE.toList ls))
  pretty (DatatypeRestrictionDA r) = pretty r
  pretty (DataRangeDA r)           = pretty r

instance PrettyM DataRange where
  pretty (DataRange dcs)
    | null (NE.tail dcs) = pretty dcs
    | otherwise          = "DataUnionOf" <> parens (pretty dcs)

instance PrettyM DataConjunction where
  pretty (DataConjunction dps)
    | null (NE.tail dps) = pretty dps
    | otherwise          = "DataIntersectionOf" <> parens (pretty dps)

instance PrettyM DatatypeRestriction where
  pretty (DatatypeRestriction d re) = "DatatypeRestriction" <> parens (pretty d <+> join "," (NE.toList  re))

instance PrettyM RestrictionExp where
  pretty (RestrictionExp f l) = pretty f <+> pretty l

instance PrettyM Facet where
  pretty LENGTH_FACET     = "xsd:length"
  pretty MIN_LENGTH_FACET = "xsd:minLength"
  pretty MAX_LENGTH_FACET = "xsd:maxLength"
  pretty PATTERN_FACET    = "xsd:pattern"
  pretty LANG_RANGE_FACET = "rdf:langRange"
  pretty LE_FACET         = "xsd:minInclusive"
  pretty L_FACET          = "xsd:minExclusive"
  pretty GE_FACET         = "xsd:maxInclusive"
  pretty G_FACET          = "xsd:maxExclusive"

instance PrettyM DatatypeFrame where
  pretty (DatatypeF dt ma mdr) = vsep [ prettyAnnotationAssertion dt (flattenAnnList ma)
                                      , "DatatypeDefinition" <> parens (pretty dt <-> pretty mdr) -- TODO: this is wrong
                                      ]

instance PrettyM AnnotDataRange where
    pretty (AnnotDataRange a dr) = pretty a <-> pretty dr

prettyAnnotationAssertion :: PrettyM a => a -> Maybe Annotations -> Doc ann
prettyAnnotationAssertion _ Nothing = mempty
prettyAnnotationAssertion c (Just aList) = vsep (
        (\(a, Annotation i t) -> pretty "AnnotationAssertion"
                              <> parens (pretty a <-> pretty i <+> pretty c <+> pretty t))
        <$> annListToList aList)

instance PrettyM ClassFrame where
  pretty (ClassF cls ces) = "Declaration" <> parens ("Class" <> parens(pretty cls))
                        <> line
                        <> vsep (pretty' <$> ces)
   where
    pretty' :: ClassElement -> Doc ann
    pretty' (AnnotationCE as)          = prettyAnnotationAssertion cls (Just as)
    pretty' (SubClassOfCE ds)          = multiD ("SubClassOf", ds)
    pretty' (EquivalentToCE ds)        = multiD ("EquivalentClasses", ds)
    pretty' (DisjointToCE ds)          = multiD ("DisjointClasses", ds)
    pretty' (DisjointUnionOfCE mas ds) = "DisjointUnion" <> parens (pretty cls <+> pretty mas <-> pretty (toList ds))
    pretty' (HasKeyCE mas od)          = "HasKey"        <> parens (pretty cls <+> pretty mas <-> pretty (NE.toList od))
    multiD :: (T.Text, Descriptions) -> Doc ann
    multiD (lbl, aList) = vsep (
        (\(a, x) -> pretty lbl
                 <> parens (pretty a <-> pretty cls <-> pretty x))
        <$> annListToList aList)

instance PrettyM ObjectOrDataPE where
  pretty (ObjectPE ope) = pretty ope
  pretty (DataPE dpe)   = pretty dpe

instance PrettyM ObjectPropertyFrame where
  pretty (ObjectPropertyF i ops) = "ObjectProperty:" <+> pretty i 
                                 <> line
                                 <> indent 4 (vsep (pretty <$> ops))

instance PrettyM ObjectPropertyElement where
  pretty (AnnotationOPE a)           = "Annotations:"      <+> align (pretty a)
  pretty (DomainOPE ds)              = "Domain:"           <+> align (pretty ds)
  pretty (RangeOPE ds)               = "Range:"            <+> align (pretty ds)
  pretty (CharacteristicsOPE ops)    = "Characteristics:"  <+> align (pretty ops)
  pretty (SubPropertyOfOPE ops)      = "SubPropertyOf:"    <+> align (pretty ops)
  pretty (EquivalentToOPE ops)       = "EquivalentTo:"     <+> align (pretty ops)
  pretty (DisjointWithOPE ops)       = "DisjointWith:"     <+> align (pretty ops)
  pretty (InverseOfOPE ops)          = "InverseOf:"        <+> align (pretty ops)
  pretty (SubPropertyChainOPE a ops) = "SubPropertyChain:" <+> align (pretty a <+> join "o" (toList ops))

instance PrettyM ObjectPropertyCharacteristics where
  pretty FUNCTIONAL         = "Functional"
  pretty INVERSE_FUNCTIONAL = "InverseFunctional"
  pretty REFLEXIVE          = "Reflexive"
  pretty IRREFLEXIVE        = "Irreflexive"
  pretty SYMMETRIC          = "Symmetric"
  pretty ASYMMETRIC         = "Asymmetric"
  pretty TRANSITIVE         = "Transitive"


instance PrettyM DataPropertyFrame where
  pretty (DataPropertyF i dps) = "DataProperty:" <+> pretty i 
                               <> line
                               <> indent 4 (vsep (pretty <$> dps))

instance PrettyM DataPropertyElement where
  pretty (AnnotationDPE a)        = "Annotations:"      <+> align (pretty a)
  pretty (DomainDPE ds)           = "Domain:"           <+> align (pretty ds)
  pretty (RangeDPE ds)            = "Range:"            <+> align (pretty ds)
  pretty (CharacteristicsDPE dps) = "Characteristics:"  <+> align (pretty dps)
  pretty (SubPropertyOfDPE dps)   = "SubPropertyOf:"    <+> align (pretty dps)
  pretty (EquivalentToDPE dps)    = "EquivalentTo:"     <+> align (pretty dps)
  pretty (DisjointWithDPE dps)    = "DisjointWith:"     <+> align (pretty dps)

instance PrettyM DataPropertyCharacteristics where
  pretty FUNCTIONAL_DPE = "Functional"

instance PrettyM Frame where
  pretty (FrameDT df)  = pretty df
  pretty (FrameC cf)   = pretty cf
  pretty (FrameOP opf) = pretty opf
  pretty (FrameDP dpf) = pretty dpf
  pretty (FrameAP af ) = pretty af 
  pretty (FrameI ifr)  = pretty ifr
  pretty (FrameM m)    = pretty m 

instance PrettyM AnnotationPropertyFrame where
  pretty (AnnotationPropertyF i aps) = "Declaration" <> parens ("AnnotationProperty" <> parens (pretty i
                                     <> mLine
                                     <> indent indt (vsep (pretty <$> aps))))
   where
    (mLine, indt)  = if null aps then (mempty, 0) else (line, 4)

instance PrettyM AnnotationPropertyElement where
  pretty (AnnotationAPE a)        = "Annotations:"      <+> pretty a
  pretty (DomainAPE as)           = "Domain:"           <+> pretty as
  pretty (RangeAPE as)            = "Range:"            <+> pretty as
  pretty (SubPropertyOfAPE aps)   = "SubPropertyOf:"    <+> pretty aps

instance PrettyM IndividualFrame where
  pretty (IndividualF i oes) = "Individual:" <+> pretty i
                             <> line
                             <> indent 4 (vsep (pretty <$> oes))

instance PrettyM IndividualElement where
  pretty (AnnotationIE a)     = "Annotations:"  <+> pretty a
  pretty (TypeIE ds)          = "Types:"        <+> pretty ds
  pretty (FactIE af)          = "Facts:"        <+> pretty af
  pretty (SameAsIE ai)        = "SameAs:"       <+> pretty ai
  pretty (DifferentFromIE ai) = "DifferentFrom" <+> pretty ai

instance PrettyM FactElement where
  pretty (ObjectPropertyFE opf) = pretty opf 
  pretty (DataPropertyFE dpf)   = pretty dpf

instance PrettyM ObjectPropertyFact where
  pretty (ObjectPropertyFact i ind) = pretty i <+> pretty ind

instance PrettyM DataPropertyFact where
  pretty (DataPropertyFact i l) = pretty i <+> pretty l

instance PrettyM Misc where
  pretty (EquivalentClasses as ds)           = "EquivalentClasses"          <> parens (pretty as <+> pretty ds)
  pretty (DisjointClasses as ds)             = "DisjointClasses"            <> parens (pretty as <+> pretty ds)
  pretty (EquivalentObjectProperties as ope) = "EquivalentObjectProperties" <> parens (pretty as <+> pretty ope)
  pretty (DisjointObjectProperties as ope)   = "DisjointObjectProperties"   <> parens (pretty as <+> pretty ope)
  pretty (EquivalentDataProperties as dpe)   = "EquivalentDataProperties"   <> parens (pretty as <+> pretty dpe)
  pretty (DisjointDataProperties as dpe)     = "DisjointDataProperties"     <> parens (pretty as <+> pretty dpe)
  pretty (SameIndividual as is)              = "SameIndividual"             <> parens (pretty as <+> pretty is)
  pretty (DifferentIndividuals as is)        = "DifferentIndividuals"       <> parens (pretty as <+> pretty is)

instance PrettyM a => PrettyM (AtLeast2List a) where
  pretty = pretty . toList

instance PrettyM Entity where
  pretty (DatatypeEntity de)           = "Declaration" <> parens ("Datatype"           <+> "(" <+> pretty de <+> ")")
  pretty (ClassEntity ce)              = "Declaration" <> parens ("Class"              <+> "(" <+> pretty ce <+> ")")
  pretty (ObjectPropertyEntity ie)     = "Declaration" <> parens ("ObjectProperty"     <+> "(" <+> pretty ie <+> ")")
  pretty (DataPropertyEntity de)       = "Declaration" <> parens ("DataProperty"       <+> "(" <+> pretty de <+> ")")
  pretty (AnnotationPropertyEntity ae) = "Declaration" <> parens ("AnnotationProperty" <+> "(" <+> pretty ae <+> ")")
  pretty (IndividualEntity ie)         = "Declaration" <> parens ("NamedIndividual"    <+> "(" <+> pretty ie <+> ")")

-----------------------
-- Utility functions --
-----------------------

-- prettyM :: PrettyM a => Maybe a -> Doc ann
-- prettyM Nothing = mempty
-- prettyM m = pretty m <> space
-- 
-- prettyM' :: PrettyM a => Maybe a -> Doc ann
-- prettyM' Nothing = mempty
-- prettyM' m = space <> pretty m

-- | Prepends a value when the there is a Just
--
-- >>> prefix = T.pack "name: "
-- >>> show $ prependM prefix (Just "Joe")
-- "name: Joe"
--
-- >>> length . show $ prependM prefix (Nothing :: Maybe ())
-- 0
--
prependM :: PrettyM a => T.Text -> Maybe a -> Doc ann
prependM t ma = let pma = (pretty t <>) . pretty <$> ma in fromMaybe mempty pma

join :: PrettyM a => T.Text -> [a] -> Doc ann
join s xs = concatWith (surround (space <> pretty s <> space)) (pretty <$> xs)

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

-}
