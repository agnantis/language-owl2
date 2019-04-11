{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.OWL2.Manchester.Pretty where

import           Control.Monad.State               hiding ( join )
import           Data.List                                ( intersperse, sort )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                               ( fromMaybe )
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as M
import           Data.Text.Prettyprint.Doc         hiding ( pretty, prettyList )
import qualified Data.Text.Prettyprint.Doc     as PP

import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types
import           Debug.Trace

indSize :: Int
indSize = 4

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
  pretty (SimpleIRI i) = pretty i

instance PrettyM PrefixDeclaration where
  pretty (PrefixD pfx i) = "Prefix:" <+> (pretty pfx <> ":") <+> pretty i

instance PrettyM ImportDeclaration where
  pretty (ImportD i) = "Import:" <+> pretty i

-- instance PrettyM a => PrettyM (AnnotatedList a) where
--   pretty (AnnList nelst) = sep $ punctuate comma xs
--     where xs :: [Doc ann]
--           xs = (\(ma, a) -> prependM "Annotations: " ma <-> pretty a) <$> NE.toList nelst
          
--instance PrettyM a => PrettyM (NE.NonEmpty (Annotated a)) where
--  pretty nelst = sep $ punctuate comma xs
--    where xs :: [Doc ann]
--          xs =  pretty <$> NE.toList nelst

instance PrettyM a => PrettyM (Annotated a) where
  pretty (Annotated ([], a)) = pretty a
  pretty (Annotated (xs, a)) = "Annotations:" <+> pretty xs <+> pretty a
  prettyList [] = mempty
  prettyList as = vsep $ pretty <$> as
          
-- instance PrettyM Description where
--   pretty (Description nel) = sep . punctuate " or " $ pretty <$> NE.toList nel
instance PrettyM ClassExpression where
  pretty (CExpClass iri)                        = pretty iri
  pretty (CExpObjectIntersectionOf ces)         = parens $ concatWith (surround " and ")  (pretty <$> toList ces)
  pretty (CExpObjectUnionOf ces)                = parens $ concatWith (surround " or ") (pretty <$> toList ces)
  pretty (CExpObjectComplementOf ce)            = "not" <+> pretty ce
  pretty (CExpObjectOneOf inds)                 = parens . braces $ concatWith (surround ", ") (pretty <$> inds)
  pretty (CExpObjectSomeValuesFrom ope ce)      = pretty ope <+> "some"    <+> pretty ce
  pretty (CExpObjectAllValuesFrom ope ce)       = pretty ope <+> "only"    <+> pretty ce
  pretty (CExpObjectHasValue ope i)             = pretty ope <+> "value"   <+> pretty i
  pretty (CExpObjectHasSelf ope)                = pretty ope <+> "Self"
  pretty (CExpObjectMinCardinality i ope mce)   = pretty ope <+> "min"     <+> pretty i   <+> pretty mce
  pretty (CExpObjectMaxCardinality i ope mce)   = pretty ope <+> "max"     <+> pretty i   <+> pretty mce
  pretty (CExpObjectExactCardinality i ope mce) = pretty ope <+> "exactly" <+> pretty i   <+> pretty mce
  pretty (CExpDataSomeValuesFrom dps dr)        = pretty (NE.head dps) <+> "some"    <+> pretty dr --TODO: dpes is a list!
  pretty (CExpDataAllValuesFrom dps dr)         = pretty (NE.head dps) <+> "only"    <+> pretty dr -- TODO: dpes is a list!
  pretty (CExpDataHasValue dpe l)               = pretty dpe <+> "value"   <+> pretty l
  pretty (CExpDataMinCardinality i dpe mdr)     = pretty dpe <+> "min"     <+> pretty i   <+> pretty mdr
  pretty (CExpDataMaxCardinality i dpe mdr)     = pretty dpe <+> "max"     <+> pretty i   <+> pretty mdr
  pretty (CExpDataExactCardinality i dpe mdr)   = pretty dpe <+> "exactly" <+> pretty i   <+> pretty mdr
  prettyList xs                                 = vsep (punctuate "," (pretty <$> xs))


instance PrettyM Annotation where
  pretty (Annotation i t) = pretty i <+> pretty t

instance PrettyM AnnotationValue where
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
  pretty (TypedL s dt) = pretty s <> "^^" <> pretty dt

instance PrettyM Datatype where
  pretty (Datatype (AbbreviatedIRI "xsd" "integer")) = "integer"
  pretty (Datatype (AbbreviatedIRI "xsd" "float"))   = "float"
  pretty (Datatype (AbbreviatedIRI "xsd" "decimal")) = "decimal"
  pretty (Datatype (AbbreviatedIRI "xsd" "string"))  = "string"
  pretty (Datatype i)                                = pretty i

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
  pretty (Ontology mvi ims ans axms) = "Ontology:" <-> pretty mvi
                                   <> emptyLine
                                   <> vsep (pretty <$> ims)
                                   <> emptyLine
                                   <> "Annotations:" <> nest indSize (line <> vsep (pretty <$> ans))
                                   <> emptyLine
                                   <> prettyAxioms axms

instance PrettyM OntologyVersionIRI where
  pretty (OntologyVersionIRI oIri mvIri) = pretty oIri <-> pretty mvIri

instance PrettyM a => PrettyM (WithNegation a) where
  pretty (Positive a) = pretty a
  pretty (Negative a) = "not" <+> pretty a

instance PrettyM ObjectPropertyExpression where
  pretty (OPE o)        = pretty o
  pretty (InverseOPE o) = "inverse" <+> pretty o

instance PrettyM Primary where
  pretty (PrimaryR r) = pretty r 
  pretty (PrimaryA a) = pretty a

instance PrettyM Atomic where
  pretty (AtomicClass i) = pretty i
  pretty (AtomicIndividuals is) = braces . pretty . NE.toList $ is 
  pretty (AtomicDescription d) = parens . pretty $ d 

instance PrettyM TotalIRI where
  pretty (NamedIRI i) = pretty i
  pretty (AnonymousIRI n) = pretty n

instance PrettyM Restriction where
  pretty (OPRestriction o) = pretty o
  pretty (DPRestriction d) = pretty d

instance PrettyM ObjectPropertyRestriction where
  pretty (OPR e rt) = pretty e <+> pretty rt

instance PrettyM DataPropertyRestriction where
  pretty (DPR e rt) = pretty e <+> pretty rt

instance PrettyM ObjectPropertyRestrictionType where
  pretty SelfOPR           = "Self"
  pretty (SomeOPR p)       = "some"    <+> pretty p
  pretty (OnlyOPR p)       = "only"    <+> pretty p
  pretty (ValueOPR i)      = "value"   <+> pretty i
  pretty (MinOPR i mp)     = "min"     <+> pretty i <-> pretty mp 
  pretty (MaxOPR i mp)     = "max"     <+> pretty i <-> pretty mp 
  pretty (ExactlyOPR i mp) = "exactly" <+> pretty i <-> pretty mp 

instance PrettyM DataPropertyRestrictionType where
  pretty (SomeDPR p)       = "some"    <+> pretty p
  pretty (OnlyDPR p)       = "only"    <+> pretty p
  pretty (ValueDPR l)      = "value"   <+> pretty l
  pretty (MinDPR i mp)     = "min"     <+> pretty i <-> pretty mp 
  pretty (MaxDPR i mp)     = "max"     <+> pretty i <-> pretty mp 
  pretty (ExactlyDPR i mp) = "exactly" <+> pretty i <-> pretty mp 

instance PrettyM DataRange where
  pretty (DatatypeDR dt)      = pretty dt
  pretty (IntersectionDR drs) = join "and" (toList drs)
  pretty (UnionDR drs)        = join "or" (toList drs)
  pretty (ComplementDR dr)    = "not" <+> pretty dr
  pretty (OneOfDR ls)         = braces $ join "," (NE.toList ls)
  pretty (RestrictionDR r)    = pretty r

instance PrettyM DatatypeRestriction where
  pretty (DatatypeRestriction d re) = pretty d <+> brackets (join "," (NE.toList  re))

instance PrettyM RestrictionExp where
  pretty (RestrictionExp f l) = pretty f <+> pretty l

instance PrettyM Facet where
  pretty LENGTH_FACET     = "length"
  pretty MIN_LENGTH_FACET = "minLength"
  pretty MAX_LENGTH_FACET = "maxLength"
  pretty PATTERN_FACET    = "pattern"
  pretty LANG_RANGE_FACET = "langRange"
  pretty LE_FACET         = "<="
  pretty L_FACET          = "<"
  pretty GE_FACET         = ">="
  pretty G_FACET          = ">"

instance PrettyM Conjunction where
  pretty (ClassConj i rs) = pretty i <+> "that" <+> join "and" (NE.toList rs)
  pretty (PrimConj ps)    = join "and" (NE.toList ps)

parensM :: Doc ann -> Doc ann
parensM = nest indSize

instance PrettyM Axiom where
  pretty (DeclarationAxiom ans e) = pretty e <> parensM (line <> pretty ans)
  --pretty (AnnotationAxiomDomain ans p i) = prettyAxiom "AnnotationPropertyDomain" (ans, p, i)
  --pretty (AnnotationAxiomRange ans p i) = prettyAxiom "AnnotationPropertyRange" (ans, p, i)
  --pretty (AnnotationAxiomSubProperty ans p1 p2) = prettyAxiom "SubAnnotationPropertyOf" (ans, p1, p2)
  pretty (AnnotationAxiomAssertion ans _ (Annotation t v)) = pretty ans <-> pretty t <+> pretty v
  --pretty (DatatypeAxiomDefinition ans d r) = prettyAxiom "DatatypeDefinition" (ans, d, r)
  --pretty (ObjectPropAxiomDomain ans o e) = prettyAxiom "ObjectPropertyDomain" (ans, o, e)
  --pretty (ObjectPropAxiomRange ans o e) = prettyAxiom "ObjectPropertyRange" (ans, o, e)
  --pretty (ObjectPropAxiomCharacteristics ans o c) = pretty c <> "ObjectProperty" <> parens (pretty ans <-> pretty o)
  --pretty (ObjectPropAxiomSubProperty ans o1 o2) = prettyAxiom "SubObjectPropertyOf" (ans, o1,  o2)
  --pretty (ObjectPropAxiomChainSubProperty ans c o) = prettyAxiom "SubObjectPropertyOf" (ans, c, o)
  --pretty (ObjectPropAxiomEquivalent ans o os) = prettyAxiom "EquivalentObjectProperties" (ans, o, os)
  --pretty (ObjectPropAxiomDisjoint  ans o os) = prettyAxiom "DisjointObjectProperties" (ans, o, os)
  --pretty (ObjectPropAxiomInverse ans o1 o2) = prettyAxiom "InverseObjectProperties" (ans, o1, o2)
  --pretty (DataPropAxiomDomain ans d c) = prettyAxiom "DataPropertyDomain" (ans, d, c)
  --pretty (DataPropAxiomRange ans d r) = prettyAxiom "DataPropertyRange" (ans, d, r)
  --pretty (DataPropAxiomCharacteristics ans d c) = pretty c <> "DataProperty" <> parens (pretty ans <-> pretty d)
  --pretty (DataPropAxiomSubProperty ans d1 d2) = prettyAxiom "SubDataPropertyOf" (ans, d1, d2)
  --pretty (DataPropAxiomEquivalent ans d ds) = prettyAxiom "EquivalentDataProperties" (ans, d, ds)
  --pretty (DataPropAxiomDisjoint ans d ds) = prettyAxiom "DisjointDataPropertiesDataProperties" (ans, d, ds)
  pretty (ClassAxiomSubClassOf ans _ e) = pretty ans <-> pretty e
  pretty (ClassAxiomEquivalentClasses ans _ es) = pretty ans <-> pretty (NE.toList es)
  --pretty (ClassAxiomDisjointClasses ans e es) = prettyAxiom "DisjointClasses" (ans, e, es)
  --pretty (ClassAxiomDisjointUnion ans i es) = prettyAxiom "DisjointUnion" (ans, i, es)
  --pretty (ClassAxiomHasKey ans e od) = prettyAxiom "HasKey" (ans, e, od)
  --pretty (AssertionAxiomSameIndividuals ans i is) = prettyAxiom "SameIndividual" (ans, i, is)
  --pretty (AssertionAxiomDifferentIndividuals ans i is) = prettyAxiom "DifferentIndividuals" (ans, i, is)
  pretty (AssertionAxiomClass ans _ e) = parensM (pretty ans <-> pretty e)
  --pretty (AssertionAxiomObjectProperty ans o i1 i2) = prettyAxiom "ObjectPropertyAssertion" (ans, o, atLeast2List i1 i2 [])
  --pretty (AssertionAxiomNegativeObjectProperty ans o i1 i2) = prettyAxiom "NegativeObjectPropertyAssertion" (ans, o, atLeast2List i1 i2 [])
  --pretty (AssertionAxiomDataProperty ans d i l) = "DataPropertyAssertion" <> parens (pretty ans <-> pretty d <+> pretty i <+> pretty l)
  --pretty (AssertionAxiomNegativeDataProperty ans d i l) = "NegativeDataPropertyAssertion" <> parens (pretty ans <-> pretty d <+> pretty i <+> pretty l)
  pretty x = pretty $ show x
  prettyList = prettyAxiomList

prettyAxiomList :: [Axiom] -> Doc ann
prettyAxiomList [] = mempty
prettyAxiomList xs@(AssertionAxiomClass{}:_) = prettyWithTitle "Types:" xs
prettyAxiomList xs@(AnnotationAxiomAssertion{}:_) = prettyWithTitle "Annotations:" xs
prettyAxiomList xs@(ClassAxiomSubClassOf{}:_) = prettyWithTitle "SubClassOf:" xs
prettyAxiomList xs@(ClassAxiomEquivalentClasses{}:_) = prettyWithTitle "EquivalentTo:" xs
prettyAxiomList xs = vsep (punctuate "," (pretty <$> xs))


prettyWithTitle :: Doc ann -> [Axiom] -> Doc ann
prettyWithTitle title els = parensM $ vsep $ title : punctuate "," (pretty <$> els)


prettyAxiomGroups :: [[Axiom]] -> Doc ann
prettyAxiomGroups = vsep . fmap prettyAxiomGroup


prettyAxiomGroup :: [Axiom] -> Doc ann
prettyAxiomGroup xss = prettyFrame decls rst
 where
   (decls, rst) = runState (extractAxioms predicate) xss
   predicate a = axiomType a == DeclarationAxiomType


-- | The first axiom will be either a declaration axiom or nothing.
-- The function suuports more than a single declaration axiom (e.g. when a class
-- and a property share the same IRI). In that case all the other axioms that refer
-- to these IRIs will be grouped together
-- When there is no declaration axiom (i.e., the first argument is an empty list)
-- means that all the other axioms are misc (independant) of a specifc entity axioms
prettyFrame :: [Axiom] -> [Axiom] -> Doc ann
prettyFrame [] ax = vsep (pretty <$> ax)
                    <> line
prettyFrame dclrs ax = vsep (pretty <$> dclrs) -- vsep implicitelly inserts a newline at the end
                       <> parensM (vsep (pretty <$> groupAxiomsOnConstructor ax))
                       <> emptyLine

prettyAxioms :: [Axiom] -> Doc ann
prettyAxioms as =
  mkTitle "Axioms"
  <> prettyAxiomGroups (groupAxiomsOnIRI as) 
   where
     predicate a = axiomType a == DeclarationAxiomType
     (decls, rst) = runState (extractAxioms predicate) as
     mkTitle :: Doc ann -> Doc ann
     mkTitle title = vsep [ emptyLine
                          , "##############################"
                          , "#" <+> title
                          , "##############################"
                          , line
                          ]

--prettyAxioms' :: [Axiom] -> Doc ann
--prettyAxioms' as =
--  mkTitle "Axioms"
--  <> pretty axs
--  -- <> prettyAxiomGroups (groupAxiomsOnIRI as) 
--   where
--     iriAxiomMap = mapAxiomsOnIRI as
--     (decls, rst) = runState (getAxioms DeclarationAxiomType) as
--     iris = getIRI <$> sort decls
--     axs = flip M.lookup iriAxiomMap <$> iris
--     decAxPairs = (<$> sort decls) 
--     -- TODO: we need to:
--     --  1. Extract entities from declarations
--     --  2. Build pair (entity, [axiom]) and pass it to prettyFrame below
--     --
--     mkTitle :: Doc ann -> Doc ann
--     mkTitle title = vsep [ emptyLine
--                          , "##############################"
--                          , "#" <+> title
--                          , "##############################"
--                          , line
--                          ]

--instance PrettyM DatatypeAxiom where
--  pretty (DatatypeF dt ma mdr) = "DatatypeFrame:" <+> pretty dt
--                               <> line
--                               <> pma
--                               <> line
--                               <> prependM "EquivalentTo: " mdr
--    where pma = if null ma
--                then mempty
--                else "Annotations:" <+> indent 4 (align (vsep (pretty <$> ma)))
--
--instance PrettyM AnnotDataRange where
--  pretty (AnnotDataRange a dr) = pretty a <+> pretty dr

-- instance PrettyM Misc where
--   pretty (EquivalentClasses as ds)           = "EquivalentClasses:"    <+> pretty as <+> pretty ds
--   pretty (DisjointClasses as ds)             = "DisjointClasses:"      <+> pretty as <+> pretty ds
--   pretty (EquivalentObjectProperties as ope) = "EquivalentProperties:" <+> pretty as <+> pretty ope
--   pretty (DisjointObjectProperties as ope)   = "DisjointProperties:"   <+> pretty as <+> pretty ope
--   pretty (EquivalentDataProperties as dpe)   = "EquivalentProperties:" <+> pretty as <+> pretty dpe
--   pretty (DisjointDataProperties as dpe)     = "DisjointProperties:"   <+> pretty as <+> pretty dpe
--   pretty (SameIndividual as is)              = "SameIndividual:"       <+> pretty as <+> pretty is
--   pretty (DifferentIndividuals as is)        = "DifferentIndividuals:" <+> pretty as <+> pretty is

-- instance PrettyM ClassFrame where
--   pretty (ClassF i ces) = "Class:" <+> pretty i
--                         <> line
--                         <> indent 4 (vsep (pretty <$> ces))
--
--instance PrettyM ClassElement where
--  pretty (AnnotationCE as)          = "Annotations:"     <+> align (pretty as)
--  pretty (SubClassOfCE ds)          = "SubClassOf:"      <+> align (pretty ds)
--  pretty (EquivalentToCE ds)        = "EquivalentTo:"    <+> align (pretty ds)
--  pretty (DisjointToCE ds)          = "DisjointWith:"    <+> align (pretty ds)
--  pretty (DisjointUnionOfCE mas ds) = "DisjointUnionOf:" <-> align (pretty mas <-> pretty (toList ds))
--  pretty (HasKeyCE mas od)          = "HasKey:"          <-> align (pretty mas <-> pretty (NE.toList od))

instance PrettyM ObjectOrDataPE where
  pretty (ObjectPE ope) = pretty ope
  pretty (DataPE dpe)   = pretty dpe

-- instance PrettyM ObjectPropertyFrame where
--   pretty (ObjectPropertyF i ops) = "ObjectProperty:" <+> pretty i 
--                                  <> line
--                                  <> indent 4 (vsep (pretty <$> ops))
-- 
-- instance PrettyM ObjectPropertyElement where
--   pretty (AnnotationOPE a)           = "Annotations:"      <+> align (pretty a)
--   pretty (DomainOPE ds)              = "Domain:"           <+> align (pretty ds)
--   pretty (RangeOPE ds)               = "Range:"            <+> align (pretty ds)
--   pretty (CharacteristicsOPE ops)    = "Characteristics:"  <+> align (pretty ops)
--   pretty (SubPropertyOfOPE ops)      = "SubPropertyOf:"    <+> align (pretty ops)
--   pretty (EquivalentToOPE ops)       = "EquivalentTo:"     <+> align (pretty ops)
--   pretty (DisjointWithOPE ops)       = "DisjointWith:"     <+> align (pretty ops)
--   pretty (InverseOfOPE ops)          = "InverseOf:"        <+> align (pretty ops)
--   pretty (SubPropertyChainOPE a ops) = "SubPropertyChain:" <+> align (pretty a <+> join "o" (toList ops))

instance PrettyM ObjectPropertyCharacteristic where
  pretty FUNCTIONAL         = "Functional"
  pretty INVERSE_FUNCTIONAL = "InverseFunctional"
  pretty REFLEXIVE          = "Reflexive"
  pretty IRREFLEXIVE        = "Irreflexive"
  pretty SYMMETRIC          = "Symmetric"
  pretty ASYMMETRIC         = "Asymmetric"
  pretty TRANSITIVE         = "Transitive"


--instance PrettyM DataPropertyFrame where
--  pretty (DataPropertyF i dps) = "DataProperty:" <+> pretty i 
--                               <> line
--                               <> indent 4 (vsep (pretty <$> dps))
--
--instance PrettyM DataPropertyElement where
--  pretty (AnnotationDPE a)        = "Annotations:"      <+> align (pretty a)
--  pretty (DomainDPE ds)           = "Domain:"           <+> align (pretty ds)
--  pretty (RangeDPE ds)            = "Range:"            <+> align (pretty ds)
--  pretty (CharacteristicsDPE dps) = "Characteristics:"  <+> align (pretty dps)
--  pretty (SubPropertyOfDPE dps)   = "SubPropertyOf:"    <+> align (pretty dps)
--  pretty (EquivalentToDPE dps)    = "EquivalentTo:"     <+> align (pretty dps)
--  pretty (DisjointWithDPE dps)    = "DisjointWith:"     <+> align (pretty dps)

instance PrettyM DataPropertyCharacteristics where
  pretty FUNCTIONAL_DPE = "Functional"


--instance PrettyM AnnotationPropertyFrame where
--  pretty (AnnotationPropertyF i aps) = "AnnotationProperty:" <+> pretty i
--                                     <> line
--                                     <> indent 4 (vsep (pretty <$> aps))
--
--instance PrettyM AnnotationPropertyElement where
--  pretty (AnnotationAPE a)        = "Annotations:"      <+> pretty a
--  pretty (DomainAPE as)           = "Domain:"           <+> pretty as
--  pretty (RangeAPE as)            = "Range:"            <+> pretty as
--  pretty (SubPropertyOfAPE aps)   = "SubPropertyOf:"    <+> pretty aps

-- instance PrettyM IndividualFrame where
--   pretty (IndividualF i oes) = "Individual:" <+> pretty i
--                              <> line
--                              <> indent 4 (vsep (pretty <$> oes))
-- 
-- instance PrettyM IndividualElement where
--   pretty (AnnotationIE a)     = "Annotations:"  <+> pretty a
--   pretty (TypeIE ds)          = "Types:"        <+> pretty ds
--   pretty (FactIE af)          = "Facts:"        <+> pretty af
--   pretty (SameAsIE ai)        = "SameAs:"       <+> pretty ai
--   pretty (DifferentFromIE ai) = "DifferentFrom" <+> pretty ai

instance PrettyM FactElement where
  pretty = undefined

-- instance PrettyM FactElement where
--   pretty (ObjectPropertyFE opf) = pretty opf 
--   pretty (DataPropertyFE dpf)   = pretty dpf
-- 
-- instance PrettyM ObjectPropertyFact where
--   pretty (ObjectPropertyFact i ind) = pretty i <+> pretty ind
-- 
-- instance PrettyM DataPropertyFact where
--   pretty (DataPropertyFact i l) = pretty i <+> pretty l

--instance PrettyM Misc where
--  pretty (EquivalentClasses as ds)           = "EquivalentClasses:"    <+> pretty as <+> pretty ds
--  pretty (DisjointClasses as ds)             = "DisjointClasses:"      <+> pretty as <+> pretty ds
--  pretty (EquivalentObjectProperties as ope) = "EquivalentProperties:" <+> pretty as <+> pretty ope
--  pretty (DisjointObjectProperties as ope)   = "DisjointProperties:"   <+> pretty as <+> pretty ope
--  pretty (EquivalentDataProperties as dpe)   = "EquivalentProperties:" <+> pretty as <+> pretty dpe
--  pretty (DisjointDataProperties as dpe)     = "DisjointProperties:"   <+> pretty as <+> pretty dpe
--  pretty (SameIndividual as is)              = "SameIndividual:"       <+> pretty as <+> pretty is
--  pretty (DifferentIndividuals as is)        = "DifferentIndividuals:" <+> pretty as <+> pretty is

instance PrettyM a => PrettyM (AtLeast2List a) where
  pretty = pretty . toList

instance PrettyM Entity where
  pretty (EntityDatatype de)           = "Datatype:"           <+> pretty (_unDatatype de)
  pretty (EntityClass ce)              = "Class:"              <+> pretty ce
  pretty (EntityObjectProperty ie)     = "ObjectProperty:"     <+> pretty ie
  pretty (EntityDataProperty de)       = "DataProperty:"       <+> pretty de
  pretty (EntityAnnotationProperty ae) = "AnnotationProperty:" <+> pretty ae
  pretty (EntityIndividual ie)         = "Individual:"         <+> pretty ie

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

prependL :: PrettyM a => T.Text -> [a] -> Doc ann
prependL _ [] = mempty
prependL t xs = pretty t <> pretty xs 

join :: PrettyM a => T.Text -> [a] -> Doc ann
join s xs = concatWith (surround (space <> pretty s <> space)) (pretty <$> xs)

emptyLine :: Doc ann
emptyLine = line <> line

-- | Like `(<+>)`, with better handling of empty representations, in order to avoid 
-- having many spaces (e.g. when you concatenate Docs where are empty
--
(<->) :: Doc ann -> Doc ann -> Doc ann
d1 <-> d2
  | null (show d1) = d2
  | null (show d2) = d1
  | otherwise = d1 <+> d2



