{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.OWL2.Manchester.Pretty where

import           Control.Monad.State               hiding ( join )
import           Data.Data                                ( toConstr )
import qualified Data.List.NonEmpty            as NE
import           Data.Text.Prettyprint.Doc         hiding ( pretty, prettyList )
import qualified Data.Text.Prettyprint.Doc     as PP

import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types


-- | Wrapper typeclass of 'Pretty' typeclass for the Manchester format
class PrettyM a where
  pretty :: a -> Doc ann
  default pretty :: PP.Pretty a => a -> Doc ann
  pretty = PP.pretty
  prettyList :: [a] -> Doc ann
  prettyList = vsep . punctuate "," . fmap pretty

instance PrettyM () where
  pretty = mempty

instance PrettyM a => PrettyM (Maybe a) where
  pretty Nothing  = mempty
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

instance PrettyM a => PrettyM (Annotated a) where
  pretty (Annotated ([], a)) = pretty a
  pretty (Annotated (xs, a)) = prettyWithTitle "Annotations:" xs <> line <> pretty a
          
instance PrettyM ClassExpression where
  pretty (CExpClass iri)                        = pretty iri
  pretty (CExpObjectIntersectionOf ces)         = parens $ concatWith (surround " and ")  (parens . pretty <$> toList ces)
  pretty (CExpObjectUnionOf ces)                = parens $ concatWith (surround " or ") (parens . pretty <$> toList ces)
  pretty (CExpObjectComplementOf ce)            = "not" <+> parens (pretty ce)
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
  pretty (TypedL s dt) = dquotes (pretty s) <> "^^" <> pretty dt

instance PrettyM Datatype where
  pretty (Datatype (AbbreviatedIRI "xsd" "integer")) = "integer"
  pretty (Datatype (AbbreviatedIRI "xsd" "float"))   = "float"
  pretty (Datatype (AbbreviatedIRI "xsd" "decimal")) = "decimal"
  pretty (Datatype (AbbreviatedIRI "xsd" "string"))  = "xsd:string"
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
                                       <> prettyWithTitle "Annotations:" ans
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
  pretty (AtomicClass i)        = pretty i
  pretty (AtomicIndividuals is) = braces . pretty . NE.toList $ is
  pretty (AtomicDescription d)  = parens . pretty $ d

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

instance PrettyM ObjectPropertyChain where
  pretty xs = join "o" (toList . unChain $ xs)

instance PrettyM ObjectOrDataPE where
  pretty (ObjectPE ope) = pretty ope
  pretty (DataPE dpe)   = pretty dpe

instance PrettyM ObjectPropertyCharacteristic where
  pretty FUNCTIONAL         = "Functional"
  pretty INVERSE_FUNCTIONAL = "InverseFunctional"
  pretty REFLEXIVE          = "Reflexive"
  pretty IRREFLEXIVE        = "Irreflexive"
  pretty SYMMETRIC          = "Symmetric"
  pretty ASYMMETRIC         = "Asymmetric"
  pretty TRANSITIVE         = "Transitive"

instance PrettyM DataPropertyCharacteristics where
  pretty FUNCTIONAL_DPE = "Functional"

instance PrettyM Entity where
  pretty (EntityDatatype de)           = "Datatype:"           <+> pretty (_unDatatype de)
  pretty (EntityClass ce)              = "Class:"              <+> pretty ce
  pretty (EntityObjectProperty ie)     = "ObjectProperty:"     <+> pretty ie
  pretty (EntityDataProperty de)       = "DataProperty:"       <+> pretty de
  pretty (EntityAnnotationProperty ae) = "AnnotationProperty:" <+> pretty ae
  pretty (EntityIndividual ie)         = "Individual:"         <+> pretty ie

instance PrettyM Axiom where
  pretty (DeclarationAxiom ans e)                          = pretty e <> parensM (line <> prettyAnns ans)
  pretty (AnnotationAxiomDomain ans _ i)                   = prettyAnns ans <> pretty i
  pretty (AnnotationAxiomRange ans _ i)                    = prettyAnns ans <> pretty i
  pretty (AnnotationAxiomSubProperty ans _ p)              = prettyAnns ans <> pretty p
  pretty (AnnotationAxiomAssertion ans _ (Annotation t v)) = prettyAnns ans <> pretty t <+> pretty v
  pretty (DatatypeAxiomDefinition ans _ r)                 = prettyAnns ans <> pretty r
  pretty (ObjectPropAxiomDomain ans _ e)                   = prettyAnns ans <> pretty e
  pretty (ObjectPropAxiomRange ans _ e)                    = prettyAnns ans <> pretty e
  pretty (ObjectPropAxiomCharacteristics ans _ c)          = prettyAnns ans <> pretty c
  pretty (ObjectPropAxiomSubProperty ans _ o)              = prettyAnns ans <> pretty o
  pretty (ObjectPropAxiomChainSubProperty ans ch _)        = prettyAnns ans <> pretty ch
  pretty (ObjectPropAxiomEquivalent ans _ os)              = prettyAnns ans <> pretty (NE.toList os)
  pretty (ObjectPropAxiomDisjoint  ans _ os)               = prettyAnns ans  <> pretty (NE.toList os)
  pretty (ObjectPropAxiomInverse ans _ o)                  = prettyAnns ans <> pretty o
  pretty (DataPropAxiomDomain ans _ c)                     = prettyAnns ans <> pretty c
  pretty (DataPropAxiomRange ans _ r)                      = prettyAnns ans <> pretty r
  pretty (DataPropAxiomCharacteristics ans _ c)            = prettyAnns ans <> pretty c
  pretty (DataPropAxiomSubProperty ans _ d)                = prettyAnns ans <> pretty d
  pretty (DataPropAxiomEquivalent ans _ ds)                = prettyAnns ans <> pretty (NE.toList ds)
  pretty (DataPropAxiomDisjoint ans _ ds)                  = prettyAnns ans <> pretty (NE.toList ds)
  pretty (ClassAxiomSubClassOf ans _ e)                    = prettyAnns ans <> pretty e
  pretty (ClassAxiomEquivalentClasses ans _ es)            = prettyAnns ans <> pretty (NE.toList es)
  pretty (ClassAxiomDisjointClasses ans _ es)              = prettyAnns ans <> pretty (NE.toList es)
  pretty (ClassAxiomDisjointUnion ans _ es)                = prettyAnns ans <> pretty (toList es)
  pretty (ClassAxiomHasKey ans _ ods)                      = prettyAnns ans <> pretty (NE.toList ods)
  pretty (AssertionAxiomSameIndividuals ans _ is)          = prettyAnns ans <> pretty (NE.toList is)
  pretty (AssertionAxiomDifferentIndividuals ans _ is)     = prettyAnns ans <> pretty (NE.toList is)
  pretty (AssertionAxiomClass ans _ e)                     = parensM (prettyAnns ans <> pretty e)
  pretty (AssertionAxiomObjectProperty ans o _ i)          = prettyAnns ans <> pretty o <+> pretty i
  pretty (AssertionAxiomNegativeObjectProperty ans o _ i)  = prettyAnns ans <> "not" <+> pretty o <+> pretty i
  pretty (AssertionAxiomDataProperty ans d _ l)            = prettyAnns ans <> pretty d <+> pretty l
  pretty (AssertionAxiomNegativeDataProperty ans d _ l)    = prettyAnns ans <> "not" <+> pretty d <+> pretty l
  prettyList                                               = prettyAxiomList


-- | Helper function to preint the "Annotation" section
--
prettyAnns :: Annotations -> Doc ann
prettyAnns [] = mempty
prettyAnns xs = prettyWithTitle "Annotations:" xs <> line

-- | Helper function to print same type of axioms (refering to the same element) as a group 
--
prettyAxiomList :: [Axiom] -> Doc ann
prettyAxiomList []                                            = mempty
prettyAxiomList xs@(DeclarationAxiom{}:_)                     = prettyWithTitle ""                  xs
prettyAxiomList xs@(AssertionAxiomClass{}:_)                  = prettyWithTitle "Types:"            xs
prettyAxiomList xs@(AssertionAxiomDifferentIndividuals{}:_)   = prettyWithTitle "DifferentFrom:"    xs
prettyAxiomList xs@(AssertionAxiomSameIndividuals{}:_)        = prettyWithTitle "SameAs:"           xs
prettyAxiomList xs@(AssertionAxiomObjectProperty{}:_)         = prettyWithTitle "Facts:"            xs
prettyAxiomList xs@(AssertionAxiomNegativeObjectProperty{}:_) = prettyWithTitle "Facts:"            xs
prettyAxiomList xs@(AssertionAxiomDataProperty{}:_)           = prettyWithTitle "Facts:"            xs
prettyAxiomList xs@(AssertionAxiomNegativeDataProperty{}:_)   = prettyWithTitle "Facts:"            xs
prettyAxiomList xs@(AnnotationAxiomAssertion{}:_)             = prettyWithTitle "Annotations:"      xs
prettyAxiomList xs@(AnnotationAxiomDomain{}:_)                = prettyWithTitle "Domain:"           xs
prettyAxiomList xs@(AnnotationAxiomRange{}:_)                 = prettyWithTitle "Range:"            xs
prettyAxiomList xs@(AnnotationAxiomSubProperty{}:_)           = prettyWithTitle "SubPropertyOf:"    xs
prettyAxiomList xs@(DatatypeAxiomDefinition{}:_)              = prettyWithTitle "EquivalentTo:"     xs
prettyAxiomList xs@(ClassAxiomSubClassOf{}:_)                 = prettyWithTitle "SubClassOf:"       xs
prettyAxiomList xs@(ClassAxiomEquivalentClasses{}:_)          = prettyWithTitle "EquivalentTo:"     xs
prettyAxiomList xs@(ClassAxiomDisjointClasses{}:_)            = prettyWithTitle "DisjointWith:"     xs
prettyAxiomList xs@(ClassAxiomDisjointUnion{}:_)              = prettyWithTitle "DisjointUnionOf:"  xs
prettyAxiomList xs@(ClassAxiomHasKey{}:_)                     = prettyWithTitle "HasKey:"           xs
prettyAxiomList xs@(ObjectPropAxiomSubProperty{}:_)           = prettyWithTitle "SubPropertyOf:"    xs
prettyAxiomList xs@(ObjectPropAxiomChainSubProperty{}:_)      = prettyWithTitle "SubPropertyChain:" xs
prettyAxiomList xs@(ObjectPropAxiomCharacteristics{}:_)       = prettyWithTitle "Characteristics:"  xs
prettyAxiomList xs@(ObjectPropAxiomDomain{}:_)                = prettyWithTitle "Domain:"           xs
prettyAxiomList xs@(ObjectPropAxiomRange{}:_)                 = prettyWithTitle "Range:"            xs
prettyAxiomList xs@(ObjectPropAxiomInverse{}:_)               = prettyWithTitle "InverseOf:"        xs
prettyAxiomList xs@(ObjectPropAxiomEquivalent{}:_)            = prettyWithTitle "EquivalentTo:"     xs
prettyAxiomList xs@(ObjectPropAxiomDisjoint{}:_)              = prettyWithTitle "DisjointWith:"     xs
prettyAxiomList xs@(DataPropAxiomEquivalent{}:_)              = prettyWithTitle "EquivalentTo:"     xs
prettyAxiomList xs@(DataPropAxiomDomain{}:_)                  = prettyWithTitle "Domain:"           xs
prettyAxiomList xs@(DataPropAxiomRange{}:_)                   = prettyWithTitle "Range:"            xs
prettyAxiomList xs@(DataPropAxiomSubProperty{}:_)             = prettyWithTitle "SubPropertyOf:"    xs
prettyAxiomList xs@(DataPropAxiomDisjoint{}:_)                = prettyWithTitle "DisjointWith:"     xs
prettyAxiomList xs@(DataPropAxiomCharacteristics{}:_)         = prettyWithTitle "Characteristics:"  xs

prettyWithTitle :: PrettyM a => Doc ann -> [a] -> Doc ann
prettyWithTitle title els = parensM $ vsep $ title : punctuate "," (pretty <$> els)

prettyAxioms :: [Axiom] -> Doc ann
prettyAxioms as = mkTitle "Axioms"
               <> prettyAxiomGroups (groupAxiomsOnIRI as) 

mkTitle :: Doc ann -> Doc ann
mkTitle title = vsep [ emptyLine
                     , "##############################"
                     , "#" <+> title
                     , "##############################"
                     , line
                     ]

prettyAxiomGroups :: [[Axiom]] -> Doc ann
prettyAxiomGroups = vsep . fmap prettyAxiomGroup

prettyAxiomGroup :: [Axiom] -> Doc ann
prettyAxiomGroup xss = prettyFrame decls rst
 where
   (decls, rst) = runState (extractAxioms predicate) xss
   predicate a = axiomType a == DeclarationAxiomType


-- | The first axiom will be either a declaration axiom or nothing.
-- The function supports more than a single declaration axiom (e.g. when a class
-- and a property share the same IRI). In that case all the other axioms that refer
-- to these IRIs will be grouped together
-- When there is no declaration axiom (i.e., the first argument is an empty list)
-- means that all the other axioms are misc (independent) of a specific entity axioms
prettyFrame :: [Axiom] -> [Axiom] -> Doc ann
prettyFrame []    ax = vsep (prettyMisc <$> ax) <> emptyLine
 where
  prettyMisc (ClassAxiomEquivalentClasses ans c es)        = "EquivalentClasses:"    <> parensM (line <> prettyAnns ans <> pretty (c:NE.toList es))
  prettyMisc (ClassAxiomDisjointClasses ans c es)          = "DisjointClasses:"      <> parensM (line <> prettyAnns ans <> pretty (c:NE.toList es))
  prettyMisc (ObjectPropAxiomEquivalent ans o os)          = "EquivalentProperties:" <> parensM (line <> prettyAnns ans <> pretty (o:NE.toList os))
  prettyMisc (ObjectPropAxiomDisjoint  ans o os)           = "DisjointProperties:"   <> parensM (line <> prettyAnns ans <> pretty (o:NE.toList os))
  prettyMisc (DataPropAxiomEquivalent ans d ds)            = "EquivalentProperties:" <> parensM (line <> prettyAnns ans <> pretty (d:NE.toList ds))
  prettyMisc (DataPropAxiomDisjoint ans d ds)              = "DisjointProperties:"   <> parensM (line <> prettyAnns ans <> pretty (d:NE.toList ds))
  prettyMisc (AssertionAxiomSameIndividuals ans a is)      = "SameIndividual:"       <> parensM (line <> prettyAnns ans <> pretty (a:NE.toList is))
  prettyMisc (AssertionAxiomDifferentIndividuals ans a is) = "DifferentIndividuals:" <> parensM (line <> prettyAnns ans <> pretty (a:NE.toList is))
  prettyMisc other = error $ "Unsupported misc type: '" <> show (toConstr other) <> "'"
prettyFrame dclrs ax = vsep (pretty <$> dclrs) -- vsep implicitly inserts a newline at the end
                       <> parensM (vsep (pretty <$> groupAxiomsOnConstructor ax))
                       <> emptyLine


-----------------------
-- Utility functions --
-----------------------

-- | Indent all but first line by 4 spaces
--
parensM :: Doc ann -> Doc ann
parensM = nest 4

-- | Concatenate Pretty elements using text between them
--
join :: PrettyM a => T.Text -> [a] -> Doc ann
join s xs = concatWith (surround (space <> pretty s <> space)) (pretty <$> xs)

-- | Prints an empty line
--
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
