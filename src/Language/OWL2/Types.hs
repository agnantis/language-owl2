{-# LANGUAGE DeriveFunctor #-}


module Language.OWL2.Types where

import           Data.List.NonEmpty                       ( NonEmpty(..) )
import           Language.OWL2.Import                     ( Text )

---------------
---- TYPES ----
---------------

-- TODO: Should I include a NonEmpty? I do not think so
data AtLeast2List a = (a, a) :# [a] deriving (Eq, Ord, Show, Read, Functor)

atLeast2List :: a -> a -> [a] -> AtLeast2List a
atLeast2List x y = (:#) (x, y)

atLeast2List' :: a -> NonEmpty a -> AtLeast2List a
atLeast2List' x nx = let (x' :| xs) = nx in (x, x') :# xs

toList :: AtLeast2List a -> [a]
toList ~((x, y) :# xs) = x : y : xs

toNonEmptyList :: AtLeast2List a -> NonEmpty a
toNonEmptyList ~((x, y) :# xs) = x :| (y : xs)

-- annListToList :: AnnotatedList a -> [Annotated a]
-- annListToList (AnnList xs) = NE.toList xs

-- Type synonyms --
type LangTag = Text
type ImportIRI = IRI
type AnnotationPropertyIRI = IRI
type VersionIRI = IRI
type OntologyIRI = IRI
type DatatypeIRI = IRI
type ClassIRI = IRI
type ObjectPropertyIRI = IRI
type DataPropertyIRI = IRI
type IndividualIRI = IRI
type Individual = TotalIRI
type PrefixName = Text
type SomeAnnotations = AnnotatedList Annotation
type Descriptions = AnnotatedList ClassExpression
type Exponent = Integer
type Fact = WithNegation FactElement
type DataPropertyExpression = DataPropertyIRI
type AnnotationProperty = AnnotationPropertyIRI
type Annotations = [Annotated Annotation]
--type Description = ClassExpression
type AnnotatedList a = NonEmpty (Annotated a)

-- Data types --
data ObjectPropertyExpression
    = OPE ObjectPropertyIRI
    | InverseOPE ObjectPropertyIRI deriving (Show)
data DataRange
    = DatatypeDR Datatype
    | IntersectionDR (AtLeast2List DataRange)
    | UnionDR (AtLeast2List DataRange)
    | ComplementDR DataRange
    | OneOfDR (NonEmpty Literal)
    | RestrictionDR DatatypeRestriction deriving (Show)
newtype DecimalLiteral = DecimalL Double deriving (Show)
newtype IntegerLiteral = IntegerL Integer deriving (Show)
newtype NodeID = NodeID Text deriving (Show)
newtype Annotated a = Annotated { unAnnotated :: ([Annotated Annotation], a) } deriving (Show) -- TODO: use a sum type instead of pair for easier access
newtype ImportDeclaration = ImportD IRI deriving (Show)
data IRI
    = FullIRI Text
    | AbbreviatedIRI PrefixName Text
    | SimpleIRI Text deriving (Show)
data TypedLiteral = TypedL Text Datatype deriving (Show)
data FloatPoint = FloatP Double (Maybe Exponent) deriving (Show)
data LiteralWithLang = LiteralWithLang Text LangTag deriving (Show)
data OntologyDocument = OntologyD [PrefixDeclaration] Ontology deriving (Show)
data PrefixDeclaration = PrefixD PrefixName IRI deriving (Show)
data Ontology = Ontology (Maybe OntologyVersionIRI) [ImportDeclaration] Annotations [Axiom] deriving (Show)
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving (Show)
data Annotation = Annotation AnnotationPropertyIRI AnnotationValue deriving (Show)
data Axiom
    = AxiomDT DatatypeAxiom
    | AxiomC ClassAxiom
    | AxiomOP ObjectPropertyAxiom
    | AxiomDP DataPropertyAxiom
    | AxiomAP AnnotationPropertyAxiom
    | AxiomI AssertionAxiom deriving (Show)
data DatatypeAxiom = DatatypeF Datatype [SomeAnnotations] (Maybe AnnotDataRange) deriving (Show)
data AnnotDataRange = AnnotDataRange Annotations DataRange deriving (Show)
newtype Datatype = Datatype { unDatatype :: DatatypeIRI } deriving (Show)
data DatatypeRestriction = DatatypeRestriction Datatype (NonEmpty RestrictionExp) deriving (Show)
data RestrictionExp = RestrictionExp Facet Literal deriving (Show)
data Facet
    = LENGTH_FACET
    | MIN_LENGTH_FACET
    | MAX_LENGTH_FACET
    | PATTERN_FACET
    | LANG_RANGE_FACET
    | LE_FACET
    | L_FACET
    | GE_FACET
    | G_FACET deriving (Show)
data ClassAxiom
    = ClassAxiomAnnotation Annotations ClassExpression Annotation -- TODO: I may have to move it from here as these axiom are included in all *Axioms*
    | ClassAxiomSubClassOf Annotations ClassExpression ClassExpression
    | ClassAxiomEquivalentClasses Annotations (AtLeast2List ClassExpression)
    | ClassAxiomDisjointClasses Annotations (AtLeast2List ClassExpression)
    | ClassAxiomDisjointUnion Annotations ClassIRI (AtLeast2List ClassExpression)
    | ClassAxiomHasKey Annotations ClassExpression (NonEmpty ObjectOrDataPE) deriving (Show)
data DeclarationAxiom = DeclarationAxiom Annotations Entity deriving (Show)
data ObjectOrDataPE
    = ObjectPE ObjectPropertyExpression
    | DataPE DataPropertyExpression deriving (Show)
data WithNegation a
    = Positive a
    | Negative a deriving (Show, Functor)
data WithInversion a
    = Plain a
    | Inverse a deriving (Show, Functor)
data Conjunction
    = ClassConj IRI (NonEmpty (WithNegation Restriction))
    | PrimConj (NonEmpty Primary) deriving (Show)
data Primary
    = PrimaryR (WithNegation Restriction)
    | PrimaryA (WithNegation Atomic) deriving (Show)
data Restriction
    = OPRestriction ObjectPropertyRestriction
    | DPRestriction DataPropertyRestriction deriving (Show)
data ClassExpression
    = CExpClass ClassIRI
    | CExpObjectIntersectionOf (AtLeast2List ClassExpression)
    | CExpObjectUnionOf (AtLeast2List ClassExpression)
    | CExpObjectComplementOf ClassExpression
    | CExpObjectOneOf (NonEmpty Individual)
    | CExpObjectSomeValuesFrom ObjectPropertyExpression ClassExpression
    | CExpObjectAllValuesFrom ObjectPropertyExpression ClassExpression
    | CExpObjectHasValue ObjectPropertyExpression Individual
    | CExpObjectHasSelf ObjectPropertyExpression
    | CExpObjectMinCardinality Int ObjectPropertyExpression (Maybe ClassExpression)
    | CExpObjectMaxCardinality Int ObjectPropertyExpression (Maybe ClassExpression)
    | CExpObjectExactCardinality Int ObjectPropertyExpression (Maybe ClassExpression)
    | CExpDataSomeValuesFrom DataPropertyExpression DataRange 
    | CExpDataAllValuesFrom DataPropertyExpression DataRange
    | CExpDataHasValue DataPropertyExpression Literal
    | CExpDataMinCardinality Int DataPropertyExpression (Maybe DataRange)
    | CExpDataMaxCardinality Int DataPropertyExpression (Maybe DataRange)
    | CExpDataExactCardinality Int DataPropertyExpression (Maybe DataRange) deriving (Show)
data ObjectPropertyRestrictionType
    = SelfOPR
    | SomeOPR Primary
    | OnlyOPR Primary
    | ValueOPR Individual
    | MinOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | MaxOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | ExactlyOPR Int (Maybe Primary) deriving (Show) -- TODO: Int -> Nat
data DataPropertyRestrictionType
    = SomeDPR DataRange
    | OnlyDPR DataRange
    | ValueDPR Literal
    | MinDPR Int (Maybe DataRange) -- TODO: Int -> Nat
    | MaxDPR Int (Maybe DataRange) -- TODO: Int -> Nat
    | ExactlyDPR Int (Maybe DataRange) deriving (Show) -- TODO: Int -> Nat
data ObjectPropertyRestriction = OPR ObjectPropertyExpression ObjectPropertyRestrictionType deriving (Show)
data DataPropertyRestriction = DPR DataPropertyExpression DataPropertyRestrictionType deriving (Show)
data TotalIRI
    = NamedIRI IRI
    | AnonymousIRI NodeID deriving (Show)
data Atomic
    = AtomicClass ClassIRI
    | AtomicIndividuals (NonEmpty Individual)
    | AtomicDescription ClassExpression deriving (Show)
data ObjectPropertyAxiom
    = ObjectPAnnotation Annotations ObjectPropertyExpression Annotation
    | ObjectPDomain Annotations ObjectPropertyExpression ClassExpression
    | ObjectPRange Annotations ObjectPropertyExpression ClassExpression
    | ObjectPCharacteristics Annotations ObjectPropertyExpression ObjectPropertyCharacteristic
    | ObjectPSubProperty Annotations ObjectPropertyExpression ObjectPropertyExpression
    | ObjectPChainSubProperty Annotations ObjectPropertyChain ObjectPropertyExpression
    | ObjectPEquivalent Annotations (AtLeast2List ObjectPropertyExpression)
    | ObjectPDisjoint Annotations (AtLeast2List ObjectPropertyExpression)
    | ObjectPInverse Annotations ObjectPropertyExpression ObjectPropertyExpression deriving (Show)
newtype ObjectPropertyChain = ObjectPropertyChain { unChain :: AtLeast2List ObjectPropertyExpression } deriving (Show)
data ObjectPropertyCharacteristic
    = FUNCTIONAL
    | INVERSE_FUNCTIONAL
    | REFLEXIVE
    | IRREFLEXIVE
    | SYMMETRIC
    | ASYMMETRIC
    | TRANSITIVE deriving (Show)
data DataPropertyAxiom
    = DataPAnnotation Annotations DataPropertyExpression Annotation
    | DataPDomain Annotations DataPropertyExpression ClassExpression
    | DataPRange Annotations DataPropertyExpression DataRange
    | DataPCharacteristics Annotations DataPropertyExpression DataPropertyCharacteristics
    | DataPSubProperty Annotations DataPropertyExpression DataPropertyExpression
    | DataPEquivalent Annotations (AtLeast2List DataPropertyExpression)
    | DataPDisjoint Annotations (AtLeast2List DataPropertyExpression) deriving (Show)
data DataPropertyCharacteristics = FUNCTIONAL_DPE deriving (Show)
data AnnotationPropertyAxiom -- TODO: Missing AnnotationAssertion; to be moved under Assertion datatype
    = AnnotationPAnnotation Annotations AnnotationProperty Annotation
    | AnnotationPDomain Annotations AnnotationProperty IRI
    | AnnotationPRange Annotations AnnotationProperty IRI
    | AnnotationPSubProperty Annotations AnnotationProperty AnnotationProperty deriving (Show)
data AssertionAxiom
    = AssertionAnnotation Annotations TotalIRI Annotation
    | AssertionSameIndividuals Annotations (AtLeast2List Individual)
    | AssertionDifferentIndividuals Annotations (AtLeast2List Individual)
    | AssertionClass Annotations Individual ClassExpression
    | AssertionObjectProperty Annotations ObjectPropertyExpression Individual Individual
    | AssertionNegativeObjectProperty Annotations ObjectPropertyExpression Individual Individual 
    | AssertionDataProperty Annotations DataPropertyExpression Individual Literal
    | AssertionNegativeDataProperty Annotations DataPropertyExpression Individual Literal deriving (Show)
data FactElement
    = ObjectPropertyFact ObjectPropertyIRI Individual
    | NegativeObjectPropertyFact ObjectPropertyIRI Individual
    | DataPropertyFact DataPropertyIRI Literal
    | NegativeDataPropertyFact DataPropertyIRI Literal deriving (Show)
data Literal
    = TypedLiteralC TypedLiteral
    | StringLiteralNoLang Text
    | StringLiteralLang LiteralWithLang
    | IntegerLiteralC IntegerLiteral
    | DecimalLiteralC DecimalLiteral
    | FloatingLiteralC FloatPoint deriving (Show)
data Declaration = Declaration Annotations Entity deriving (Show)
data Entity
    = EntityDatatype Datatype
    | EntityClass ClassIRI
    | EntityObjectProperty ObjectPropertyIRI
    | EntityDataProperty DataPropertyIRI
    | EntityAnnotationProperty AnnotationPropertyIRI
    | EntityIndividual IndividualIRI deriving (Show)
data AnnotationValue
    = NodeAT NodeID
    | IriAT IRI
    | LiteralAT Literal deriving (Show)


---------------------------
---- utility functions ----
---------------------------
flattenAnnList :: [AnnotatedList a] -> Maybe (AnnotatedList a)
flattenAnnList [] = Nothing
flattenAnnList xs = Just $ foldl1 (<>) xs

singleton :: a -> NonEmpty a
singleton x = x :| []

-------------------------
---- Class instances ----
-------------------------

-- instance Semigroup (AnnotatedList a) where
--   (AnnList xs) <> (AnnList ys) = AnnList (xs <> ys)  

-- data AtLeast2List a = (a, a) :# [a] deriving (Eq, Ord, Read, Functor)
-- instance Show a => Show (AtLeast2List a) where
--   show ((a, b) :# xs) = show $ a:b:xs 


