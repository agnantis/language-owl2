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

type LangTag = Text
type ImportIRI = IRI
type AnnotationPropertyIRI = TotalIRI
type VersionIRI = IRI
type OntologyIRI = IRI
-- type FullIRI = IRI
type DatatypeIRI = IRI
type ClassIRI = IRI
type ObjectPropertyIRI = IRI
type DataPropertyIRI = IRI
type IndividualIRI = IRI
--type Frame = Text
type PrefixName = Text
type SomeAnnotations = AnnotatedList Annotation
type Descriptions = AnnotatedList Description
type Exponent = Integer
type Fact = WithNegation FactElement
type DataPropertyExpression = DataPropertyIRI
data ObjectPropertyExpression
    = OPE ObjectPropertyIRI
    | InverseOPE ObjectPropertyIRI deriving (Show)
newtype DataPrimary = DataPrimary (WithNegation DataAtomic) deriving (Show)
type Annotations = [Annotated Annotation]

-------
data DataRange'
    = DatatypeDR Datatype
    | IntersectionDR (AtLeast2List DataRange')
    | UnionDR (AtLeast2List DataRange')
    | ComplementDR DataRange'
    | OneOfDR (NonEmpty Literal)
    | RestrictionDR DatatypeRestriction deriving (Show)
-------
newtype DataRange = DataRange (NonEmpty DataConjunction) deriving (Show)
newtype DataConjunction = DataConjunction (NonEmpty DataPrimary) deriving (Show)
newtype DecimalLiteral = DecimalL Double deriving (Show)
newtype Description = Description (NonEmpty Conjunction) deriving (Show)
newtype IntegerLiteral = IntegerL Integer deriving (Show)
newtype NodeID = NodeID Text deriving (Show)
newtype Annotated a = Annotated { unAnnotated :: ([Annotated Annotation], a) } deriving (Show)
type AnnotatedList a = NonEmpty (Annotated a)
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
data Ontology = Ontology (Maybe OntologyVersionIRI) [ImportDeclaration] [Annotated Annotation] [Frame] deriving (Show)
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving (Show)
data Annotation = Annotation AnnotationPropertyIRI AnnotationTarget deriving (Show)
data Frame
    = FrameDT DatatypeFrame
    | FrameC ClassFrame
    | FrameOP ObjectPropertyFrame
    | FrameDP DataPropertyFrame
    | FrameAP AnnotationPropertyFrame
    | FrameI IndividualFrame
    | FrameM Misc deriving (Show)
data DatatypeFrame = DatatypeF Datatype [SomeAnnotations] (Maybe AnnotDataRange) deriving (Show)
data AnnotDataRange = AnnotDataRange Annotations DataRange' deriving (Show)
newtype Datatype = Datatype { unDatatype :: DatatypeIRI } deriving (Show)
-- newtype Class = Class { unClass :: ClassIRI } deriving (Show)
-- newtype ObjectProperty = ObjectProperty{ unObjectProperty :: ObjectPropertyIRI } deriving (Show)
-- newtype DataProperty = DataProperty{ unDataProperty :: DataPropertyIRI } deriving (Show)
data DataAtomic
    = DatatypeDA Datatype
    | LiteralListDA (NonEmpty Literal)
    | DatatypeRestrictionDA DatatypeRestriction
    | DataRangeDA DataRange deriving (Show)
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
data ClassFrame = ClassF IRI [ClassElement] deriving (Show)
data ClassElement
    = AnnotationCE SomeAnnotations
    | SubClassOfCE Descriptions
    | EquivalentToCE Descriptions
    | DisjointToCE Descriptions
    | DisjointUnionOfCE Annotations (AtLeast2List Description)
    | HasKeyCE Annotations (NonEmpty ObjectOrDataPE) deriving (Show)
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
data ObjectPropertyRestrictionType
    = SelfOPR
    | SomeOPR Primary
    | OnlyOPR Primary
    | ValueOPR Individual
    | MinOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | MaxOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | ExactlyOPR Int (Maybe Primary) deriving (Show) -- TODO: Int -> Nat
data DataPropertyRestrictionType
    = SomeDPR DataRange'
    | OnlyDPR DataRange'
    | ValueDPR Literal
    | MinDPR Int (Maybe DataRange') -- TODO: Int -> Nat
    | MaxDPR Int (Maybe DataRange') -- TODO: Int -> Nat
    | ExactlyDPR Int (Maybe DataRange') deriving (Show) -- TODO: Int -> Nat
data ObjectPropertyRestriction = OPR ObjectPropertyExpression ObjectPropertyRestrictionType deriving (Show)
data DataPropertyRestriction = DPR DataPropertyExpression DataPropertyRestrictionType deriving (Show)
data TotalIRI
    = NamedIRI IRI
    | AnonymousIRI NodeID deriving (Show)
type Individual = TotalIRI
data Atomic
    = AtomicClass ClassIRI
    | AtomicIndividuals (NonEmpty Individual)
    | AtomicDescription Description deriving (Show)
data ObjectPropertyFrame = ObjectPropertyF ObjectPropertyIRI [ObjectPropertyElement] deriving (Show)
data ObjectPropertyElement
    = AnnotationOPE SomeAnnotations
    | DomainOPE Descriptions
    | RangeOPE Descriptions
    | CharacteristicsOPE (AnnotatedList ObjectPropertyCharacteristics)
    | SubPropertyOfOPE (AnnotatedList ObjectPropertyExpression)
    | EquivalentToOPE (AnnotatedList ObjectPropertyExpression)
    | DisjointWithOPE (AnnotatedList ObjectPropertyExpression)
    | InverseOfOPE (AnnotatedList ObjectPropertyExpression)
    | SubPropertyChainOPE Annotations (AtLeast2List ObjectPropertyExpression) deriving (Show)
data ObjectPropertyCharacteristics
    = FUNCTIONAL
    | INVERSE_FUNCTIONAL
    | REFLEXIVE
    | IRREFLEXIVE
    | SYMMETRIC
    | ASYMMETRIC
    | TRANSITIVE deriving (Show)
data DataPropertyFrame = DataPropertyF DataPropertyIRI [DataPropertyElement] deriving (Show)
data DataPropertyElement
    = AnnotationDPE SomeAnnotations
    | DomainDPE Descriptions
    | RangeDPE (AnnotatedList DataRange')
    | CharacteristicsDPE (AnnotatedList DataPropertyCharacteristics)
    | SubPropertyOfDPE (AnnotatedList DataPropertyExpression)
    | EquivalentToDPE (AnnotatedList DataPropertyExpression)
    | DisjointWithDPE (AnnotatedList DataPropertyExpression) deriving (Show)
data DataPropertyCharacteristics = FUNCTIONAL_DPE deriving (Show)
data AnnotationPropertyFrame = AnnotationPropertyF AnnotationPropertyIRI [AnnotationPropertyElement] deriving (Show)
data AnnotationPropertyElement
    = AnnotationAPE SomeAnnotations
    | DomainAPE (AnnotatedList IRI)
    | RangeAPE (AnnotatedList IRI)
    | SubPropertyOfAPE (AnnotatedList AnnotationPropertyIRI) deriving (Show)
data IndividualFrame = IndividualF Individual [IndividualElement] deriving (Show)
data IndividualElement
    = AnnotationIE SomeAnnotations
    | TypeIE Descriptions
    | FactIE (AnnotatedList Fact)
    | SameAsIE (AnnotatedList Individual)
    | DifferentFromIE (AnnotatedList Individual) deriving (Show)
data FactElement
    = ObjectPropertyFE ObjectPropertyFact
    | DataPropertyFE DataPropertyFact deriving (Show)
data ObjectPropertyFact = ObjectPropertyFact ObjectPropertyIRI Individual deriving (Show)
data DataPropertyFact = DataPropertyFact DataPropertyIRI Literal deriving (Show)
data Misc
    = EquivalentClasses Annotations (AtLeast2List Description)
    | DisjointClasses Annotations (AtLeast2List Description)
    | EquivalentObjectProperties Annotations (AtLeast2List ObjectPropertyExpression)
    | DisjointObjectProperties Annotations (AtLeast2List ObjectPropertyExpression)
    | EquivalentDataProperties Annotations (AtLeast2List DataPropertyExpression)
    | DisjointDataProperties Annotations (AtLeast2List DataPropertyExpression)
    | SameIndividual Annotations (AtLeast2List Individual)
    | DifferentIndividuals Annotations (AtLeast2List Individual) deriving (Show)
data Literal
    = TypedLiteralC TypedLiteral
    | StringLiteralNoLang Text
    | StringLiteralLang LiteralWithLang
    | IntegerLiteralC IntegerLiteral
    | DecimalLiteralC DecimalLiteral
    | FloatingLiteralC FloatPoint deriving (Show)
data Entity
    = EntityDatatype Datatype
    | EntityClass ClassIRI
    | EntityObjectProperty ObjectPropertyIRI
    | EntityDataProperty DataPropertyIRI
    | EntityAnnotationProperty AnnotationPropertyIRI
    | EntityIndividual IndividualIRI deriving (Show)
data AnnotationTarget
    = NodeAT NodeID
    | IriAT IRI
    | LiteralAT Literal deriving (Show)
-- data DataPrimary = DataPr DataAtomic | DataPrNot DataAtomic


---------------------------
---- UTILITY FUNCTIONS ----
---------------------------
flattenAnnList :: [AnnotatedList a] -> Maybe (AnnotatedList a)
flattenAnnList [] = Nothing
flattenAnnList xs = Just $ foldl1 (<>) xs

singleton :: a -> NonEmpty a
singleton x = x :| []

-------------------------
---- CLASS INSTANCES ----
-------------------------

-- instance Semigroup (AnnotatedList a) where
--   (AnnList xs) <> (AnnList ys) = AnnList (xs <> ys)  

-- data AtLeast2List a = (a, a) :# [a] deriving (Eq, Ord, Read, Functor)
-- instance Show a => Show (AtLeast2List a) where
--   show ((a, b) :# xs) = show $ a:b:xs 


-- instance Show Annotation where
--   show (Annotation i s) = unwords [show i, "<undefinded-annotation-target>"]

-- instance (Show a) => Show (AnnotatedList a) where
--   show (AnnList []) = ""
--   show (AnnList xs) = intercalate ",\n" (go <$> xs)
--    where
--     go (AnnList [], x) = show x
--     go (al, x) = unwords ["Annotations:", show al, "\n ", show x]

-- instance Show FloatPoint where
--   show (FloatP n me) = concat [show n, maybe "" show me]
-- instance Show Exponent where
--   show (Exponent i) = concat ["e", if i > 0 then "+" else "", show i]

