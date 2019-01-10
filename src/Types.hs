{-# LANGUAGE DeriveFunctor #-}

module Types where

import           Data.List                                ( intercalate )
import           Data.List.NonEmpty                       ( NonEmpty(..) )

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


type LangTag = String
type ImportIRI = IRI
type AnnotationPropertyIRI = IRI
type VersionIRI = IRI
type OntologyIRI = IRI
-- type FullIRI = IRI
type DatatypeIRI = IRI
type ClassIRI = IRI
type ObjectPropertyIRI = IRI
type DataPropertyIRI = IRI
type IndividualIRI = IRI
--type Frame = String
type PrefixName = String
type Annotations = AnnotatedList Annotation
type Descriptions = AnnotatedList Description
type Exponent = Integer
type Description = NonEmpty Conjunction
type Fact = WithNegation FactElement
type DataPropertyExpression = DataPropertyIRI
type ObjectPropertyExpression = WithInversion ObjectPropertyIRI
type DataPrimary = WithNegation DataAtomic

newtype DataRange = DataRange (NonEmpty DataConjunction) deriving (Show)
newtype DataConjunction = DataConjunction (NonEmpty DataPrimary) deriving (Show)
newtype DecimalLiteral = DecimalL Double deriving (Show) -- PP
newtype IntegerLiteral = IntegerL Integer deriving (Show) -- PP
newtype NodeID = NodeID String deriving (Show) -- PP
newtype AnnotatedList a = AnnList (NonEmpty (Maybe Annotations, a)) deriving (Show) -- PP
newtype ImportDeclaration = ImportD IRI deriving (Show) -- PP

data IRI = FullIRI String
         | AbbreviatedIRI PrefixName String
         | SimpleIRI String deriving (Show) -- PP
data TypedLiteral = TypedL String Datatype deriving (Show) -- PP
data FloatPoint = FloatP Double (Maybe Exponent) deriving (Show) -- PP
data LiteralWithLang = LiteralWithLang String LangTag deriving (Show) -- PP
data OntologyDocument = OntologyD [PrefixDeclaration] Ontology deriving (Show)
data PrefixDeclaration = PrefixD PrefixName IRI deriving (Show) -- PP
data Ontology = Ontology (Maybe OntologyVersionIRI) [ImportDeclaration] [AnnotatedList Annotation] [Frame] deriving (Show)
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving (Show) -- PP
data Annotation = Annotation AnnotationPropertyIRI AnnotationTarget deriving (Show) -- PP
data Frame = FrameDT DatatypeFrame
           | FrameC ClassFrame
           | FrameOP ObjectPropertyFrame
           | FrameDP DataPropertyFrame
           | FrameAP AnnotationPropertyFrame
           | FrameI IndividualFrame
           | FrameM Misc deriving (Show)
data DatatypeFrame = DatatypeF Datatype (Maybe Annotations) (Maybe AnnotDataRange) deriving (Show) -- PP
data AnnotDataRange = AnnotDataRange Annotations DataRange deriving (Show) -- PP
data Datatype = IriDT IRI
              | IntegerDT
              | DecimalDT
              | FloatDT
              | StringDT deriving (Show) -- PP
data DataAtomic = DatatypeDA Datatype
                | LiteralListDA (NonEmpty Literal)
                | DatatypeRestrictionDA DatatypeRestriction
                | DataRangeDA DataRange deriving (Show)
data DatatypeRestriction = DatatypeRestriction Datatype (NonEmpty RestrictionExp) deriving (Show)
data RestrictionExp = RestrictionExp Facet Literal deriving (Show)
data Facet = LENGTH_FACET
           | MIN_LENGTH_FACET
           | MAX_LENGTH_FACET
           | PATTERN_FACET
           | LANG_RANGE_FACET
           | LE_FACET
           | L_FACET
           | GE_FACET
           | G_FACET deriving (Show)
data ClassFrame = ClassF IRI [ClassElement] deriving (Show)
data ClassElement = AnnotationCE Annotations
                  | SubClassOfCE Descriptions
                  | EquivalentToCE Descriptions
                  | DisjointToCE Descriptions
                  | DisjointUnionOfCE (Maybe Annotations) (AtLeast2List Description)
                  | HasKeyCE (Maybe Annotations) (NonEmpty ObjectOrDataPE) deriving (Show)
data ObjectOrDataPE = ObjectPE ObjectPropertyExpression
                    | DataPE DataPropertyExpression deriving (Show)
data WithNegation a = Positive a | Negative a deriving (Show, Functor) -- PP
data WithInversion a = Plain a | Inverse a deriving (Show, Functor) -- PP
data Conjunction = ClassConj IRI (NonEmpty (WithNegation Restriction)) | PrimConj (NonEmpty Primary) deriving (Show)
data Primary = PrimaryR (WithNegation Restriction) | PrimaryA (WithNegation Atomic) deriving (Show)
data Restriction = OPRestriction ObjectPropertyRestriction | DPRestriction DataPropertyRestriction deriving (Show)
data ObjectPropertyRestrictionType = SelfOPR
                                   | SomeOPR Primary
                                   | OnlyOPR Primary
                                   | ValueOPR Individual
                                   | MinOPR Int (Maybe Primary) -- TODO: Int -> Nat
                                   | MaxOPR Int (Maybe Primary) -- TODO: Int -> Nat
                                   | ExactlyOPR Int (Maybe Primary) deriving (Show) -- TODO: Int -> Nat
data DataPropertyRestrictionType = SomeDPR DataPrimary
                                 | OnlyDPR DataPrimary
                                 | ValueDPR Literal
                                 | MinDPR Int (Maybe DataPrimary) -- TODO: Int -> Nat
                                 | MaxDPR Int (Maybe DataPrimary) -- TODO: Int -> Nat
                                 | ExactlyDPR Int (Maybe DataPrimary) deriving (Show) -- TODO: Int -> Nat -- PP
data ObjectPropertyRestriction = OPR ObjectPropertyExpression ObjectPropertyRestrictionType deriving (Show)
data DataPropertyRestriction = DPR DataPropertyExpression DataPropertyRestrictionType deriving (Show)
data Individual = IRIIndividual IndividualIRI | NodeIndividual NodeID deriving (Show)
data Atomic = AtomicClass ClassIRI | AtomicIndividuals (NonEmpty Individual) | AtomicDescription Description deriving (Show)
data ObjectPropertyFrame = ObjectPropertyF ObjectPropertyIRI [ObjectPropertyElement] deriving (Show)
data ObjectPropertyElement = AnnotationOPE Annotations
                           | DomainOPE Descriptions
                           | RangeOPE Descriptions
                           | CharacteristicsOPE (AnnotatedList ObjectPropertyCharacteristics)
                           | SubPropertyOfOPE (AnnotatedList ObjectPropertyExpression)
                           | EquivalentToOPE (AnnotatedList ObjectPropertyExpression)
                           | DisjointWithOPE (AnnotatedList ObjectPropertyExpression)
                           | InverseOfOPE (AnnotatedList ObjectPropertyExpression)
                           | SubPropertyChainOPE Annotations (AtLeast2List ObjectPropertyExpression) deriving (Show)
data ObjectPropertyCharacteristics = FUNCTIONAL
                                   | INVERSE_FUNCTIONAL
                                   | REFLEXIVE
                                   | IRREFLEXIVE
                                   | SYMMETRIC
                                   | ASYMMETRIC
                                   | TRANSITIVE deriving (Show)
data DataPropertyFrame = DataPropertyF DataPropertyIRI [DataPropertyElement] deriving (Show)
data DataPropertyElement = AnnotationDPE Annotations
                         | DomainDPE Descriptions
                         | RangeDPE (AnnotatedList DataRange)
                         | CharacteristicsDPE (AnnotatedList DataPropertyCharacteristics)
                         | SubPropertyOfDPE (AnnotatedList DataPropertyExpression)
                         | EquivalentToDPE (AnnotatedList DataPropertyExpression)
                         | DisjointWithDPE (AnnotatedList DataPropertyExpression) deriving (Show)
data DataPropertyCharacteristics = FUNCTIONAL_DPE deriving (Show)
data AnnotationPropertyFrame = AnnotationPropertyF AnnotationPropertyIRI [AnnotationPropertyElement] deriving (Show)
data AnnotationPropertyElement = AnnotationAPE Annotations
                               | DomainAPE (AnnotatedList IRI)
                               | RangeAPE (AnnotatedList IRI)
                               | SubPropertyOfAPE (AnnotatedList AnnotationPropertyIRI) deriving (Show)
data IndividualFrame = IndividualF Individual [IndividualElement] deriving (Show)
data IndividualElement = AnnotationIE Annotations
                       | TypeIE Descriptions
                       | FactIE (AnnotatedList Fact)
                       | SameAsIE (AnnotatedList Individual)
                       | DifferentFromIE (AnnotatedList Individual) deriving (Show)
data FactElement = ObjectPropertyFE ObjectPropertyFact | DataPropertyFE DataPropertyFact deriving (Show)
data ObjectPropertyFact = ObjectPropertyFact ObjectPropertyIRI Individual deriving (Show)
data DataPropertyFact = DataPropertyFact DataPropertyIRI Literal deriving (Show)
data Misc = EquivalentClasses (AnnotatedList (AtLeast2List Description))
          | DisjointClasses (AnnotatedList (AtLeast2List Description))
          | EquivalentObjectProperties (AnnotatedList (AtLeast2List ObjectPropertyExpression))
          | DisjointObjectProperties (AnnotatedList (AtLeast2List ObjectPropertyExpression))
          | EquivalentDataProperties (AnnotatedList (AtLeast2List DataPropertyExpression))
          | DisjointDataProperties (AnnotatedList (AtLeast2List DataPropertyExpression))
          | SameIndividual (AnnotatedList (AtLeast2List Individual))
          | DifferentIndividual (AnnotatedList (AtLeast2List Individual)) deriving (Show)
data Literal = TypedLiteralC TypedLiteral
             | StringLiteralNoLang String
             | StringLiteralLang LiteralWithLang
             | IntegerLiteralC IntegerLiteral
             | DecimalLiteralC DecimalLiteral
             | FloatingLiteralC FloatPoint deriving (Show) -- PP
data Entity = DatatypeEntity Datatype
            | ClassEntity ClassIRI
            | ObjectPropertyEntity ObjectPropertyIRI
            | DataPropertyEntity DataPropertyIRI
            | AnnotationPropertyEntity AnnotationPropertyIRI
            | IndividualEntity IndividualIRI deriving (Show)
data AnnotationTarget = NodeAT NodeID
                      | IriAT IRI
                      | LiteralAT Literal deriving (Show) -- PP
-- data DataPrimary = DataPr DataAtomic | DataPrNot DataAtomic


---------------------------
---- UTILITY FUNCTIONS ----
---------------------------
flattenAnnList :: [AnnotatedList a] -> Maybe (AnnotatedList a)
flattenAnnList [] = Nothing
flattenAnnList xs = Just $ foldl1 (<>) xs

-------------------------
---- CLASS INSTANCES ----
-------------------------

instance Semigroup (AnnotatedList a) where
  (AnnList xs) <> (AnnList ys) = AnnList (xs <> ys)  

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

