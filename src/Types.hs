{-# LANGUAGE DeriveFunctor #-}

module Types where

import           Data.List                                ( intercalate )
import           Data.List.NonEmpty                       ( NonEmpty(..) )

---------------
---- TYPES ----
---------------

-- TODO: Should I include a NonEmpty? I do not think so
data AtLeast2List a = (a, a) :# [a] deriving ( Eq, Ord, Show, Read, Functor)


atLeast2List :: a -> a -> [a] -> AtLeast2List a
atLeast2List x y = (:#) (x, y)

toList :: AtLeast2List a -> [a]
toList ~((x, y) :# xs) = x : y : xs

toNonEmptyList :: AtLeast2List a -> NonEmpty a
toNonEmptyList ~((x, y) :# xs) = x :| (y : xs)


type LangTag = String
type IRI = String
type ImportIRI = IRI
type AnnotationPropertyIRI = IRI
type VersionIRI = IRI
type OntologyIRI = IRI
type FullIRI = IRI
type DatatypeIRI = IRI
type ClassIRI = IRI
type ObjectPropertyIRI = IRI
type DataPropertyIRI = IRI
type IndividualIRI = IRI
type Frame = String
type PrefixName = String
type Annotations = AnnotatedList Annotation
type Descriptions = AnnotatedList Description
type DataRange = NonEmpty DataConjunction
type DataConjunction = NonEmpty DataPrimary
type ObjectPropertyExpression = WithNegation IRI
type Description = NonEmpty Conjunction
type Fact = WithNegation FactElement

newtype Exponent = Exponent Integer
newtype DecimalLiteral = DecimalL Double deriving Show
newtype IntegerLiteral = IntegerL Integer deriving Show
newtype NodeID = NodeID String deriving Show
newtype AnnotatedList a = AnnList [(AnnotatedList Annotation, a)]
newtype DataPropertyExpression = Dpe IRI
newtype DataProperty = DataP IRI deriving Show

data TypedLiteral = TypedL String String deriving Show
data FloatPoint = FloatP Double (Maybe Exponent)
data LiteralWithLang = LiteralWithLang String LangTag deriving Show
data ObjectProperty = ObjectP IRI | InverseObjectP IRI deriving Show
data OntologyDocument = OntologyD [PrefixDeclaration] Ontology deriving Show
data PrefixDeclaration = PrefixD PrefixName FullIRI deriving Show
data Ontology = Ontology (Maybe OntologyVersionIRI) [ImportIRI] [AnnotatedList Annotation] [Frame] deriving Show
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving Show
data Annotation = Annotation AnnotationPropertyIRI String
data FrameF = DatatypeFrame
            | ClassFrame
            | ObjectPropertyFrame
            | DataPropertyFrame
            | AnnotationPropertyFrame
            | IndividualFrame
            | Misc
            deriving Show
data DatatypeFrame = DatatypeF Datatype Annotations (Maybe AnnotDataRange)
data AnnotDataRange = AnnotDataRange Annotations DataRange
data Datatype = DatatypeIRI
              | IntegerDT
              | DecimalDT
              | FloatDT
              | StringDT deriving Show
data DataPrimary = DataPrimary Bool DataAtomic
data DataAtomic = DatatypeDA Datatype
                | LiteralList (NonEmpty Literal)
data ClassFrame = ClassF IRI [ClassElement] Key
data ClassElement = AnnotationCE Annotations
                  | SubClassOfCE Descriptions
                  | EquivalentToCE Descriptions
                  | DisjointToCE Descriptions
                  | DisjointUnionOfCE Annotations (AtLeast2List Description)
data Key = KeyAnn Annotations [DataPropertyExpression] [ObjectPropertyExpression]
data WithNegation a = Positive a | Negative a
data Conjunction = ClassConj IRI (NonEmpty (WithNegation Restriction)) | PrimConj (NonEmpty Primary)
data Primary = PrimaryR (WithNegation Restriction) | PrimaryA (WithNegation Atomic)
data Restriction = OPRestriction ObjectPropertyRestriction | DPRestriction DataPropertyRestriction
data ObjectPropertyRestrictionType = SomeOPR Primary
                                   | OnlyOPR Primary
                                   | ValueOPR Individual
                                   | SelfOPR
                                   | MinOPR Int (Maybe Primary) -- TODO: Int -> Nat
                                   | MaxOPR Int (Maybe Primary) -- TODO: Int -> Nat
                                   | ExactlyOPR Int (Maybe Primary) -- TODO: Int -> Nat
data DataPropertyRestrictionType = SomeDPR DataPrimary
                                 | OnlyDPR DataPrimary
                                 | ValueDPR Literal
                                 | MinDPR Int (Maybe DataPrimary) -- TODO: Int -> Nat
                                 | MaxDPR Int (Maybe DataPrimary) -- TODO: Int -> Nat
                                 | ExactlyDPR Int (Maybe DataPrimary) -- TODO: Int -> Nat
data ObjectPropertyRestriction = OPR ObjectPropertyExpression ObjectPropertyRestrictionType
data DataPropertyRestriction = DPR DataPropertyExpression DataPropertyRestrictionType
data Individual = IRIIndividual IndividualIRI | NodeIndividual NodeID
data Atomic = AtomicClass ClassIRI | AtomicIndividuals [Individual] | AtomicDescription Description
data ObjectPropertyFrame = ObjectPropertyF ObjectPropertyIRI [ObjectPropertyElement]
data ObjectPropertyElement = AnnotationOPE Annotations
                           | DomainOPE Descriptions
                           | RangeOPE Descriptions
                           | CharacteristicsOPE (AnnotatedList ObjectPropertyCharacteristics)
                           | SubPropertyOfOPE (AnnotatedList ObjectPropertyExpression)
                           | EquivalentToOPE (AnnotatedList ObjectPropertyExpression)
                           | DisjointWithOPE (AnnotatedList ObjectPropertyExpression)
                           | InverseOfOPE (AnnotatedList ObjectPropertyExpression)
                           | SubPropertyChainOPE Annotations (AtLeast2List ObjectPropertyExpression)
data ObjectPropertyCharacteristics = FUNCTIONAL
                                   | INVERSEFUNCTIONAL
                                   | REFLEXIVE
                                   | IRREFLEXIVE
                                   | SYMMETRIC
                                   | ASYMMETRIC
                                   | TRANSITIVE
data DataPropertyFrame = DataPropertyF DataPropertyIRI [DataPropertyElement]
data DataPropertyElement = AnnotationDPE Annotations
                         | DomainDPE Descriptions
                         | RangeDPE (AnnotatedList DataRange)
                         | CharacteristicsDPE Annotations DataPropertyCharacteristics
                         | SubPropertyOfDPE (AnnotatedList DataPropertyExpression)
                         | EquivalentToDPE (AnnotatedList DataPropertyExpression)
                         | DisjointWithDPE (AnnotatedList DataPropertyExpression)
data DataPropertyCharacteristics = FUNCTIONAL_DPE
data AnnotationPropertyFrame = AnnotationPropertyF AnnotationPropertyIRI [AnnotationPropertyElement]
data AnnotationPropertyElement = AnnotationAPE Annotations
                               | DomainAPE (AnnotatedList IRI)
                               | RangeAPE (AnnotatedList IRI)
                               | SubPropertyOfAPE (AnnotatedList AnnotationPropertyIRI)
data IndividualFrame = IndividualF Individual [IndividualElement]
data IndividualElement = AnnotationIE Annotations
                       | TypeIE Descriptions
                       | FactIE (AnnotatedList Fact)
                       | SameAsIE (AnnotatedList Individual)
                       | DifferentFromIE (AnnotatedList Individual)
data FactElement = ObjectPropertyFE ObjectPropertyFact | DataPropertyFE DataPropertyFact
data ObjectPropertyFact = ObjectPropertyFact ObjectPropertyIRI Individual
data DataPropertyFact = FataPropertyFact DataPropertyIRI Literal
data Misc = EquivalentClasses Annotations (AtLeast2List Description)
          | DisjointClasses Annotations (AtLeast2List Description)
          | EquivalentObjectProperties Annotations (AtLeast2List ObjectProperty)
          | DisjointObjectProperties Annotations (AtLeast2List ObjectProperty)
          | EquivalentDataProperties Annotations (AtLeast2List DataProperty)
          | DisjointDataProperties Annotations (AtLeast2List DataProperty)
          | SameIndividual Annotations (AtLeast2List Individual)
          | DifferentIndividual Annotations (AtLeast2List Individual)
data Literal = TypedLiteralC TypedLiteral
             | StringLiteralNoLang String
             | StringLiteralLang LiteralWithLang
             | IntegerLiteralC IntegerLiteral
             | DecimalLiteralC DecimalLiteral
             | FloatingLiteralC FloatPoint
-- data DataPrimary = DataPr DataAtomic | DataPrNot DataAtomic deriving Show

-------------------------
---- CLASS INSTANCES ----
-------------------------

instance Show Annotation where
  show (Annotation i s) = unwords [i, show s]

instance (Show a) => Show (AnnotatedList a) where
  show (AnnList []) = ""
  show (AnnList xs) = intercalate ",\n" (go <$> xs)
   where
    go (AnnList [], x) = show x
    go (al, x) = unwords ["Annotations:", show al, "\n ", show x]

instance Show FloatPoint where
  show (FloatP n me) = concat [show n, maybe "" show me]
instance Show Exponent where
  show (Exponent i) = concat ["e", if i > 0 then "+" else "", show i]

