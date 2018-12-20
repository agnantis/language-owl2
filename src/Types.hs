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
type FullIRI = IRI
type DatatypeIRI = IRI
type ClassIRI = IRI
type ObjectPropertyIRI = IRI
type DataPropertyIRI = IRI
type IndividualIRI = IRI
--type Frame = String
type PrefixName = String
type Annotations = AnnotatedList Annotation
type Descriptions = AnnotatedList Description
newtype DataRange = DataRange (NonEmpty DataConjunction)
newtype DataConjunction = DataConjunction (NonEmpty DataPrimary)
type WithInversion = WithNegation
type Description = NonEmpty Conjunction
type Fact = WithNegation FactElement

newtype Exponent = Exponent Integer
newtype DecimalLiteral = DecimalL Double deriving Show
newtype IntegerLiteral = IntegerL Integer deriving Show
newtype NodeID = NodeID String deriving Show
newtype AnnotatedList a = AnnList [(AnnotatedList Annotation, a)]
type DataPropertyExpression = DataPropertyIRI
type ObjectPropertyExpression = WithInversion ObjectPropertyIRI

data IRI = FullIRI String
         | AbbreviatedIRI PrefixName String
         | SimpleIRI String deriving Show
data TypedLiteral = TypedL String Datatype deriving Show
data FloatPoint = FloatP Double (Maybe Exponent)
data LiteralWithLang = LiteralWithLang String LangTag
data OntologyDocument = OntologyD [PrefixDeclaration] Ontology
data PrefixDeclaration = PrefixD PrefixName IRI deriving Show
data Ontology = Ontology (Maybe OntologyVersionIRI) [ImportIRI] [AnnotatedList Annotation] [Frame]
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI)
data Annotation = Annotation AnnotationPropertyIRI AnnotationTarget
data Frame = FrameDT DatatypeFrame
           | FrameC ClassFrame
           | FrameOP ObjectPropertyFrame
           | FrameDP DataPropertyFrame
           | FrameAP AnnotationPropertyFrame
           | FrameI IndividualFrame
           | FrameM Misc
data DatatypeFrame = DatatypeF Datatype Annotations (Maybe AnnotDataRange)
data AnnotDataRange = AnnotDataRange Annotations DataRange
data Datatype = DatatypeIRI IRI
              | IntegerDT
              | DecimalDT
              | FloatDT
              | StringDT deriving Show
type DataPrimary = WithNegation DataAtomic
data DataAtomic = DatatypeDA Datatype
                | LiteralListDA (NonEmpty Literal)
                | DatatypeRestrictionDA DatatypeRestriction
                | DataRangeDA DataRange
data DatatypeRestriction = DatatypeRestriction Datatype (NonEmpty RestrictionExp)
data RestrictionExp = RestrictionExp Facet Literal
data Facet = LENGTH_FACET
           | MIN_LENGTH_FACET
           | MAX_LENGTH_FACET
           | PATTERN_FACET
           | LANG_RANGE_FACET
           | LE_FACET
           | L_FACET
           | GE_FACET
           | G_FACET
data ClassFrame = ClassF IRI [ClassElement]
data ClassElement = AnnotationCE Annotations
                  | SubClassOfCE Descriptions
                  | EquivalentToCE Descriptions
                  | DisjointToCE Descriptions
                  | DisjointUnionOfCE (Maybe Annotations) (AtLeast2List Description)
                  | HasKeyCE (Maybe Annotations) NonEmptyListOfObjectOrDataPE
data NonEmptyListOfObjectOrDataPE = NonEmptyO (NonEmpty ObjectPropertyExpression) [DataPropertyExpression]
                                  | NonEmptyD [ObjectPropertyExpression] (NonEmpty DataPropertyExpression) 
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
data Atomic = AtomicClass ClassIRI | AtomicIndividuals (NonEmpty Individual) | AtomicDescription Description
data ObjectPropertyFrame = ObjectPropertyF ObjectPropertyIRI [ObjectPropertyElement]
data ObjectPropertyElement = AnnotationOPE Annotations
                           | DomainOPE Descriptions
                           | RangeOPE Descriptions
                           | CharacteristicsOPE (AnnotatedList ObjectPropertyCharacteristics)
                           | SubPropertyOfOPE (AnnotatedList ObjectPropertyExpression)
                           | EquivalentToOPE (AnnotatedList ObjectPropertyExpression)
                           | DisjointWithOPE (AnnotatedList ObjectPropertyExpression)
                           | InverseOfOPE (AnnotatedList ObjectPropertyExpression)
                           | SubPropertyChainOPE (AnnotatedList (AtLeast2List ObjectPropertyExpression))
data ObjectPropertyCharacteristics = FUNCTIONAL
                                   | INVERSE_FUNCTIONAL
                                   | REFLEXIVE
                                   | IRREFLEXIVE
                                   | SYMMETRIC
                                   | ASYMMETRIC
                                   | TRANSITIVE
data DataPropertyFrame = DataPropertyF DataPropertyIRI [DataPropertyElement]
data DataPropertyElement = AnnotationDPE Annotations
                         | DomainDPE Descriptions
                         | RangeDPE (AnnotatedList DataRange)
                         | CharacteristicsDPE (AnnotatedList DataPropertyCharacteristics)
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
data DataPropertyFact = DataPropertyFact DataPropertyIRI Literal
data Misc = EquivalentClasses (AnnotatedList (AtLeast2List Description))
          | DisjointClasses (AnnotatedList (AtLeast2List Description))
          | EquivalentObjectProperties (AnnotatedList (AtLeast2List ObjectPropertyExpression))
          | DisjointObjectProperties (AnnotatedList (AtLeast2List ObjectPropertyExpression))
          | EquivalentDataProperties (AnnotatedList (AtLeast2List DataPropertyExpression))
          | DisjointDataProperties (AnnotatedList (AtLeast2List DataPropertyExpression))
          | SameIndividual (AnnotatedList (AtLeast2List Individual))
          | DifferentIndividual (AnnotatedList (AtLeast2List Individual))
data Literal = TypedLiteralC TypedLiteral
             | StringLiteralNoLang String
             | StringLiteralLang LiteralWithLang
             | IntegerLiteralC IntegerLiteral
             | DecimalLiteralC DecimalLiteral
             | FloatingLiteralC FloatPoint
data Entity = DatatypeEntity Datatype
            | ClassEntity ClassIRI
            | ObjectPropertyEntity ObjectPropertyIRI
            | DataPropertyEntity DataPropertyIRI
            | AnnotationPropertyEntity AnnotationPropertyIRI
            | IndividualEntity IndividualIRI
data AnnotationTarget = NodeAT NodeID
                      | IriAT IRI
                      | LiteralAT Literal
-- data DataPrimary = DataPr DataAtomic | DataPrNot DataAtomic deriving Show


---------------------------
---- UTILITY FUNCTIONS ----
---------------------------


-------------------------
---- CLASS INSTANCES ----
-------------------------

instance Semigroup (AnnotatedList a) where
  (AnnList xs) <> (AnnList ys) = AnnList (xs <> ys)  

instance Monoid (AnnotatedList a) where
  mempty = AnnList []


instance Show Annotation where
  show (Annotation i s) = unwords [show i, "<undefinded-annotation-target>"]

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

