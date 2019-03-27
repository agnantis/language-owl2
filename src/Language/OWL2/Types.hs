{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Language.OWL2.Types where

import           Data.List                                ( uncons )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                               ( fromMaybe )
import           Language.OWL2.Import                     ( Text )

---------------
---- TYPES ----
---------------

-- TODO: Should I include a NonEmpty? I do not think so
data AtLeast2List a = (a, a) :# [a] deriving (Eq, Ord, Show, Read, Functor)

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
type Exponent = Integer
type DataPropertyExpression = DataPropertyIRI
type AnnotationProperty = AnnotationPropertyIRI
type Annotations = [Annotated Annotation]

-- Data types --
data ObjectPropertyExpression
    = OPE ObjectPropertyIRI
    | InverseOPE ObjectPropertyIRI deriving (Eq, Ord, Show)
data DataRange
    = DatatypeDR Datatype
    | IntersectionDR (AtLeast2List DataRange)
    | UnionDR (AtLeast2List DataRange)
    | ComplementDR DataRange
    | OneOfDR (NonEmpty Literal)
    | RestrictionDR DatatypeRestriction deriving (Eq, Ord, Show)
newtype DecimalLiteral = DecimalL Double deriving (Eq, Ord, Show)
newtype IntegerLiteral = IntegerL Integer deriving (Eq, Ord, Show)
newtype NodeID = NodeID Text deriving (Eq, Ord, Show)
newtype Annotated a = Annotated { unAnnotated :: ([Annotated Annotation], a) } deriving (Eq, Ord, Show) -- TODO: use a sum type instead of pair for easier access
newtype ImportDeclaration = ImportD IRI deriving (Eq, Ord, Show)
data IRI
    = FullIRI Text
    | AbbreviatedIRI PrefixName Text
    | SimpleIRI Text deriving (Eq, Ord, Show)
data TypedLiteral = TypedL Text Datatype deriving (Eq, Ord, Show)
data FloatPoint = FloatP Double (Maybe Exponent) deriving (Eq, Ord, Show)
data LiteralWithLang = LiteralWithLang Text LangTag deriving (Eq, Ord, Show)
data OntologyDocument = OntologyD
    { prefixes :: [PrefixDeclaration]
    , ontology :: Ontology
    } deriving (Eq, Ord, Show)
data PrefixDeclaration = PrefixD PrefixName IRI deriving (Eq, Ord, Show)
data Ontology = Ontology 
    { version :: (Maybe OntologyVersionIRI)
    , imports :: [ImportDeclaration]
    , ants    :: Annotations
    , axioms  :: [Axiom]
    } deriving (Eq, Ord, Show)
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving (Eq, Ord, Show)
data Annotation = Annotation AnnotationProperty AnnotationValue deriving (Eq, Ord, Show)
newtype Datatype = Datatype { unDatatype :: DatatypeIRI } deriving (Eq, Ord, Show)
data DatatypeRestriction = DatatypeRestriction Datatype (NonEmpty RestrictionExp) deriving (Eq, Ord, Show)
data RestrictionExp = RestrictionExp Facet Literal deriving (Eq, Ord, Show)
data Facet
    = LENGTH_FACET
    | MIN_LENGTH_FACET
    | MAX_LENGTH_FACET
    | PATTERN_FACET
    | LANG_RANGE_FACET
    | LE_FACET
    | L_FACET
    | GE_FACET
    | G_FACET deriving (Eq, Ord, Show)
data ObjectOrDataPE
    = ObjectPE ObjectPropertyExpression
    | DataPE DataPropertyExpression deriving (Eq, Ord, Show)
data WithNegation a
    = Positive a
    | Negative a deriving (Eq, Ord, Show, Functor)
data WithInversion a
    = Plain a
    | Inverse a deriving (Eq, Ord, Show, Functor)
data Conjunction
    = ClassConj IRI (NonEmpty (WithNegation Restriction))
    | PrimConj (NonEmpty Primary) deriving (Eq, Ord, Show)
data Primary
    = PrimaryR (WithNegation Restriction)
    | PrimaryA (WithNegation Atomic) deriving (Eq, Ord, Show)
data Restriction
    = OPRestriction ObjectPropertyRestriction
    | DPRestriction DataPropertyRestriction deriving (Eq, Ord, Show)
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
    | CExpDataExactCardinality Int DataPropertyExpression (Maybe DataRange) deriving (Eq, Ord, Show)
data ObjectPropertyRestrictionType
    = SelfOPR
    | SomeOPR Primary
    | OnlyOPR Primary
    | ValueOPR Individual
    | MinOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | MaxOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | ExactlyOPR Int (Maybe Primary) deriving (Eq, Ord, Show) -- TODO: Int -> Nat
data DataPropertyRestrictionType
    = SomeDPR DataRange
    | OnlyDPR DataRange
    | ValueDPR Literal
    | MinDPR Int (Maybe DataRange) -- TODO: Int -> Nat
    | MaxDPR Int (Maybe DataRange) -- TODO: Int -> Nat
    | ExactlyDPR Int (Maybe DataRange) deriving (Eq, Ord, Show) -- TODO: Int -> Nat
data ObjectPropertyRestriction = OPR ObjectPropertyExpression ObjectPropertyRestrictionType deriving (Eq, Ord, Show)
data DataPropertyRestriction = DPR DataPropertyExpression DataPropertyRestrictionType deriving (Eq, Ord, Show)
data TotalIRI
    = NamedIRI IRI
    | AnonymousIRI NodeID deriving (Eq, Ord, Show)
data Atomic
    = AtomicClass ClassIRI
    | AtomicIndividuals (NonEmpty Individual)
    | AtomicDescription ClassExpression deriving (Eq, Ord, Show)
newtype ObjectPropertyChain = ObjectPropertyChain { unChain :: AtLeast2List ObjectPropertyExpression } deriving (Eq, Ord, Show)
data ObjectPropertyCharacteristic
    = FUNCTIONAL
    | INVERSE_FUNCTIONAL
    | REFLEXIVE
    | IRREFLEXIVE
    | SYMMETRIC
    | ASYMMETRIC
    | TRANSITIVE deriving (Eq, Ord, Show)
data DataPropertyCharacteristics = FUNCTIONAL_DPE deriving (Eq, Ord, Show)
data FactElement
    = ObjectPropertyFact ObjectPropertyIRI Individual
    | NegativeObjectPropertyFact ObjectPropertyIRI Individual
    | DataPropertyFact DataPropertyIRI Literal
    | NegativeDataPropertyFact DataPropertyIRI Literal deriving (Eq, Ord, Show)
data Literal
    = TypedLiteralC TypedLiteral
    | StringLiteralNoLang Text
    | StringLiteralLang LiteralWithLang
    | IntegerLiteralC IntegerLiteral
    | DecimalLiteralC DecimalLiteral
    | FloatingLiteralC FloatPoint deriving (Eq, Ord, Show)
data Entity
    = EntityDatatype Datatype
    | EntityClass ClassIRI
    | EntityObjectProperty ObjectPropertyIRI
    | EntityDataProperty DataPropertyIRI
    | EntityAnnotationProperty AnnotationProperty
    | EntityIndividual IndividualIRI deriving (Eq, Ord, Show)
data AnnotationValue
    = NodeAT NodeID
    | IriAT IRI
    | LiteralAT Literal deriving (Eq, Ord, Show)
data Axiom
    = DeclarationAxiom Annotations Entity
    | AnnotationAxiomDomain Annotations AnnotationProperty IRI
    | AnnotationAxiomRange Annotations AnnotationProperty IRI
    | AnnotationAxiomSubProperty Annotations AnnotationProperty AnnotationProperty
    | AnnotationAxiomAssertion Annotations TotalIRI Annotation
    | DatatypeAxiomEquivalent Annotations Datatype DataRange
    | ObjectPropAxiomDomain Annotations ObjectPropertyExpression ClassExpression
    | ObjectPropAxiomRange Annotations ObjectPropertyExpression ClassExpression
    | ObjectPropAxiomCharacteristics Annotations ObjectPropertyExpression ObjectPropertyCharacteristic
    | ObjectPropAxiomSubProperty Annotations ObjectPropertyExpression ObjectPropertyExpression
    | ObjectPropAxiomChainSubProperty Annotations ObjectPropertyChain ObjectPropertyExpression
    | ObjectPropAxiomEquivalent Annotations ObjectPropertyExpression (NonEmpty ObjectPropertyExpression)
    | ObjectPropAxiomDisjoint Annotations ObjectPropertyExpression (NonEmpty ObjectPropertyExpression)
    | ObjectPropAxiomInverse Annotations ObjectPropertyExpression ObjectPropertyExpression
    | DataPropAxiomDomain Annotations DataPropertyExpression ClassExpression
    | DataPropAxiomRange Annotations DataPropertyExpression DataRange
    | DataPropAxiomCharacteristics Annotations DataPropertyExpression DataPropertyCharacteristics
    | DataPropAxiomSubProperty Annotations DataPropertyExpression DataPropertyExpression
    | DataPropAxiomEquivalent Annotations DataPropertyExpression (NonEmpty DataPropertyExpression)
    | DataPropAxiomDisjoint Annotations DataPropertyExpression (NonEmpty DataPropertyExpression)
    | ClassAxiomSubClassOf Annotations ClassExpression ClassExpression
    | ClassAxiomEquivalentClasses Annotations ClassExpression (NonEmpty ClassExpression)
    | ClassAxiomDisjointClasses Annotations ClassExpression (NonEmpty ClassExpression)
    | ClassAxiomDisjointUnion Annotations ClassIRI (AtLeast2List ClassExpression)
    | ClassAxiomHasKey Annotations ClassExpression (NonEmpty ObjectOrDataPE)
    | AssertionAxiomSameIndividuals Annotations (AtLeast2List Individual)
    | AssertionAxiomDifferentIndividuals Annotations (AtLeast2List Individual)
    | AssertionAxiomClass Annotations Individual ClassExpression
    | AssertionAxiomObjectProperty Annotations ObjectPropertyExpression Individual Individual
    | AssertionAxiomNegativeObjectProperty Annotations ObjectPropertyExpression Individual Individual 
    | AssertionAxiomDataProperty Annotations DataPropertyExpression Individual Literal
    | AssertionAxiomNegativeDataProperty Annotations DataPropertyExpression Individual Literal deriving (Eq, Ord, Show)


---------------------------
---- utility functions ----
---------------------------
atLeast2List :: a -> a -> [a] -> AtLeast2List a
atLeast2List x y = (:#) (x, y)

atLeast2List' :: a -> NonEmpty a -> AtLeast2List a
atLeast2List' x nx = let (x' :| xs) = nx in (x, x') :# xs

toList :: AtLeast2List a -> [a]
toList ~((x, y) :# xs) = x : y : xs

toNonEmptyList :: AtLeast2List a -> NonEmpty a
toNonEmptyList ~((x, y) :# xs) = x :| (y : xs)

singleton :: a -> NonEmpty a
singleton x = x :| []

-- | Searches the provided initial element and the elements the list for an element that sutisfies
-- the predicate p. It returns a pair with this first element and a list with the rest, or the default
-- (i.e., arguments "a" and "NonEmpty a" if no such element exists
-- if no element can satisfy the predicate
--
-- >>> xs = NE.fromList [4,7,6,12,9]
-- >>> p = ((== 0) . (`mod` 3))
-- >>> promote p 13 xs
-- (6,13 :| [4,7,12,9])
--
-- >>> promote p 12 xs
-- (12,4 :| [7,6,12,9])
--
-- >>> promote (const False) 12 xs
-- (12,4 :| [7,6,12,9])
--
promote :: (a -> Bool) -> a -> NonEmpty a -> (a, NonEmpty a)
promote p x xs
  | p x = (x, xs)
  | otherwise = fromMaybe (x, xs) result
   where
    (xs1, xs2) = break p $ NE.toList xs
    els = uncons xs2
    result = (\(v, rest) -> (v, x :| (xs1 <> rest))) <$> els

-- | Provided a pair of elements it reolrder to the pair when the first element does not
-- comply with the predicate but the other does comply. In any other case the order remains
-- unchanged
--
-- >>> reorder ((== 1) . (`mod` 4)) (5,7)
-- (5,7)
--
-- >>> reorder ((== 1) . (`mod` 4)) (4,9)
-- (9,4)
--
-- >>> reorder ((== 1) . (`mod` 4)) (5,9)
-- (5,9)
--
reorder :: (a -> Bool) -> (a, a) -> (a, a)
reorder p (x1, x2)
  | p x1 = (x1, x2)
  | p x2 = (x2, x1)
  | otherwise = (x1, x2) 
-- | Like 'promote' but on a 'AtLeast2List a' list
--
-- >>> xs = atLeast2List 13 4 [7, 6,12,9]
-- >>> xs' = atLeast2List 12 4 [7, 6,12,9]
-- >>> p = ((== 0) . (`mod` 3))
-- >>> extract p xs
-- (6,13 :| [4,7,12,9])
--
-- >>> extract p xs'
-- (12,4 :| [7,6,12,9])
--
-- >>> extract (const False) xs'
-- (12,4 :| [7,6,12,9])
--
extract :: (a -> Bool) -> AtLeast2List a -> (a, NonEmpty a)
extract p ((x1, x2) :# xs) = promote p x1 (x2 :| xs)

-- | utility function that returns 'True' only if the 'ClassExpression' is about a named
-- class, i.e., is an IRI
filterClassIRI :: ClassExpression -> Bool
filterClassIRI (CExpClass _) = True
filterClassIRI _ = False

-- | utility function that returns 'True' only if the 'ObjectPropertyExpression' is about a
-- named property, i.e., is an IRI
filterObjectPropIRI :: ObjectPropertyExpression -> Bool
filterObjectPropIRI (OPE _) = True
filterObjectPropIRI _ = False

