{-# LANGUAGE DeriveFunctor #-}


module Language.OWL2.Types where

import           Data.List                                ( uncons )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
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
data Annotation = Annotation AnnotationProperty AnnotationValue deriving (Show)
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
newtype ObjectPropertyChain = ObjectPropertyChain { unChain :: AtLeast2List ObjectPropertyExpression } deriving (Show)
data ObjectPropertyCharacteristic
    = FUNCTIONAL
    | INVERSE_FUNCTIONAL
    | REFLEXIVE
    | IRREFLEXIVE
    | SYMMETRIC
    | ASYMMETRIC
    | TRANSITIVE deriving (Show)
data DataPropertyCharacteristics = FUNCTIONAL_DPE deriving (Show)
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
--data Declaration = Declaration Annotations Entity deriving (Show)
data Entity
    = EntityDatatype Datatype
    | EntityClass ClassIRI
    | EntityObjectProperty ObjectPropertyIRI
    | EntityDataProperty DataPropertyIRI
    | EntityAnnotationProperty AnnotationProperty
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

data Axiom
    = DeclarationAxiom Annotations Entity
    -- | DatatypeAxiomAnnotation Annotations Datatype Annotation
    | DatatypeAxiomEquivalent Annotations Datatype DataRange
    -- | ClassAxiomAnnotation Annotations ClassExpression Annotation -- TODO: I may have to move it from here as these axiom are included in all *Axioms*
    | ClassAxiomSubClassOf Annotations ClassExpression ClassExpression
    | ClassAxiomEquivalentClasses Annotations (AtLeast2List ClassExpression)
    | ClassAxiomDisjointClasses Annotations (AtLeast2List ClassExpression)
    | ClassAxiomDisjointUnion Annotations ClassIRI (AtLeast2List ClassExpression)
    | ClassAxiomHasKey Annotations ClassExpression (NonEmpty ObjectOrDataPE)
    -- | ObjectPropAxiomAnnotation Annotations ObjectPropertyExpression Annotation
    | ObjectPropAxiomDomain Annotations ObjectPropertyExpression ClassExpression
    | ObjectPropAxiomRange Annotations ObjectPropertyExpression ClassExpression
    | ObjectPropAxiomCharacteristics Annotations ObjectPropertyExpression ObjectPropertyCharacteristic
    | ObjectPropAxiomSubProperty Annotations ObjectPropertyExpression ObjectPropertyExpression
    | ObjectPropAxiomChainSubProperty Annotations ObjectPropertyChain ObjectPropertyExpression
    | ObjectPropAxiomEquivalent Annotations (AtLeast2List ObjectPropertyExpression)
    | ObjectPropAxiomDisjoint Annotations (AtLeast2List ObjectPropertyExpression)
    | ObjectPropAxiomInverse Annotations ObjectPropertyExpression ObjectPropertyExpression
    -- | DataPropAxiomAnnotation Annotations DataPropertyExpression Annotation
    | DataPropAxiomDomain Annotations DataPropertyExpression ClassExpression
    | DataPropAxiomRange Annotations DataPropertyExpression DataRange
    | DataPropAxiomCharacteristics Annotations DataPropertyExpression DataPropertyCharacteristics
    | DataPropAxiomSubProperty Annotations DataPropertyExpression DataPropertyExpression
    | DataPropAxiomEquivalent Annotations (AtLeast2List DataPropertyExpression)
    | DataPropAxiomDisjoint Annotations (AtLeast2List DataPropertyExpression)
    -- | AnnotationAxiomAnnotation Annotations AnnotationProperty Annotation
    | AnnotationAxiomDomain Annotations AnnotationProperty IRI
    | AnnotationAxiomRange Annotations AnnotationProperty IRI
    | AnnotationAxiomSubProperty Annotations AnnotationProperty AnnotationProperty
    | AnnotationAxiomAssertion Annotations TotalIRI Annotation
    -- | AssertionAxiomAnnotation Annotations TotalIRI Annotation
    | AssertionAxiomSameIndividuals Annotations (AtLeast2List Individual)
    | AssertionAxiomDifferentIndividuals Annotations (AtLeast2List Individual)
    | AssertionAxiomClass Annotations Individual ClassExpression
    | AssertionAxiomObjectProperty Annotations ObjectPropertyExpression Individual Individual
    | AssertionAxiomNegativeObjectProperty Annotations ObjectPropertyExpression Individual Individual 
    | AssertionAxiomDataProperty Annotations DataPropertyExpression Individual Literal
    | AssertionAxiomNegativeDataProperty Annotations DataPropertyExpression Individual Literal deriving (Show)



-- | Searches the provided intial element and the elements the list for an element that sutisfies
-- the predicate p. It returns a pair with this first element and a list with the rest, or Nothing
-- if no element can satisfy the predicate
--
-- >>> xs = NE.fromList [4,7,6,12,9]
-- >>> p = ((== 0) . (`mod` 3))
-- >>> promote p 13 xs
-- Just (6,13 :| [4,7,12,9])
--
-- >>> promote p 12 xs
-- Just (12,4 :| [7,6,12,9])
--
-- >>> promote (const False) 12 xs
-- Nothing
--
promote :: (a -> Bool) -> a -> NonEmpty a -> Maybe (a, NonEmpty a)
promote p x xs
  | p x = Just (x, xs)
  | otherwise = result
   where
    (xs1, xs2) = break p $ NE.toList xs
    els = uncons xs2
    result = (\(v, rest) -> (v, x :| (xs1 <> rest))) <$> els


-- safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safehead (x:_) = Just x
-- 
-- safeTail :: [a] -> Maybe [a]
-- safeTail [] = Nothing
-- safeTail (_:xs) = Just xs

