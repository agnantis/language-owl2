{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.OWL2.Types
  ( Annotated(..)
  , Annotation(..)
  , AnnotationPropertyIRI
  , AnnotationValue(..)
  , Annotations
  , AtLeast2List(..)
  , Atomic(..)
  , Axiom(..)
  , AxiomType(..)
  , ClassExpression(..)
  , ClassIRI
  , Conjunction(..)
  , DataPropertyCharacteristics(..)
  , DataPropertyExpression
  , DataPropertyIRI
  , DataPropertyRestriction(..)
  , DataPropertyRestrictionType(..)
  , DataRange(..)
  , Datatype(..)
  , DatatypeRestriction(..)
  , DecimalLiteral(..)
  , Entity(..)
  , Exponent
  , Facet(..)
  , FactElement(..)
  , FloatPoint(..)
  , IRI(..)
  , ImportDeclaration(..)
  , Individual
  , IndividualIRI
  , IntegerLiteral(..)
  , LangTag
  , Literal(..)
  , LiteralWithLang(..)
  , NodeID(..)
  , ObjectOrDataPE(..)
  , ObjectPropertyChain(..)
  , ObjectPropertyCharacteristic(..)
  , ObjectPropertyExpression(..)
  , ObjectPropertyIRI
  , ObjectPropertyRestriction(..)
  , ObjectPropertyRestrictionType(..)
  , Ontology(..)
  , OntologyDocument(..)
  , OntologyVersionIRI(..)
  , PrefixDeclaration(..)
  , Primary(..)
  , Restriction(..)
  , RestrictionExp(..)
  , TotalIRI(..)
  , TypedLiteral(..)
  , WithNegation(..)
  , atLeast2List
  , atLeast2List'
  , axiomType
  , extract
  , extractAxioms
  , filterClassIRI
  , filterNamedIRI
  , filterObjectPropIRI
  , groupAxiomsOnConstructor
  , groupAxiomsOnIRI
  , mapAxiomsOnIRI
  , reorder
  , singleton
  , toList
  )
where

import           Control.Applicative                      ( (<|>) )
import           Control.Monad.State
import           Data.Data                                ( Data, Typeable, toConstr )
import           Data.Function                            ( on )
import           Data.List                                ( partition, uncons, groupBy )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                               ( fromMaybe )
import           GHC.Exts                                 ( groupWith )

import           Language.OWL2.Import                     ( Text )

---------------
---- TYPES ----
---------------

-- TODO: Should I include a NonEmpty? I do not think so
data AtLeast2List a = (a, a) :# [a] deriving (Eq, Ord, Show, Read, Functor, Typeable, Data)

-- Type synonyms --
type LangTag = Text
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
    | InverseOPE ObjectPropertyIRI deriving (Eq, Ord, Show, Typeable, Data)
data DataRange
    = DatatypeDR Datatype
    | IntersectionDR (AtLeast2List DataRange)
    | UnionDR (AtLeast2List DataRange)
    | ComplementDR DataRange
    | OneOfDR (NonEmpty Literal)
    | RestrictionDR DatatypeRestriction deriving (Eq, Ord, Show, Typeable, Data)
newtype DecimalLiteral = DecimalL Double deriving (Eq, Ord, Show, Typeable, Data)
newtype IntegerLiteral = IntegerL Integer deriving (Eq, Ord, Show, Typeable, Data)
newtype NodeID = NodeID Text deriving (Eq, Ord, Show, Typeable, Data)
newtype Annotated a = Annotated { _unAnnotated :: ([Annotated Annotation], a) } deriving (Eq, Ord, Show, Typeable, Data) -- TODO: use a sum type instead of pair for easier access
newtype ImportDeclaration = ImportD IRI deriving (Eq, Ord, Show, Typeable, Data)
data IRI
    = FullIRI Text
    | AbbreviatedIRI PrefixName Text
    | SimpleIRI Text deriving (Eq, Ord, Show, Typeable, Data)
data TypedLiteral = TypedL Text Datatype deriving (Eq, Ord, Show, Typeable, Data)
data FloatPoint = FloatP Double (Maybe Exponent) deriving (Eq, Ord, Show, Typeable, Data)
data LiteralWithLang = LiteralWithLang Text LangTag deriving (Eq, Ord, Show, Typeable, Data)
data OntologyDocument = OntologyD
    { _prefixes :: [PrefixDeclaration]
    , _ontology :: Ontology
    } deriving (Eq, Ord, Show, Typeable, Data)
data PrefixDeclaration = PrefixD PrefixName IRI deriving (Eq, Ord, Show, Typeable, Data)
data Ontology = Ontology 
    { _version :: Maybe OntologyVersionIRI
    , _imports :: [ImportDeclaration]
    , _ants    :: Annotations
    , _axioms  :: [Axiom]
    } deriving (Eq, Ord, Show, Typeable, Data)
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving (Eq, Ord, Show, Typeable, Data)
data Annotation = Annotation
    { _property :: AnnotationProperty
    , _value     :: AnnotationValue
    } deriving (Eq, Ord, Show, Typeable, Data)
newtype Datatype = Datatype { _unDatatype :: DatatypeIRI } deriving (Eq, Ord, Show, Typeable, Data)
data DatatypeRestriction = DatatypeRestriction Datatype (NonEmpty RestrictionExp) deriving (Eq, Ord, Show, Typeable, Data)
data RestrictionExp = RestrictionExp Facet Literal deriving (Eq, Ord, Show, Typeable, Data)
data Facet
    = LENGTH_FACET
    | MIN_LENGTH_FACET
    | MAX_LENGTH_FACET
    | PATTERN_FACET
    | LANG_RANGE_FACET
    | LE_FACET
    | L_FACET
    | GE_FACET
    | G_FACET deriving (Eq, Ord, Show, Typeable, Data)
data ObjectOrDataPE
    = ObjectPE ObjectPropertyExpression
    | DataPE DataPropertyExpression deriving (Eq, Ord, Show, Typeable, Data)
data WithNegation a
    = Positive a
    | Negative a deriving (Eq, Ord, Show, Functor, Typeable, Data)
data WithInversion a
    = Plain a
    | Inverse a deriving (Eq, Ord, Show, Functor, Typeable, Data)
data Conjunction
    = ClassConj IRI (NonEmpty (WithNegation Restriction))
    | PrimConj (NonEmpty Primary) deriving (Eq, Ord, Show, Typeable, Data)
data Primary
    = PrimaryR (WithNegation Restriction)
    | PrimaryA (WithNegation Atomic) deriving (Eq, Ord, Show, Typeable, Data)
data Restriction
    = OPRestriction ObjectPropertyRestriction
    | DPRestriction DataPropertyRestriction deriving (Eq, Ord, Show, Typeable, Data)
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
    | CExpDataSomeValuesFrom (NonEmpty DataPropertyExpression) DataRange 
    | CExpDataAllValuesFrom (NonEmpty DataPropertyExpression) DataRange
    | CExpDataHasValue DataPropertyExpression Literal
    | CExpDataMinCardinality Int DataPropertyExpression (Maybe DataRange)
    | CExpDataMaxCardinality Int DataPropertyExpression (Maybe DataRange)
    | CExpDataExactCardinality Int DataPropertyExpression (Maybe DataRange) deriving (Eq, Ord, Show, Typeable, Data)
data ObjectPropertyRestrictionType
    = SelfOPR
    | SomeOPR Primary
    | OnlyOPR Primary
    | ValueOPR Individual
    | MinOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | MaxOPR Int (Maybe Primary) -- TODO: Int -> Nat
    | ExactlyOPR Int (Maybe Primary) deriving (Eq, Ord, Show, Typeable, Data) -- TODO: Int -> Nat
data DataPropertyRestrictionType
    = SomeDPR DataRange
    | OnlyDPR DataRange
    | ValueDPR Literal
    | MinDPR Int (Maybe DataRange) -- TODO: Int -> Nat
    | MaxDPR Int (Maybe DataRange) -- TODO: Int -> Nat
    | ExactlyDPR Int (Maybe DataRange) deriving (Eq, Ord, Show, Typeable, Data) -- TODO: Int -> Nat
data ObjectPropertyRestriction = OPR ObjectPropertyExpression ObjectPropertyRestrictionType deriving (Eq, Ord, Show, Typeable, Data)
data DataPropertyRestriction = DPR DataPropertyExpression DataPropertyRestrictionType deriving (Eq, Ord, Show, Typeable, Data)
data TotalIRI
    = NamedIRI IRI
    | AnonymousIRI NodeID deriving (Eq, Ord, Show, Typeable, Data)
data Atomic
    = AtomicClass ClassIRI
    | AtomicIndividuals (NonEmpty Individual)
    | AtomicDescription ClassExpression deriving (Eq, Ord, Show, Typeable, Data)
newtype ObjectPropertyChain = ObjectPropertyChain { unChain :: AtLeast2List ObjectPropertyExpression } deriving (Eq, Ord, Show, Typeable, Data)
data ObjectPropertyCharacteristic
    = FUNCTIONAL
    | INVERSE_FUNCTIONAL
    | REFLEXIVE
    | IRREFLEXIVE
    | SYMMETRIC
    | ASYMMETRIC
    | TRANSITIVE deriving (Eq, Ord, Show, Typeable, Data)
data DataPropertyCharacteristics = FUNCTIONAL_DPE deriving (Eq, Ord, Show, Typeable, Data)
data FactElement
    = ObjectPropertyFact ObjectPropertyIRI Individual
    | NegativeObjectPropertyFact ObjectPropertyIRI Individual
    | DataPropertyFact DataPropertyIRI Literal
    | NegativeDataPropertyFact DataPropertyIRI Literal deriving (Eq, Ord, Show, Typeable, Data)
data Literal
    = TypedLiteralC TypedLiteral
    | StringLiteralNoLang Text
    | StringLiteralLang LiteralWithLang
    | IntegerLiteralC IntegerLiteral
    | DecimalLiteralC DecimalLiteral
    | FloatingLiteralC FloatPoint deriving (Eq, Ord, Show, Typeable, Data)
data Entity
    = EntityClass ClassIRI
    | EntityDatatype Datatype
    | EntityObjectProperty ObjectPropertyIRI
    | EntityDataProperty DataPropertyIRI
    | EntityAnnotationProperty AnnotationProperty
    | EntityIndividual IndividualIRI deriving (Eq, Ord, Show, Typeable, Data)
data AnnotationValue
    = NodeAT NodeID
    | IriAT IRI
    | LiteralAT Literal deriving (Eq, Ord, Show, Typeable, Data)
data Axiom
    = DeclarationAxiom Annotations Entity
    | AnnotationAxiomDomain Annotations AnnotationProperty IRI
    | AnnotationAxiomRange Annotations AnnotationProperty IRI
    | AnnotationAxiomSubProperty Annotations AnnotationProperty AnnotationProperty
    | AnnotationAxiomAssertion Annotations TotalIRI Annotation
    | DatatypeAxiomDefinition Annotations Datatype DataRange
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
    | AssertionAxiomSameIndividuals Annotations Individual (NonEmpty Individual)
    | AssertionAxiomDifferentIndividuals Annotations Individual (NonEmpty Individual)
    | AssertionAxiomClass Annotations Individual ClassExpression
    | AssertionAxiomObjectProperty Annotations ObjectPropertyExpression Individual Individual
    | AssertionAxiomNegativeObjectProperty Annotations ObjectPropertyExpression Individual Individual 
    | AssertionAxiomDataProperty Annotations DataPropertyExpression Individual Literal
    | AssertionAxiomNegativeDataProperty Annotations DataPropertyExpression Individual Literal deriving (Eq, Ord, Show, Typeable, Data)


data AxiomType
  = DeclarationAxiomType
  | AnnotationAxiomType
  | AnnotationPropAxiomType
  | DatatypeAxiomType
  | ObjectPropAxiomType
  | DataPropAxiomType
  | ClassAxiomType
  | AssertionAxiomType deriving (Show, Eq)

axiomType :: Axiom -> AxiomType
axiomType DeclarationAxiom{}                     = DeclarationAxiomType
axiomType AnnotationAxiomDomain{}                = AnnotationAxiomType
axiomType AnnotationAxiomRange{}                 = AnnotationAxiomType
axiomType AnnotationAxiomSubProperty{}           = AnnotationAxiomType
axiomType AnnotationAxiomAssertion{}             = AnnotationPropAxiomType
axiomType DatatypeAxiomDefinition{}              = DatatypeAxiomType
axiomType ObjectPropAxiomDomain{}                = ObjectPropAxiomType
axiomType ObjectPropAxiomRange{}                 = ObjectPropAxiomType
axiomType ObjectPropAxiomCharacteristics{}       = ObjectPropAxiomType
axiomType ObjectPropAxiomSubProperty{}           = ObjectPropAxiomType
axiomType ObjectPropAxiomChainSubProperty{}      = ObjectPropAxiomType
axiomType ObjectPropAxiomEquivalent{}            = ObjectPropAxiomType
axiomType ObjectPropAxiomDisjoint{}              = ObjectPropAxiomType
axiomType ObjectPropAxiomInverse{}               = ObjectPropAxiomType
axiomType DataPropAxiomDomain{}                  = DataPropAxiomType
axiomType DataPropAxiomRange{}                   = DataPropAxiomType
axiomType DataPropAxiomCharacteristics{}         = DataPropAxiomType
axiomType DataPropAxiomSubProperty{}             = DataPropAxiomType
axiomType DataPropAxiomEquivalent{}              = DataPropAxiomType
axiomType DataPropAxiomDisjoint{}                = DataPropAxiomType
axiomType ClassAxiomSubClassOf{}                 = ClassAxiomType
axiomType ClassAxiomEquivalentClasses{}          = ClassAxiomType
axiomType ClassAxiomDisjointClasses{}            = ClassAxiomType
axiomType ClassAxiomDisjointUnion{}              = ClassAxiomType
axiomType ClassAxiomHasKey{}                     = ClassAxiomType
axiomType AssertionAxiomSameIndividuals{}        = AssertionAxiomType
axiomType AssertionAxiomDifferentIndividuals{}   = AssertionAxiomType
axiomType AssertionAxiomClass{}                  = AssertionAxiomType
axiomType AssertionAxiomObjectProperty{}         = AssertionAxiomType
axiomType AssertionAxiomNegativeObjectProperty{} = AssertionAxiomType
axiomType AssertionAxiomDataProperty{}           = AssertionAxiomType
axiomType AssertionAxiomNegativeDataProperty{}   = AssertionAxiomType


class HasIRI a where
  getIRI :: a -> Maybe IRI

instance HasIRI TotalIRI where
  getIRI (NamedIRI i) = Just i
  getIRI AnonymousIRI{} = Nothing

instance HasIRI ObjectPropertyExpression where
  getIRI (OPE o) = Just o
  getIRI (InverseOPE _) = Nothing

instance HasIRI ClassExpression where
  getIRI (CExpClass c) = Just c
  getIRI _ = Nothing

instance HasIRI Datatype where
  getIRI = Just . _unDatatype

instance HasIRI Axiom where
  getIRI (DeclarationAxiom _ e)                           = getIRI e
  getIRI (AnnotationAxiomDomain _ a _)                    = Just a
  getIRI (AnnotationAxiomRange _ a _)                     = Just a
  getIRI (AnnotationAxiomSubProperty _ a _)               = Just a
  getIRI (AnnotationAxiomAssertion _ t _)                 = getIRI t
  getIRI (DatatypeAxiomDefinition _ d _)                  = getIRI d
  getIRI (ObjectPropAxiomDomain _ o _)                    = getIRI o
  getIRI (ObjectPropAxiomRange _ o _)                     = getIRI o
  getIRI (ObjectPropAxiomCharacteristics _ o _)           = getIRI o
  getIRI (ObjectPropAxiomSubProperty _ o _)               = getIRI o
  getIRI (ObjectPropAxiomChainSubProperty _ _ o)          = getIRI o
  getIRI (ObjectPropAxiomEquivalent _ o _)                = getIRI o
  getIRI (ObjectPropAxiomDisjoint _ o _)                  = getIRI o
  getIRI (ObjectPropAxiomInverse _ o _)                   = getIRI o
  getIRI (DataPropAxiomDomain _ d _)                      = Just d
  getIRI (DataPropAxiomRange _ d _)                       = Just d
  getIRI (DataPropAxiomCharacteristics _ d _)             = Just d
  getIRI (DataPropAxiomSubProperty _ d _)                 = Just d
  getIRI (DataPropAxiomEquivalent _ d _)                  = Just d
  getIRI (DataPropAxiomDisjoint _ d _)                    = Just d
  getIRI (ClassAxiomSubClassOf _ c _)                     = getIRI c
  getIRI (ClassAxiomEquivalentClasses _ c _)              = getIRI c
  getIRI (ClassAxiomDisjointClasses _ c _)                = getIRI c
  getIRI (ClassAxiomDisjointUnion _ c _)                  = Just c
  getIRI (ClassAxiomHasKey _ c _)                         = getIRI c
  getIRI (AssertionAxiomSameIndividuals _ i _)            = getIRI i
  getIRI (AssertionAxiomDifferentIndividuals _ i _)       = getIRI i
  getIRI (AssertionAxiomClass _ i _)                      = getIRI i
  getIRI (AssertionAxiomObjectProperty _ _ i1 i2)         = getIRI i1 <|> getIRI i2
  getIRI (AssertionAxiomNegativeObjectProperty _ _ i1 i2) = getIRI i1 <|> getIRI i2
  getIRI (AssertionAxiomDataProperty _ _ i _)             = getIRI i
  getIRI (AssertionAxiomNegativeDataProperty _ _ i _)     = getIRI i

instance HasIRI Entity where
  getIRI (EntityDatatype d)           = getIRI d
  getIRI (EntityClass c)              = Just c
  getIRI (EntityObjectProperty o)     = Just o
  getIRI (EntityDataProperty d)       = Just d
  getIRI (EntityAnnotationProperty a) = Just a
  getIRI (EntityIndividual i)         = Just i


---------------------------
---- utility functions ----
---------------------------
atLeast2List :: a -> a -> [a] -> AtLeast2List a
atLeast2List x y = (:#) (x, y)

atLeast2List' :: a -> NonEmpty a -> AtLeast2List a
atLeast2List' x nx = let (x' :| xs) = nx in (x, x') :# xs

toList :: AtLeast2List a -> [a]
toList ~((x, y) :# xs) = x : y : xs

--toNonEmptyList :: AtLeast2List a -> NonEmpty a
--toNonEmptyList ~((x, y) :# xs) = x :| (y : xs)

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

-- | Utility function that returns 'True' only if the 'ClassExpression' is about a named
-- class, i.e., is an IRI
filterClassIRI :: ClassExpression -> Bool
filterClassIRI (CExpClass _) = True
filterClassIRI _ = False

-- | Utility function that returns 'True' only if the 'ObjectPropertyExpression' is about a
-- named property, i.e., is an IRI
filterObjectPropIRI :: ObjectPropertyExpression -> Bool
filterObjectPropIRI (OPE _) = True
filterObjectPropIRI _ = False

-- | Utility function that returns 'True' for named resources
filterNamedIRI :: TotalIRI -> Bool
filterNamedIRI (NamedIRI _) = True
filterNamedIRI _ = False

type AxiomState = State [Axiom]

-- | Filters out all axiom of type 'AxiomType' from the state and returns them 
extractAxioms :: (Axiom -> Bool) -> AxiomState [Axiom]
extractAxioms p = do
  axms <- get
  let (sel, rest) = partition p axms
  put rest
  pure sel

-- | group Axioms based on their IRI
groupAxiomsOnIRI :: [Axiom] -> [[Axiom]]
groupAxiomsOnIRI = groupWith getIRI

groupAxiomsOnConstructor :: [Axiom] -> [[Axiom]]
groupAxiomsOnConstructor = groupBy ((==) `on` toConstr)

mapAxiomsOnIRI :: [Axiom] -> Map (Maybe IRI) [Axiom]
mapAxiomsOnIRI as = M.fromListWith (++) pairs
 where
  pairs :: [(Maybe IRI, [Axiom])]
  pairs = zip (getIRI <$> as) (pure <$> as) 


