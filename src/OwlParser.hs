{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module OwlParser where

import           Prelude                           hiding ( exponent )
import           Data.Functor                             ( ($>) )
import           Data.List                                ( intercalate )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
-- import qualified Data.List.NonEmpty            as NEL
import           Data.Maybe                               ( fromMaybe )
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

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


type Parser = Parsec Void String
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

data TypedLiteral = TypedL String String deriving Show
data FloatPoint = FloatP Double (Maybe Exponent)
data LiteralWithLang = LiteralWithLang String LangTag deriving Show

newtype Exponent = Exponent Integer
newtype DecimalLiteral = DecimalL Double deriving Show
newtype IntegerLiteral = IntegerL Integer deriving Show
newtype NodeID = NodeID String deriving Show

data OntologyDocument = OntologyD [PrefixDeclaration] Ontology deriving Show
data PrefixDeclaration = PrefixD PrefixName FullIRI deriving Show
type PrefixName = String
data Ontology = Ontology (Maybe OntologyVersionIRI) [ImportIRI] [AnnotatedList Annotation] [Frame] deriving Show
data OntologyVersionIRI = OntologyVersionIRI OntologyIRI (Maybe VersionIRI) deriving Show
newtype AnnotatedList a = AnnList [(AnnotatedList Annotation, a)]
type Annotations = AnnotatedList Annotation
type Descriptions = AnnotatedList Description
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
type DataRange = NonEmpty DataConjunction
type DataConjunction = NonEmpty DataPrimary
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
newtype DataPropertyExpression = Dpe IRI
data WithNegation a = Positive a | Negative a
type ObjectPropertyExpression = WithNegation IRI
type Description = NonEmpty Conjunction
data Conjunction = ClassConj IRI (NonEmpty (WithNegation Restriction)) | PrimConj (NonEmpty Primary)
data Primary = PrimaryR (WithNegation Restriction) | PrimaryA (WithNegation Atomic)
data Restriction = OPRestriction ObjectPropertyRestriction | DPRestriction DataPropertyRestriction
data ObjectPropertyRestrictionType = OPSome Primary
                                   | OPOnly Primary
                                   | OPValue Individual
                                   | OPSelf
                                   | OPMin Int (Maybe Primary) -- TODO: Int -> Nat
                                   | OPMax Int (Maybe Primary) -- TODO: Int -> Nat
                                   | OPExactly Int (Maybe Primary) -- TODO: Int -> Nat
data DataPropertyRestrictionType = DPSome DataPrimary
                                 | DPOnly DataPrimary
                                 | DPValue Literal
                                 | DPMin Int (Maybe DataPrimary) -- TODO: Int -> Nat
                                 | DPMax Int (Maybe DataPrimary) -- TODO: Int -> Nat
                                 | DPExactly Int (Maybe DataPrimary) -- TODO: Int -> Nat
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
type Fact = WithNegation FactElement
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

-------------------------
---- CLASS INSTANCES ----
-------------------------

instance Show Annotation where
  show (Annotation i s) = unwords [i, show s]

instance (Show a) => Show (AnnotatedList a) where
  show (AnnList []) = ""
  show (AnnList xs) = intercalate ",\n" (go <$> xs)
   where
    go ((AnnList []), x) = show x
    go (al, x) = unwords ["Annotations:", show al, "\n ", show x]

instance Show FloatPoint where
  show (FloatP n me) = concat [show n, maybe "" show me]
instance Show Exponent where
  show (Exponent i) = concat ["e", if i > 0 then "+" else "", show i]

-- | Parses white space and line comments
--
-- parsec (sc *> satisfy (const true)) "    some indented text"
-- "some individual text"
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

-- | Parses the actual lexeme and then any remaining space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

iriParens :: Parser a -> Parser a
iriParens = enclosed '<'

parens :: Parser a -> Parser a
parens = enclosed '('

enclosed :: Char -> Parser a -> Parser a
enclosed c = between (char c) (char (cChar c))

enclosedS :: Char -> Parser a -> Parser a
enclosedS c = between (symbol [c]) (symbol [cChar c])

-- | Reserved keywords
allKeywords :: [String]
allKeywords = concat [datatypeKeywords, entityKeywords, ontologyKeywords, propertyKeywords]

datatypeKeywords :: [String]
datatypeKeywords = ["integer", "decimal", "float", "string"]

-- c for complement
cChar :: Char -> Char
cChar = \case
  '{' -> '}'
  '}' -> '{'
  '<' -> '>'
  '>' -> '<'
  '(' -> ')'
  ')' -> '('
  '[' -> ']'
  ']' -> '['
  c   -> c

entityKeywords :: [String]
entityKeywords =
  ["Datatype", "Class", "ObjectProperty", "DataProperty", "AnnotationProperty", "NamedInvividual"]

ontologyKeywords :: [String]
ontologyKeywords = ["Annotations", "Prefix", "Ontology", "Import"]

propertyKeywords :: [String]
propertyKeywords =
  [ "inverse"
  , "or"
  , "and"
  , "not"
  , "length"
  , "minLength"
  , "maxLength"
  , "pattern"
--  , "<="
--  , "<"
--  , ">="
--  , ">"
  ]

-- | Parses the symbol and then any remaining space
--
-- >>> parseTest (symbol "a symbol" *> symbol "and a second") "a symbol    and a second"
-- "and a second"
--
symbol :: String -> Parser String
symbol = L.symbol sc

-- | It parses zero
--
-- >>> parseTest zero "0"
-- '0'
zero :: Parser Char
zero = char '0'

-- | It parses non positive integer
--
-- >>> parseTest nonZero "1"
-- '1'
nonZero :: Parser Char
nonZero = notFollowedBy zero *> digitChar

digit :: Parser Char
digit = zero <|> nonZero

digits :: Parser String
digits = some digit

-- | It parses positive integer
--
-- >>> parseTest positiveInteger "13"
-- 13
positiveInteger :: Parser Integer
positiveInteger = do
  fd  <- nonZero
  rem <- many digit
  return $ read (fd : rem)

-- | It parses non negativeinteger
--
-- >>> parseTest nonNegativeInteger "13"
-- 13
-- >>> parseTest nonNegativeInteger "0"
-- 0
nonNegativeInteger :: Parser Integer
nonNegativeInteger = (0 <$ zero) <|> positiveInteger

-- | It may parse a sign or no sign at all
--
-- >>> parseTest sign "+"
-- "" 
-- >>> parseTest sign "-"
-- "-"
-- >>> parseTest sign ""
-- ""
sign :: Parser String
sign = do
  mSign <- optional $ "" <$ symbol "+" <|> symbol "-"
  return $ fromMaybe "" mSign

-- | It parses arbitrary alpharithmetics provived that it does not belong to
-- the list of reserved keywords
--
-- >>> parseTest identifier "label"
-- "label"
--
-- >>> parseTest identifier "label3With"
-- "label3With"
--
-- >>> parseTest identifier "label_3_With"
-- "label_3_With"
--
-- >>> parseTest identifier "1label"
-- ...
-- unexpected '1'
-- expecting letter
--
-- >>> parseTest identifier "Ontology"
-- ...
-- keyword "Ontology" cannot be an identifier
identifier :: Parser String
identifier = lexeme identifier_

-- | It parses arbitrary alpharithmetics provived that it does not belong to
-- the list of reserved keywords. It does not parse any space after the identifier
identifier_ :: Parser String
identifier_ = try (anyIdentifier_ >>= check)
 where
  check x =
    if x `elem` allKeywords then fail $ concat ["keyword ", show x, " cannot be an identifier"] else return x

-- | It parses arbitrary alpharithmetics. It does not parse any space after the identifier
anyIdentifier_ :: Parser String
anyIdentifier_ = try p where p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

-- | It parses arbitrary alpharithmetics. It parses any space after the identifier
anyIdentifier :: Parser String
anyIdentifier = lexeme anyIdentifier_

-- | It parses prefix names
--
-- >>> parseTest prefixName "owl:"
-- "owl"
--
-- >>> parseTest prefixName ":"
-- ""
--
-- >>> parseTest prefixName "owl    :"
-- ...
-- unexpected space
-- ...
--
prefixName :: Parser String
prefixName = (identifier_ <|> pure "") <* string ":"

-- | TODO currently IRI is defined as text inside <>
-- No validation is being performed
-- Check: http://www.rfc-editor.org/rfc/rfc3987.txt for BNF representation 
--
-- >>> parseTest fullIRI "<http://www.uom.gr/ai/TestOntology.owl#Child>"
-- "http://www.uom.gr/ai/TestOntology.owl#Child"
--
-- >>> parseTest fullIRI "<http://www.uom.gr/ai/TestOntology.owl#Child"
-- ...
-- unexpected end of input
-- expecting '>'
--
-- >>> parseTest fullIRI "http://www.uom.gr/ai/TestOntology.owl#Child"
-- ...
-- unexpected 'h'
-- expecting '<'
fullIRI :: Parser IRI
fullIRI = lexeme . iriParens $ takeWhileP Nothing (/= '>')

-- | It parses abbreviated IRIs. Format: 'prefix:term'
-- >>> parseTest abbreviatedIRI "xsd:string"
-- ("xsd","string")
--
-- >>> parseTest abbreviatedIRI "owl:   user1"
-- ...
-- unexpected space
-- ...
--
abbreviatedIRI :: Parser (PrefixName, String)
abbreviatedIRI = (,) <$> prefixName <*> anyIdentifier

-- | It parses simple IRIs; a finite sequence of characters matching the PN_LOCAL
-- production of [SPARQL] and not matching any of the keyword terminals of the syntax 
--
-- TODO: Simplified to a simple identifier parser
-- >>> parseTest simpleIRI "John"
-- "John"
--
simpleIRI :: Parser String
simpleIRI = identifier

-- | It parses any of the three different formats of IRIs
-- >>> parseTest iri "John"
-- "John"
--
iri :: Parser String
iri = fullIRI <|> (concatAbbrIRI <$> try abbreviatedIRI) <|> simpleIRI
  where concatAbbrIRI (x, y) = concat [x, ":", y]

-- | It parses class IRIs
classIRI :: Parser IRI
classIRI = iri

-- | It parses datatypes
--
-- >>> parseTest datatype "<http://example.iri>"
-- "http://example.iri"
--
-- >>> parseTest datatype "integer"
-- "integer"
--
-- >>> parseTest datatype "xsd:string"
-- "xsd:string"
--
datatype :: Parser String
datatype = try datatypeIRI <|> symbol "integer" <|> symbol "decimal" <|> symbol "float" <|> symbol "string"

datatypeIRI :: Parser String
datatypeIRI = iri

objectPropertyIRI :: Parser String
objectPropertyIRI = iri

dataPropertyIRI :: Parser String
dataPropertyIRI = iri

annotationPropertyIRI :: Parser String
annotationPropertyIRI = iri

individual :: Parser String
individual = individualIRI <|> (show <$> nodeID)

individualIRI :: Parser String
individualIRI = iri

-- | It parses blank nodes
--
-- >>> parseTest nodeID "_:blank"
-- NodeID "blank"
--
-- >>> parseTest nodeID ":blank"
-- ...
-- unexpected ":b"
-- expecting "_:"
--
-- >>> parseTest nodeID "blanknode"
-- ...
-- unexpected "bl"
-- expecting "_:"
--
nodeID :: Parser NodeID
nodeID = NodeID <$> (symbol "_:" *> identifier)

-- | It parses literals
--
-- >>> parseTest literal "\"32\"^^integer"
-- "TypedL \"32\" \"integer\""
--
-- >>> parseTest literal "\"stringLiteralNoLanguage\""
-- "stringLiteralNoLanguage"
--
-- >>> parseTest literal "\"stringLiteralWithLang\"@en"
-- "LiteralWithLang \"stringLiteralWithLang\" \"en\""
--
literal :: Parser String
literal =
  lexeme
    $   (show <$> try typedLiteral)
    <|> (show <$> try stringLiteralWithLanguage)
    <|> (try stringLiteralNoLanguage)
    <|> (show <$> try integerLiteral)
    <|> (show <$> try decimalLiteral)
    <|> (show <$> try floatingPointLiteral)

-- | It parses a typed literal
--
-- >>> parseTest typedLiteral "\"32\"^^integer"
-- TypedL "32" "integer"
--
-- >>> parseTest typedLiteral "\"Jack\"^^xsd:string"
-- TypedL "Jack" "xsd:string"
--
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue <*> (symbol "^^" *> datatype)

-- | It parses a string value with no language tag
--
-- >>> parseTest stringLiteralNoLanguage "\"hello there\""
-- "hello there"
--
stringLiteralNoLanguage :: Parser String
stringLiteralNoLanguage = quotedString


-- | It parses a string value with language tag
--
-- >>> parseTest stringLiteralWithLanguage "\"hello there\"@en"
-- LiteralWithLang "hello there" "en"
--
stringLiteralWithLanguage :: Parser LiteralWithLang
stringLiteralWithLanguage = LiteralWithLang <$> quotedString <*> languageTag

-- | It parse a language tag
--
-- parseTest languageTag "@en"
-- "en"
-- parseTest languageTag "en"
-- ...
-- unexpected 'e'
-- expecting '@'
--
-- TODO: check for valid lang tags
languageTag :: Parser LangTag
languageTag = char '@' *> identifier_ -- (U+40) followed a nonempty sequence of characters matching the langtag production from [BCP 47]

lexicalValue :: Parser String
lexicalValue = quotedString

-- | It parses a string enclosed in double quotes
--
-- >>> parseTest quotedString "\"this is a quoted string\""
-- "this is a quoted string"
-- >>> parseTest quotedString "\"this is \\\"test \\\" message\"" 
-- "this is \\\"test \\\" message"
quotedString :: Parser String
quotedString = do
  char '\"'
  strings <- many chars
  char '\"'
  return $ concat strings
 where
  chars     = fmap return nonEscape <|> escape
  nonEscape = noneOf "\\\"\0\n\r\v\t\b\f" -- all the characters that can be escaped
  escape    = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf"
    return [d, c]


-- | It parses folating point numbers.
-- Valid formats:
--   * 12F
--   * .3f
--   * 12.3f
--   * -12.3F
--
-- >>> parseTest floatingPointLiteral "12F"
-- 12.0
-- >>> parseTest floatingPointLiteral "12.3f"
-- 12.3
-- >>> parseTest floatingPointLiteral "-12.332F"
-- -12.332
-- >>> parseTest floatingPointLiteral ".3f"
-- 0.3
-- >>> parseTest floatingPointLiteral ".3e10f"
-- 0.3e+10
-- >>> parseTest floatingPointLiteral "-12.3e-10F"
-- -12.3e-10
floatingPointLiteral :: Parser FloatPoint
floatingPointLiteral = do
  sgn  <- sign
  dgts <- dig1 <|> dig2
  mExp <- optional exponent
  symbol "f" <|> symbol "F"
  return $ FloatP (read (sgn ++ dgts)) mExp
 where
  dig1 = do
    dg'  <- digits
    mDec <- optional $ do
      symbol "."
      dg <- digits
      return $ '.' : dg
    let dc = fromMaybe "" mDec
    return $ dg' ++ dc
  dig2 = do
    _    <- symbol "."
    dgts <- digits
    return $ "0." ++ dgts


-- | It parses an exponent
--
-- >>> parseTest exponent "e10"
-- e+10
-- >>> parseTest exponent "E10"
-- e+10
-- >>> parseTest exponent "e+10"
-- e+10
-- >>> parseTest exponent "E+10"
-- e+10
-- >>> parseTest exponent "e-10"
-- e-10
-- >>> parseTest exponent "E-10"
-- e-10
exponent :: Parser Exponent
exponent = do
  symb <- symbol "e" <|> symbol "E"
  ms   <- sign
  dgts <- digits
  return . Exponent . read . concat $ [ms, dgts]

-- | It parser decimal values
--
-- >>> parseTest decimalLiteral "10.345"
-- DecimalL 10.345
-- >>> parseTest decimalLiteral "-10.345"
-- DecimalL (-10.345)
-- >>> parseTest decimalLiteral "+10.345"
-- DecimalL 10.345
decimalLiteral :: Parser DecimalLiteral
decimalLiteral = do
  mSign <- sign
  dig1  <- digits
  dig2  <- symbol "." >> digits
  return . DecimalL . read . concat $ [mSign, dig1, ".", dig2]

-- | It parser integer values
--
-- >>> parseTest integerLiteral "10"
-- IntegerL 10
-- >>> parseTest integerLiteral "-10"
-- IntegerL (-10)
-- >>> parseTest integerLiteral"+10"
-- IntegerL 10
integerLiteral :: Parser IntegerLiteral
integerLiteral = do
  mSign <- sign
  digs  <- digits
  return . IntegerL . read . concat $ [mSign, digs]

entity :: Parser String
entity = choice $ fmap (uncurry classParser) alts
 where
  classParser :: String -> Parser String -> Parser String
  classParser s p = do
    smb  <- symbol s
    name <- p
    return . concat $ ["<", s, ">"]
  alts :: [(String, Parser String)]
  alts =
    [ ("Datatype"          , datatype)
    , ("Class"             , classIRI)
    , ("ObjectProperty"    , objectPropertyIRI)
    , ("DataProperty"      , dataPropertyIRI)
    , ("AnnotationProperty", annotationPropertyIRI)
    , ("NamedIndividual"   , individualIRI)
    ]


------------------------------
-- Ontology and Annotations --
------------------------------

-- | It parses annotations
--
-- >>> :{
-- let input :: [String]
--     input =
--      [ "Annotations: creator \"John\","
--      , "             Annotations: rdfs:comment \"Creation Year\" creationYear 2008,"
--      , "             mainClass Person"
--      ]
-- :}
--
-- >>> parseTest annotations (unlines input)
-- creator "John",
-- Annotations: rdfs:comment "Creation Year"
--   creationYear "IntegerL 2008",
-- mainClass "Person"
--
annotations :: Parser (AnnotatedList Annotation)
annotations = symbol "Annotations:" *> annotatedList annotation

-- | It parses a single annotation
--
-- >>> parseTest annotation ":creator \"john\""
-- :creator "john"
--
annotation :: Parser Annotation
annotation = Annotation <$> annotationPropertyIRI <*> annotationTarget

-- | It parser node ids, iris or literals
--
-- >>> parseTest annotationTarget "\"john\""
-- "john"
--
-- >>> parseTest annotationTarget "John"
-- "John"
--
-- >>> parseTest annotationTarget "_:node"
-- "NodeID \"node\""
--
-- >>> parseTest annotationTarget "<http://some.iri>"
-- "http://some.iri"
--
annotationTarget :: Parser String
annotationTarget = (show <$> try nodeID) <|> (try iri) <|> (try literal)

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyD <$> many prefixDeclaration <*> ontology

-- | It parses prefix names. Format: 'Prefix: <name>: <IRI>
-- >>> parseTest prefixDeclaration "Prefix: g: <http://ex.com/owl2/families#>"
-- PrefixD "g" "http://ex.com/owl2/families#"
--
-- >>> parseTest prefixDeclaration "Prefix: : <http://ex.com/owl/families#>"
-- PrefixD "" "http://ex.com/owl/families#"
--
prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = PrefixD <$> (symbol "Prefix:" *> lexeme prefixName) <*> fullIRI

ontology :: Parser Ontology
ontology = do
  symbol "Ontology:"
  ontoIRI <- optional $ OntologyVersionIRI <$> ontologyIRI <*> optional versionIRI -- Maybe (iri, Maybe iri)
  imports <- many importStmt
  annots  <- annotations
  frames  <- many frame
  return $ Ontology ontoIRI imports [annots] frames

ontologyIRI :: Parser IRI
ontologyIRI = iri

versionIRI :: Parser IRI
versionIRI = iri

-- | It parses import statements
--
-- >>> parseTest importStmt "Import: <http://ex.com/owl2/families.owl>"
-- "http://ex.com/owl2/families.owl"
--
importStmt :: Parser ImportIRI
importStmt = symbol "Import:" *> iri

frame :: Parser String
frame =
  datatypeFrame
    <|> classFrame
    <|> objectPropertyFrame
    <|> dataPropertyFrame
    <|> annotationPropertyFrame
    <|> individualFrame
    <|> misc


-------------------------------------------
--- Properties and datatype expressions ---
-------------------------------------------

data ObjectProperty = ObjectP IRI | InverseObjectP IRI deriving Show
newtype DataProperty = DataP IRI deriving Show
-- data DataPrimary = DataPr DataAtomic | DataPrNot DataAtomic deriving Show

-- | It parses an object property expression
--
-- >>> parseTest objectPropertyExpression "<http://object-property-iri.com>"
-- ObjectP "http://object-property-iri.com"
--
-- >>> parseTest objectPropertyExpression "inverse <http://object-property-iri.com>"
-- InverseObjectP "http://object-property-iri.com"
-- 
objectPropertyExpression :: Parser ObjectProperty
objectPropertyExpression = (ObjectP <$> objectPropertyIRI) <|> inverseObjectProperty

-- | It parses an inverse object property expression
--
-- >>> parseTest inverseObjectProperty "inverse <http://object-property-iri.com>"
-- InverseObjectP "http://object-property-iri.com"
-- 
inverseObjectProperty :: Parser ObjectProperty
inverseObjectProperty = symbol "inverse" *> (InverseObjectP <$> objectPropertyIRI)

-- | It parses a data property expression
--
-- >>> parseTest dataPropertyExpression "<http://object-property-iri.com>"
-- DataP "http://object-property-iri.com"
--
dataPropertyExpression :: Parser DataProperty
dataPropertyExpression = DataP <$> dataPropertyIRI

-- | It parses a data range
--
-- >>> parseTest dataRange "integer[>10] and integer[<20] or integer[>100]"
-- "integer > IntegerL 10 and integer < IntegerL 20 or integer > IntegerL 100"
--
dataRange :: Parser String
dataRange = intercalate " or " <$> singleOrMany "or" dataConjunction

-- | It parses a data conjunction (i.e. 'and')
--
-- >>> parseTest dataConjunction "integer[<10] and integer[>0]"
-- "integer < IntegerL 10 and integer > IntegerL 0"
--
dataConjunction :: Parser String
dataConjunction = intercalate " and " <$> singleOrMany "and" dataPrimary

-- | It parses a data primary
--
-- >>> parseTest dataPrimary "integer[<0]"
-- "integer < IntegerL 0"
--
dataPrimary :: Parser String
dataPrimary = do
  neg <- optionalNegation
  da  <- dataAtomic
  pure $ concat [fromMaybe "" neg, da]

-- | It parses an atomic data
--
-- >>> parseTest dataAtomic  "integer[<0]"
-- "integer < IntegerL 0"
--
dataAtomic :: Parser String
dataAtomic =
  try datatypeRestriction
    <|> try datatype
    <|> unwords
    <$> enclosedS '{' literalList
    <|> enclosedS '(' dataRange

-- | It parsers a non empty list of literal
--
-- >>> parseTest literalList "\"kostas\", 32, \"true\""
-- ["kostas","IntegerL 32","true"]
--
literalList :: Parser [String]
literalList = nonEmptyList literal

-- | It parses datatype restrictions
--
-- >>> parseTest datatypeRestriction "integer[> 0, maxLength 2]"
-- "integer > IntegerL 0 AND integer maxLength IntegerL 2"
--
-- >>> parseTest datatypeRestriction "integer[< 0]"
-- "integer < IntegerL 0"
--
datatypeRestriction :: Parser String
datatypeRestriction = do
  dt <- datatype
  symbol "["
  rvList <- nonEmptyList ((,) <$> facet <*> restrictionValue)
  symbol "]"
  let res = fmap (\(f, v) -> unwords [dt, f, v]) rvList
  return $ intercalate " AND " res

facet :: Parser String
facet =
  choice $ fmap symbol ["length", "maxLength", "minLength", "pattern", "langRange", "<=", "<", ">=", ">"]

restrictionValue :: Parser String
restrictionValue = literal

predifinedPrefixex :: [PrefixDeclaration]
predifinedPrefixex =
  [ PrefixD "rdf"  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  , PrefixD "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
  , PrefixD "xsd"  "http://www.w3.org/2001/XMLSchema#"
  , PrefixD "owl"  "http://www.w3.org/2002/07/owl#"
  ]


---------------------
---  Descriptions ---
---------------------

-- | It parses a description
--
-- >>> parseTest (description >> eof) "Man"
-- ()
--
-- >>> parseTest (description >> eof) "Man or Woman"
-- ()
--
-- >>> parseTest (description >> eof) "hasFirstName value \"John\" or Man"
-- ()
--
-- >>> parseTest (description >> eof) "hasFirstName value \"John\" or hasFirstName value \"Jack\"^^xsd:string"
-- ()
--
description :: Parser [String]
description = singleOrMany "or" conjunction

-- | It parses a conjunction
--
-- >>> parseTest conjunction "Person and Man"
-- "Person Man"
--
-- >>> parseTest conjunction "Person"
-- "Person"
--
-- >>> parseTest (conjunction >> eof) "owl:Thing that hasFirstName exactly 1"
-- ()
--
conjunction :: Parser String
conjunction = try restrictions <|> try (unwords <$> singleOrMany "and" primary)
 where
  restrictions = do
    clsIRI <- classIRI
    symbol "that"
    neg  <- optionalNegation
    rst  <- restriction
    rsts <- many $ do
      symbol "and"
      neg' <- optionalNegation
      rst' <- restriction
      pure . unwords . filter (not . null) $ [fromMaybe "" neg', rst']
    pure . unwords . filter (not . null) $ [clsIRI, fromMaybe "" neg, rst] ++ rsts

-- | It parses a primary
--
-- >>> parseTest primary "not Person"
-- "not Person"
--
-- >>> parseTest primary "not hasFirstName value \"John\""
-- "not John"
--
primary :: Parser String
primary = do
  neg  <- optionalNegation
  rOrA <- try restriction <|> (try (unwords <$> atomic))
  pure . unwords . filter (not . null) $ [fromMaybe "" neg, rOrA]

-- | It parses one of the many differnt type of restrictions on object or data properties
--
-- >>> parseTest (restriction >>eof) "hasFirstName value \"John\""
-- ()
--
-- >>> parseTest (restriction >> eof) "hasFirstName exactly 1"
-- ()
--
-- >>> parseTest (restriction >> eof) "hasFirstName only string[minLength 1]"
-- ()
--
restriction :: Parser String
restriction = lexeme . choice $ try <$> objectExprParsers <> dataExprParsers
 where
  objExprs =
    [ ("some"   , primary)
    , ("only"   , primary)
    , ("Self"   , pure "")
    , ("min", show <$> nonNegativeInteger <* optional primary)
    , ("max", show <$> nonNegativeInteger <* optional primary)
    , ("exactly", show <$> nonNegativeInteger <* optional primary)
    ]
  dataExprs =
    [ ("some"   , dataPrimary)
    , ("only"   , dataPrimary)
    , ("value"  , literal)
    , ("min", show <$> nonNegativeInteger <* optional dataPrimary)
    , ("max", show <$> nonNegativeInteger <* optional dataPrimary)
    , ("exactly", show <$> nonNegativeInteger <* optional dataPrimary)
    ]
  -- TODO: now, i keep only the last parser!
  objectExprParsers = (\(smb, p) -> objectPropertyExpression *> symbol smb *> p) <$> objExprs
  dataExprParsers   = (\(smb, p) -> dataPropertyExpression *> symbol smb *> p) <$> dataExprs

-- | It parses a class IRI or a list of individual IRIs
--
-- >>> parseTest atomic "<class.iri>"
-- ["class.iri"]
--
-- >>> parseTest atomic "Person"
-- ["Person"]
--
-- >>> parseTest atomic "{ <class.iri#ind1>, <class.iri#ind2> }"
-- ["class.iri#ind1","class.iri#ind2"]
--
atomic :: Parser [String]
atomic = pure <$> classIRI <|> enclosedS '{' (nonEmptyList individual)


--------------------------------
--- Frames and Miscellaneous ---
--------------------------------


-- | It parses a datatype frame
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--         "Datatype: NegInt"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "EquivalentTo: Annotations: rdf:comment \"General Domain\" integer[< 0]"
--       ]
-- :}
--
-- >>> parseTest (datatypeFrame >> eof) (unlines input)
-- ()
--
datatypeFrame :: Parser String
datatypeFrame = do
  dttp    <- symbol "Datatype:" *> datatype
  annots  <- many $ symbol "Annotations:" *> annotatedList annotation
  equiv   <- optional $ symbol "EquivalentTo:" *> annotations <* dataRange -- TODO: in the specifications the EquivalentTo *should always* followed by the "Annotations:" string. However this may be an error, as a later example the EquivalentTo is not followed by any annotation
  annots' <- many $ symbol "Annotations:" *> annotatedList annotation
  pure "<data-frame>"

-- | It parses a class frame
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--         "Class: Person"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "  SubClassOf: owl:Thing that hasFirstName exactly 1 and hasFirstName only string[minLength 1]"
--       , "  SubClassOf: hasAge exactly 1 and hasAge only not NegInt"
--       , "  SubClassOf: hasGender exactly 1 and hasGender only {female , male}"
--       , "  SubClassOf: hasSSN max 1, hasSSN min 1"
--       , "  SubClassOf: not hates Self"
--       , "  EquivalentTo: g:People"
--       , "  DisjointWith: g:Rock , g:Mineral"
--       , "  DisjointUnionOf: Child, Adult"
--       , "  HasKey: hasSSN"
--       ]
-- :}
--
-- >>> parseTest (classFrame >> eof) (unlines input)
-- ()
--
classFrame :: Parser String
classFrame = do
  clsIRI <- symbol "Class:" *> classIRI
  blob   <- (unwords <$> many fstChoice) <* optional sndChoice -- TODO-check: in specs `sndChoice` aka `HasKey` is an alternative to the others
  pure "<class-frame>"
 where
  fstChoice =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "SubClassOf:" *> annotatedList description $> "<subclass-description>")
      <|> (symbol "EquivalentTo:" *> annotatedList description $> "<equivalent-to-description>")
      <|> (symbol "DisjointWith:" *> annotatedList description $> "<disjoint-with-descriptnio>")
      <|> (  symbol "DisjointUnionOf:"
          *> optional annotations -- TODO-check: in spec `annotations` are not optional
          *> listOfAtLeast2 description
          $> "<disjoin-union-description>"
          )
  sndChoice = do
    symbol "HasKey:"
    annots <- optional annotations -- TODO-check: in spec `annotations` are not optional
    mExpr  <-
      nonEmptyList
      $   (objectPropertyExpression $> "<objectPropertyExpression>")
      <|> (dataPropertyExpression $> "<dataPropertyExpression>")
    pure "<sndChoice>"

-- | It parses an object property
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--         "ObjectProperty: hasWife"
--       , "  Annotations: rdfs:comment \"haswife object property\""
--       , "  Characteristics: Functional, InverseFunctional, Reflexive, Irreflexive, Asymmetric, Transitive"
--       , "  Domain: Annotations: rdfs:comment \"General domain\","
--       , "                       creator John "
--       , "            Person, "
--       , "          Annotations: rdfs:comment \"More specific domain\""
--       , "            Man"
--       , "  Range: Person, Woman"
--       , "  SubPropertyOf: hasSpouse, loves"
--       , "  EquivalentTo: isMarriedTo"
--       , "  DisjointWith: hates"
--       , "  InverseOf: hasSpouse, inverse hasSpouse"
--       , "  SubPropertyChain: Annotations: rdfs:comment \"property chain\" hasChild o hasParent"
--       ]
-- :}
--
-- >>> parseTest objectPropertyFrame (unlines input)
-- "hasWife:9"
--
objectPropertyFrame :: Parser String
objectPropertyFrame = do
  objPropIRI <- symbol "ObjectProperty:" *> objectPropertyIRI
  blob       <- length <$> many altr
  pure $ concat [objPropIRI, ":", show blob]
 where
  altr =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "Domain:" *> annotatedList description $> "<domain-description>")
      <|> (symbol "Range:" *> annotatedList description $> "<range-description>")
      <|> (  symbol "Characteristics:"
          *> annotatedList objectPropertyCharacteristic
          $> "<characteristics-props>"
          )
      <|> (symbol "SubPropertyOf:" *> annotatedList objectPropertyExpression $> "<sub-property-of-expr>")
      <|> (symbol "EquivalentTo:" *> annotatedList objectPropertyExpression $> "<equivalent-to-expr>")
      <|> (symbol "DisjointWith:" *> annotatedList objectPropertyExpression $> "<disjoin-with-expr>")
      <|> (symbol "InverseOf:" *> annotatedList objectPropertyExpression $> "<inverse-of-expr>")
      <|> (  symbol "SubPropertyChain:"
          *> annotations -- TODO: is it really required?
          *> objectPropertyExpression
          *> nonEmptyList (symbol "o" *> objectPropertyExpression)
          $> "<disjoin-union-description>"
          )


-- | It parses one of the permitted object property characteristics
--
-- >>> parseTest objectPropertyCharacteristic "InverseFunctional"
-- "InverseFunctional"
--
-- >>> parseTest objectPropertyCharacteristic "Functionalandmore"
-- "Functional"
--
-- >>> parseTest objectPropertyCharacteristic "Random"
-- ...
-- unexpected "Random"
-- ...
--
objectPropertyCharacteristic :: Parser String
objectPropertyCharacteristic =
  let charNames =
        [ "Functional"
        , "InverseFunctional"
        , "Reflexive"
        , "Irreflexive"
        , "Symmetric"
        , "Asymmetric"
        , "Transitive"
        ]
  in  choice $ symbol <$> charNames


-- | It parses an data property
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--         "DataProperty: hasAge"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "Characteristics: Functional"
--       , "Domain: Person , Woman"
--       , "Range: integer"
--       , "SubPropertyOf: hasVerifiedAge , hasSomeAge"
--       , "EquivalentTo: hasAgeInYears"
--       , "DisjointWith: hasSSN"
--       ]
-- :}
--
-- >>> parseTest dataPropertyFrame (unlines input)
-- "hasAge:7"
--
dataPropertyFrame :: Parser String
dataPropertyFrame = do
  dataPropIRI <- symbol "DataProperty:" *> dataPropertyIRI
  blob        <- length <$> many altr
  return $ concat [dataPropIRI, ":", show blob]
 where
  altr =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "Domain:" *> annotatedList description $> "<domain-description>")
      <|> (symbol "Range:" *> annotatedList dataRange $> "<range-description>")
      <|> (  symbol "Characteristics:"
          *> (optional annotations)
          *> symbol "Functional"
          $> "<characteristics-props>"
          )
      <|> (symbol "SubPropertyOf:" *> annotatedList dataPropertyExpression $> "<sub-property-of-expr>")
      <|> (symbol "EquivalentTo:" *> annotatedList dataPropertyExpression $> "<equivalent-to-expr>")
      <|> (symbol "DisjointWith:" *> annotatedList dataPropertyExpression $> "<disjoin-with-expr>")


-- | It parses an annotation property
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--         "AnnotationProperty: creator"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "Domain: Person , Woman"
--       , "Range: <integer>" -- FIX www: range cannot be a datatype -> it's IRI
--       , "SubPropertyOf: hasVerifiedAge , hasSomeAge"
--       ]
-- :}
--
-- >>> parseTest annotationPropertyFrame (unlines input)
-- "creator:4"
--
annotationPropertyFrame :: Parser String
annotationPropertyFrame = do
  annPropIRI <- symbol "AnnotationProperty:" *> annotationPropertyIRI
  blob       <- length <$> many altr
  pure $ concat [annPropIRI, ":", show blob]
 where
  altr =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "Domain:" *> annotatedList iri $> "<domain-description>")
      <|> (symbol "Range:" *> annotatedList iri $> "<range-description>")
      <|> (symbol "SubPropertyOf:" *> annotatedList annotationPropertyIRI $> "<sub-property-of-expr>")

-- | It parses an individual frame
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--        "Individual: John"
--       , "  Annotations: rdfs:creator \"John\""
--       , "  Types: Person, hasFirstName value \"John\" or hasFirstName value \"Jack\"^^xsd:string"
--       , "  Facts: hasWife Mary, not hasChild Susan, hasAge 33, hasChild _:child1"
--       , "  DifferentFrom: Susan"
--       , "  SameAs: Jack , Bob"
--       ]
--     input2 :: [String]
--     input2 = "Individual: _:child1":(tail input)
-- :}
--
-- >>> parseTest individualFrame (unlines input)
-- "John:5"
--
-- >>> parseTest individualFrame (unlines input2)
-- "NodeID \"child1\":5"
--
individualFrame :: Parser String
individualFrame = do
  indi <- symbol "Individual:" *> individual
  blob <- length <$> many altr
  pure $ concat [indi, ":", show blob]
 where
  altr =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "Types:" *> annotatedList description $> "<types-description>")
      <|> (symbol "Facts:" *> annotatedList fact $> "<facts-description>")
      <|> (symbol "SameAs:" *> annotatedList individual $> "<same-as-individual>")
      <|> (symbol "DifferentFrom:" *> annotatedList individual $> "<different-from-individual>")

fact :: Parser String
fact = do
  neg  <- optionalNegation
  fact <- try objectPropertyFact <|> try dataPropertyFact
  return . unwords $ [fromMaybe "" neg, fact]

objectPropertyFact :: Parser String
objectPropertyFact = do
  objProp <- objectPropertyIRI
  indv    <- individual
  return . unwords $ [objProp, indv]

dataPropertyFact :: Parser String
dataPropertyFact = do
  dataProp <- dataPropertyIRI
  ltr      <- literal
  return . unwords $ [dataProp, ltr]

-- | It parses an class miscelaneous properties
--
-- >>> :{
-- let input :: [String]
--     input =
--       [ 
--         "DisjointClasses: g:Rock, g:Scissor, g:Paper"
--       , "EquivalentProperties: hates, loathes, despises"
--       , "DisjointProperties: hates, loves, indifferent"
--       , "EquivalentProperties: favoriteNumber, g:favouriteNumber, g:favouriteInteger"
--       , "DisjointProperties: favoriteInteger, favouriteReal"
--       , "SameIndividual: John, Jack, Joe, Jim"
--       , "DifferentIndividuals: John, Susan, Mary, Jill"
--       ]
-- :}
--
-- >>> parseTest (many misc >> eof) (unlines input)
-- ()
--
misc :: Parser String
misc =
  (  symbol "EquivalentClasses:"
    *> optional annotations -- TODO-check
    *> listOfAtLeast2 description
    $> "<equivalent-classes>"
    )
    <|> (  symbol "DisjointClasses:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 description
        $> "<disjoint-classes>"
        )
    <|> (  symbol "EquivalentProperties:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 objectPropertyIRI
        $> "<equivalent-object-property>"
        )
    <|> (  symbol "DisjointProperties:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 objectPropertyIRI
        $> "<disjoint-object-property>"
        )
    <|> (  symbol "EquivalentProperties:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 dataPropertyIRI
        $> "<equivalent-object-property>"
        )
    <|> (  symbol "DisjointProperties:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 dataPropertyIRI
        $> "<disjoint-object-property>"
        )
    <|> (  symbol "SameIndividual:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 individual
        $> "<same-individual>"
        )
    <|> (  symbol "DifferentIndividuals:"
        *> optional annotations -- TODO-check
        *> listOfAtLeast2 individual
        $> "<different-individual>"
        )

-----------------------
--- Generic parsers ---
-----------------------

optionalNegation :: Parser (Maybe String)
optionalNegation = optional . symbol $ "not"

-- | It parser one or more elements parsed by the input parser p and separated by the input string
--
-- >>> parseTest (singleOrMany "," . string $ "test") "test"
-- ["test"]
--
-- >>> parseTest (singleOrMany "or" . lexeme . string $ "test") "test or test or test"
-- ["test","test","test"]
--
singleOrMany :: String -> Parser p -> Parser [p]
singleOrMany sep p =
  let multipleP = (:) <$> p <*> some (symbol sep *> p) in (try multipleP) <|> (pure <$> p)

-- | It parses non empty lists
--
-- >>> parseTest (nonEmptyList languageTag) "@en, @el, @test"
-- ["en","el","test"]
--
-- >>> parseTest (nonEmptyList languageTag) ""
-- ...
-- unexpected end of input
-- expecting '@'
--
nonEmptyList :: Parser p -> Parser [p]
nonEmptyList p = (:) <$> p <*> many (symbol "," *> p <* sc)

-- | It parses lists with at least two elements
--
-- >>> parseTest (listOfAtLeast2 languageTag) "@en, @el, @test"
-- ["en","el","test"]
--
-- >>> parseTest (listOfAtLeast2 languageTag) "@en"
-- ...
-- unexpected end of input
-- ...
--
listOfAtLeast2 :: Parser p -> Parser [p]
listOfAtLeast2 p = (:) <$> p <*> some (symbol "," >> p)

-- | It parses non empty annotated lists
--
-- >>> parseTest (annotatedList description) "Man, Person"
-- ...
--
annotatedList :: Parser p -> Parser (AnnotatedList p)
annotatedList p =
  let annotationList = (,) <$> fmap (fromMaybe (AnnList [])) (optional annotations) <*> p
  in  AnnList <$> nonEmptyList annotationList

