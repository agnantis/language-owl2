{-# LANGUAGE LambdaCase #-}

module OwlParser where

import           Prelude                           hiding ( exponent )
import           Data.List                                ( intercalate )
import           Data.Maybe                               ( fromMaybe )
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

---- TYPES ----
type Parser = Parsec Void String
type LangTag = String
type IRI = String
type Prefix = String

data TypedLiteral = TypedL String String deriving Show
data FloatPoint = FloatP Double (Maybe Exponent)
data LiteralWithLang = LiteralWithLang String LangTag deriving Show

newtype Exponent = Exponent Integer
newtype DecimalLiteral = DecimalL Double deriving Show
newtype IntegerLiteral = IntegerL Integer deriving Show
newtype NodeID = NodeID String deriving Show

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
allKeywords =
  concat [datatypeKeywords, entityKeywords, ontologyKeywords, propertyKeywords]

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
  [ "Datatype"
  , "Class"
  , "ObjectProperty"
  , "DataProperty"
  , "AnnotationProperty"
  , "NamedInvividual"
  ]

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
-- >>> parseTest (symbol "a symbol") "a symbol  "
-- "a symbol"
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
fullIRI = iriParens $ takeWhileP Nothing (/= '>')

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
identifier = (lexeme . try) (p >>= check)
 where
  p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  check x = if x `elem` allKeywords
    then fail $ concat ["keyword ", show x, " cannot be an identifier"]
    else return x

-- | It parses prefix names
--
prefixName :: Parser String
prefixName = identifier

-- | It parses abbreviated IRIs. Format: 'prefix:term'
-- >>> parseTest abbreviatedIRI "owl:user1"
-- ("owl","user1")
--
abbreviatedIRI :: Parser (Prefix, String)
abbreviatedIRI = (,) <$> (identifier <* symbol ":") <*> identifier

-- | It parses simple IRIs; a finite sequence of characters matching the PN_LOCAL
-- production of [SPARQL] and not matching any of the keyword terminals of the syntax 
--
-- TODO: Simplified to a simple identifier parser
simpleIRI :: Parser String
simpleIRI = identifier

-- | It parses any of the three different formats of IRIs
iri :: Parser String
iri = fullIRI <|> (concatAbbrIRI <$> abbreviatedIRI) <|> simpleIRI
  where concatAbbrIRI (x, y) = concat [x, ":", y]

-- | It parses class IRIs
classIRI :: Parser IRI
classIRI = iri

-- | It parses datatypes
--
-- >>> parseTest dataType "<http://example.iri>"
-- "http://example.iri"
--
-- >>> parseTest dataType "integer"
-- "integer"
--
dataType :: Parser String
dataType =
  try dataTypeIRI
    <|> symbol "integer"
    <|> symbol "decimal"
    <|> symbol "float"
    <|> symbol "string"

dataTypeIRI :: Parser String
dataTypeIRI = iri

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

literal :: Parser String
literal =
  show
    <$> typedLiteral
    <|> stringLiteralNoLanguage
    <|> (show <$> stringLiteralWithLanguage)
    <|> (show <$> integerLiteral)
    <|> (show <$> decimalLiteral)
    <|> (show <$> floatingPointLiteral)

-- | It parses a typed literal
--
-- >>> parseTest typedLiteral "\"32\"^^integer"
-- TypedL "32" "integer"
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue <*> (symbol "^^" *> dataType)

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
languageTag = char '@' *> identifier -- (U+40) followed a nonempty sequence of characters matching the langtag production from [BCP 47]

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
    [ ("Datatype"          , dataType)
    , ("Class"             , classIRI)
    , ("ObjectProperty"    , objectPropertyIRI)
    , ("DataProperty"      , dataPropertyIRI)
    , ("AnnotationProperty", annotationPropertyIRI)
    , ("NamedIndividual"   , individualIRI)
    ]


------------------------------
-- Ontology and Annotations --
------------------------------

type ImportIRI = IRI
type Frame = String
data OntologyIRI = OntologyIRI IRI (Maybe IRI) deriving Show
data Ontology = Ontology (Maybe OntologyIRI) [ImportIRI] [Annotation] [Frame] deriving Show

data Annotation = Annotation IRI String deriving Show
data PrefixEntry = PrefixE Prefix IRI deriving Show
data OntologyDocument = OntologyD [PrefixEntry] Ontology deriving Show

annotations :: Parser [Annotation]
annotations = symbol "Annotations:" *> some annotation -- TODO: annotatedList annotation

annotation :: Parser Annotation
annotation = Annotation <$> annotationPropertyIRI <*> annotationTarget

annotationTarget :: Parser String
annotationTarget = (show <$> nodeID) <|> iri <|> literal

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyD <$> many prefixDeclaration <*> ontology

-- | It parses prefix names. Format: 'Prefix: <name>: <IRI>
-- >>> parseTest prefixDeclaration "Prefix: owl: <http://www.w3.org/2002/07/owl#>"
-- PrefixE "owl" "http://www.w3.org/2002/07/owl#"
--
prefixDeclaration :: Parser PrefixEntry
prefixDeclaration =
  PrefixE <$> (symbol "Prefix:" *> (prefixName <* symbol ":")) <*> fullIRI

ontology :: Parser Ontology
ontology = do
  symbol "Ontology:"
  ontoIRI <- optional $ OntologyIRI <$> ontologyIRI <*> optional versionIRI -- Maybe (iri, Maybe iri)
  imports <- many importStmt
  annots  <- annotations
  frames  <- many frame
  return $ Ontology ontoIRI imports annots frames

ontologyIRI :: Parser IRI
ontologyIRI = iri

versionIRI :: Parser IRI
versionIRI = iri

importStmt :: Parser ImportIRI
importStmt = symbol "Import:" >> iri

frame :: Parser String
frame = undefined


------------------------------------------
--- Properties and datatype epressions ---
------------------------------------------
data ObjectProperty = ObjectP IRI | InverseObjectP IRI deriving Show
newtype DataProperty = DataP IRI deriving Show
-- data DataPrimary = DataPr DataAtomic | DataPrNot DataAtomic deriving Show

objectPropertyExpression :: Parser ObjectProperty
objectPropertyExpression = (ObjectP <$> objectPropertyIRI) <|> inverseObjectProperty

inverseObjectProperty :: Parser ObjectProperty
inverseObjectProperty = symbol "inverse" *> (InverseObjectP <$> objectPropertyIRI)

dataPropertyExpression :: Parser DataProperty
dataPropertyExpression = DataP <$> dataPropertyIRI

dataRange :: Parser String
dataRange = intercalate " or " <$> singleOrMany "or" dataConjuction

dataConjuction :: Parser String
dataConjuction = intercalate " and " <$> singleOrMany "and" dataPrimary

dataPrimary :: Parser String
dataPrimary = do
  not <- optional $ symbol "not"
  let v = case not of
        Just _  -> "not "
        Nothing -> ""
  atom <- dataAtomic
  return $ v ++ atom

dataAtomic :: Parser String
dataAtomic =
  dataType
    <|> show <$> enclosedS '{' (nonEmptyList literal)
    <|> datatypeRestriction
    <|> enclosedS '(' dataRange

datatypeRestriction :: Parser String
datatypeRestriction = do
  dt <- dataType
  symbol "["
  rvList <- nonEmptyList ((,) <$> facet <*> restrictionValue)
  symbol "]"
  return $ unwords
    ["datatypeRestriction: {", show dt, ", [", unwords (show <$> rvList), "]"]

facet :: Parser String
facet = choice $ fmap
  symbol
  [ "length"
  , "maxLength"
  , "minLength"
  , "pattern"
  , "langRange"
  , "<="
  , "<"
  , ">="
  , ">"
  ]

restrictionValue :: Parser String
restrictionValue = literal

predifinedPrefixex :: [PrefixEntry]
predifinedPrefixex =
  [ PrefixE "rdf"  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  , PrefixE "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
  , PrefixE "xsd"  "http://www.w3.org/2001/XMLSchema#"
  , PrefixE "owl"  "http://www.w3.org/2002/07/owl#"
  ]


---------------------
---  Descriptions ---
---------------------

description :: Parser [String]
description = singleOrMany "or" conjunction

conjunction :: Parser String
conjunction = undefined


primary :: Parser String
primary = undefined

restriction :: Parser String
restriction = undefined

-- | It parses a class IRI or a list of individual IRIs
--
-- >>> parseTest atomic "<class.iri>"
-- ["class.iri"]
--
-- >>> parseTest atomic "{ <class.iri#ind1>, <class.iri#ind2> }"
-- ["class.iri#ind1","class.iri#ind2"]
--
atomic :: Parser [String]
atomic =
  (pure <$> classIRI) <|> enclosedS '{' (nonEmptyList individual)

-----------------------
--- Generic parsers ---
-----------------------

singleOrMany :: String -> Parser p -> Parser [p]
singleOrMany sep p =
  let multipleP =
        (:) <$> p <*> some (symbol sep *> p)
  in multipleP <|> (pure <$> p)

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
annotatedList :: Parser p -> Parser [([Annotation], p)]
annotatedList p =
  let annotationList = (,) <$> fmap (fromMaybe []) (optional annotations) <*> p
  in  nonEmptyList annotationList

