{-# LANGUAGE LambdaCase #-}

module OwlParser where

import           Prelude                           hiding ( exponent )
import           Data.Functor                             ( ($>) )
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
identifier_ = try (p >>= check)
 where
  p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  check x =
    if x `elem` allKeywords then fail $ concat ["keyword ", show x, " cannot be an identifier"] else return x

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
-- >>> parseTest abbreviatedIRI "owl:user1"
-- ("owl","user1")
--
-- >>> parseTest abbreviatedIRI "owl:   user1"
-- ...
-- unexpected space
-- ...
--
abbreviatedIRI :: Parser (Prefix, String)
abbreviatedIRI = (,) <$> prefixName <*> identifier

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
-- >>> parseTest dataType "<http://example.iri>"
-- "http://example.iri"
--
-- >>> parseTest dataType "integer"
-- "integer"
--
dataType :: Parser String
dataType = try dataTypeIRI <|> symbol "integer" <|> symbol "decimal" <|> symbol "float" <|> symbol "string"

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
data Ontology = Ontology (Maybe OntologyIRI) [ImportIRI] (AnnotatedList Annotation) [Frame] deriving Show

data Annotation = Annotation IRI String
data PrefixEntry = PrefixE Prefix IRI deriving Show
data OntologyDocument = OntologyD [PrefixEntry] Ontology deriving Show
newtype AnnotatedList a = AnnList [(AnnotatedList Annotation, a)]

instance Show Annotation where
  show (Annotation i s) = unwords [i, show s]

instance (Show a) => Show (AnnotatedList a) where
  show (AnnList []) = ""
  show (AnnList xs) = intercalate ",\n" (go <$> xs)
   where
    go ((AnnList []), x) = show x
    go (al, x) = unwords ["Annotations:", show al, "\n ", show x]

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
-- PrefixE "g" "http://ex.com/owl2/families#"
--
-- >>> parseTest prefixDeclaration "Prefix: : <http://ex.com/owl/families#>"
-- PrefixE "" "http://ex.com/owl/families#"
--
prefixDeclaration :: Parser PrefixEntry
prefixDeclaration = PrefixE <$> (symbol "Prefix:" *> lexeme prefixName) <*> fullIRI

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
dataPrimary =
  let neg  = fromMaybe "" <$> optionalNegation
      seqs = sequence [neg, dataAtomic]
  in  show <$> seqs

dataAtomic :: Parser String
dataAtomic =
  dataType
    <|> show
    <$> enclosedS '{' (nonEmptyList literal)
    <|> datatypeRestriction
    <|> enclosedS '(' dataRange

datatypeRestriction :: Parser String
datatypeRestriction = do
  dt <- dataType
  symbol "["
  rvList <- nonEmptyList ((,) <$> facet <*> restrictionValue)
  symbol "]"
  return $ unwords ["datatypeRestriction: {", show dt, ", [", unwords (show <$> rvList), "]"]

facet :: Parser String
facet =
  choice $ fmap symbol ["length", "maxLength", "minLength", "pattern", "langRange", "<=", "<", ">=", ">"]

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
conjunction = restrictions <|> primaries <|> primary
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
      return . unwords $ [fromMaybe "" neg', rst']
    return . unwords $ [show clsIRI, fromMaybe "" neg, rst] ++ rsts
  primaries =
    let prims = symbol "and" *> primary
        list  = (:) <$> primary <*> some prims
    in  show <$> list

primary :: Parser String
primary = do
  neg  <- optionalNegation
  rOrA <- restriction <|> (show <$> atomic)
  return . unwords $ [fromMaybe "" neg, rOrA]

restriction :: Parser String
restriction = choice $ objectExprParsers <> dataExprParsers
 where
  objExprs =
    [ ("some"   , primary)
    , ("only"   , primary)
    , ("Self"   , pure "")
    , ("min", show <$> nonNegativeInteger <* optional primary)
    , ("max", show <$> nonNegativeInteger <* optional primary)
    , ("exaclty", show <$> nonNegativeInteger <* optional primary)
    ]
  dataExprs =
    [ ("some"   , dataPrimary)
    , ("only"   , dataPrimary)
    , ("value"  , literal)
    , ("min", show <$> nonNegativeInteger <* optional dataPrimary)
    , ("max", show <$> nonNegativeInteger <* optional dataPrimary)
    , ("exaclty", show <$> nonNegativeInteger <* optional dataPrimary)
    ]
  -- TODO: now, i keep only the last parser!
  objectExprParsers = (\(smb, p) -> objectPropertyExpression *> symbol smb *> p) <$> objExprs
  dataExprParsers   = (\(smb, p) -> dataPropertyExpression *> symbol smb *> p) <$> dataExprs

-- | It parses a class IRI or a list of individual IRIs
--
-- >>> parseTest atomic "<class.iri>"
-- ["class.iri"]
--
-- >>> parseTest atomic "{ <class.iri#ind1>, <class.iri#ind2> }"
-- ["class.iri#ind1","class.iri#ind2"]
--
atomic :: Parser [String]
atomic = pure <$> classIRI <|> enclosedS '{' (nonEmptyList individual)


--------------------------------
--- Frames and Miscellaneous ---
--------------------------------

datatypeFrame :: Parser String
datatypeFrame = do
  dttp    <- symbol "Datatype:" *> dataType
  annots  <- many $ symbol "Annotations:" *> annotatedList annotation
  equiv   <- optional $ symbol "EquivalentTo:" *> annotations <* dataRange
  annots' <- many $ symbol "Annotations:" *> annotatedList annotation
  pure "<data-frame>"

classFrame :: Parser String
classFrame = do
  clsIRI <- symbol "Class:" *> classIRI
  blob   <- (unwords <$> many fstChoice) <|> sndChoice
  pure "<class-frame>"
 where
  fstChoice =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "SubClassOf:" *> annotatedList description $> "<subclass-description>")
      <|> (symbol "EquivalentTo:" *> annotatedList description $> "<equivalent-to-description>")
      <|> (symbol "DisjointWith:" *> annotatedList description $> "<disjoint-with-descriptnio>")
      <|> (  symbol "DisjointUnionOf:"
          *> annotations
          *> listOfAtLeast2 description
          $> "<disjoin-union-description>"
          )
  sndChoice = do
    symbol "HasKey:"
    annots <- annotations
    mExpr  <-
      nonEmptyList
      $   (objectPropertyExpression $> "<objectPropertyExpression>")
      <|> (dataPropertyExpression $> "<dataPropertyExpression>")
    pure "<sndChoice>"

objectPropertyFrame :: Parser String
objectPropertyFrame = do
  objPropIRI <- symbol "ObjectProperty:" *> objectPropertyIRI
  blob       <- unwords <$> many altr
  pure "<object-property-frame>"
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
      <|> (symbol "DisjoinWith:" *> annotatedList objectPropertyExpression $> "<disjoin-with-expr>")
      <|> (symbol "InverseOf:" *> annotatedList objectPropertyExpression $> "<inverse-of-expr>")
      <|> (  symbol "SubPropertyChain:"
          *> annotations
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

dataPropertyFrame :: Parser String
dataPropertyFrame = do
  dataPropIRI <- symbol "ObjectProperty:" *> dataPropertyIRI
  blob        <- unwords <$> many altr
  pure "<data-property-frame>"
 where
  altr =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "Domain:" *> annotatedList description $> "<domain-description>")
      <|> (symbol "Range:" *> annotatedList description $> "<range-description>")
      <|> (symbol "Characteristics:" *> annotations *> symbol "Functional" $> "<characteristics-props>")
      <|> (symbol "SubPropertyOf:" *> annotatedList dataPropertyExpression $> "<sub-property-of-expr>")
      <|> (symbol "EquivalentTo:" *> annotatedList dataPropertyExpression $> "<equivalent-to-expr>")
      <|> (symbol "DisjoinWith:" *> annotatedList dataPropertyExpression $> "<disjoin-with-expr>")

annotationPropertyFrame :: Parser String
annotationPropertyFrame = do
  annPropIRI <- symbol "AnnotationProperty:" *> annotationPropertyIRI
  blob       <- unwords <$> many altr
  pure "<annotation-property-frame>"
 where
  altr =
    (symbol "Annotations:" *> annotatedList annotation $> "<annotations>")
      <|> (symbol "Domain:" *> annotatedList iri $> "<domain-description>")
      <|> (symbol "Range:" *> annotatedList iri $> "<range-description>")
      <|> (symbol "SubPropertyOf:" *> annotatedList annotationPropertyIRI $> "<sub-property-of-expr>")

individualFrame :: Parser String
individualFrame = do
  indi <- symbol "Indivvidual:" *> individual
  blob <- unwords <$> many altr
  pure "<annotation-property-frame>"
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
  fact <- objectPropertyFact <|> dataPropertyFact
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

misc :: Parser String
misc =
  (symbol "EquivalentClasses:" *> annotations *> listOfAtLeast2 description $> "<equivalent-classes>")
    <|> (symbol "DisjointClasses:" *> annotations *> listOfAtLeast2 description $> "<disjoint-classes>")
    <|> (  symbol "EquivalentProperties:"
        *> annotations
        *> listOfAtLeast2 objectPropertyIRI
        $> "<equivalent-object-property>"
        )
    <|> (  symbol "DisjointProperties:"
        *> annotations
        *> listOfAtLeast2 objectPropertyIRI
        $> "<disjoint-object-property>"
        )
    <|> (  symbol "EquivalentProperties:"
        *> annotations
        *> listOfAtLeast2 dataPropertyIRI
        $> "<equivalent-object-property>"
        )
    <|> (  symbol "DisjointProperties:"
        *> annotations
        *> listOfAtLeast2 dataPropertyIRI
        $> "<disjoint-object-property>"
        )
    <|> (symbol "SameIndividuals:" *> annotations *> listOfAtLeast2 individual $> "<same-individual>")
    <|> (  symbol "DifferentIndividuals:"
        *> annotations
        *> listOfAtLeast2 individual
        $> "<different-individual>"
        )

-----------------------
--- Generic parsers ---
-----------------------

optionalNegation :: Parser (Maybe String)
optionalNegation = optional . symbol $ "not"

singleOrMany :: String -> Parser p -> Parser [p]
singleOrMany sep p = let multipleP = (:) <$> p <*> some (symbol sep *> p) in multipleP <|> (pure <$> p)

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
annotatedList :: Parser p -> Parser (AnnotatedList p)
annotatedList p =
  let annotationList = (,) <$> fmap (fromMaybe (AnnList [])) (optional annotations) <*> p
  in  AnnList <$> nonEmptyList annotationList

