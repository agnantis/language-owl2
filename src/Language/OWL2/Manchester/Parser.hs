{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Manchester.Parser where

import           Prelude                           hiding ( exponent )
import           Data.Functor                             ( ($>) )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                               ( fromMaybe )
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.OWL2.Import                     ( Text )
import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types

-- DocTest setup
--
-- $setup
-- >>> :set -XOverloadedStrings

--------------------------
-- Parser related types --
--------------------------

type Parser = Parsec Void Text
-- | Parses white space and line comments
--
-- >>> parseTest (sc *> many (satisfy (const True))) "    some indented text"
-- "some indented text"
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
enclosedS c = between (symbol . T.singleton $ c) (symbol . T.singleton . cChar $ c)

-- | Reserved keywords
allKeywords :: [Text]
allKeywords = concat [ datatypeKeywords, entityKeywords, ontologyKeywords
                     , propertyKeywords, miscKeywords
                     ]

datatypeKeywords :: [Text]
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

entityKeywords :: [Text]
entityKeywords = [ "Datatype", "Class", "ObjectProperty", "DataProperty", "AnnotationProperty"
                 , "NamedInvividual", "EquivalentTo", "SubClassOf", "DisjointWith"
                 , "DisjointUnionOf", "HasKey", "Domain", "Range", "Characteristics"
                 , "SubPropertyOf", "InverseOf", "SubPropertyChain"
                 ]

ontologyKeywords :: [Text]
ontologyKeywords = ["Annotations", "Prefix", "Ontology", "Import"]

propertyKeywords :: [Text]
propertyKeywords = [ "inverse", "or", "and", "not", "length", "minLength", "maxLength", "pattern"
                   , "Functional", "InverseFunctional", "Reflexive", "Irreflexive", "Symmetric"
                   , "Asymmetric", "Transitive"
                   ]

miscKeywords :: [Text]
miscKeywords = [ "Individual", "Types", "Facts", "SameAs", "DifferentFrom", "EquivalentClasses"
               , "DisjointClasses", "EquivalentProperties", "DisjointProperties"
               , "EquivalentProperties", "DisjointProperties", "SameIndividual"
               , "DifferentIndividuals"
               ]

-- | Parses the symbol and then any remaining space
--
-- >>> parseTest (symbol "a symbol" *> symbol "and a second") "a symbol    and a second"
-- "and a second"
--
symbol :: Text -> Parser Text
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

digits :: Parser Text
digits = T.pack <$> some digit

-- | It parses positive integer
--
-- >>> parseTest positiveInteger "13"
-- 13
--
positiveInteger :: Parser Int
positiveInteger = do
  fd <- nonZero
  rm <- many digit
  lexeme . pure $ read (fd : rm)

-- | It parses non negativeinteger
--
-- >>> parseTest nonNegativeInteger "13"
-- 13
-- >>> parseTest nonNegativeInteger "0"
-- 0
nonNegativeInteger :: Parser Int
nonNegativeInteger =
  let num = (0 <$ zero) <|> positiveInteger
  in lexeme num

-- | It may parse a sign or no sign at all
--
-- >>> parseTest sign "+"
-- ""
-- >>> parseTest sign "-"
-- "-"
-- >>> parseTest sign ""
-- ""
sign :: Parser Text
sign = do
  mSign <- optional $ "" <$ symbol "+" <|> symbol "-"
  pure $ fromMaybe "" mSign

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
identifier :: Parser Text
identifier = lexeme identifier_

-- | It parses arbitrary alpharithmetics provived that it does not belong to
-- the list of reserved keywords. It does not parse any space after the identifier
identifier_ :: Parser Text
identifier_ = try (anyIdentifier_ >>= check)
 where
  check x =
    if x `elem` allKeywords
    then fail $ concat ["keyword ", show x, " cannot be an identifier"]
    else pure x

-- | It parses arbitrary alpharithmetics. It does not parse any space after the identifier
anyIdentifier_ :: Parser Text
anyIdentifier_ = T.pack <$> try ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-- | It parses arbitrary alpharithmetics. It parses any space after the identifier
anyIdentifier :: Parser Text
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
prefixName :: Parser Text
prefixName = (identifier_ <|> pure "") <* string ":"

-- | TODO currently IRI is defined as text inside <>
-- No validation is being performed
-- Check: http://www.rfc-editor.org/rfc/rfc3987.txt for BNF representation
--
-- >>> parseTest fullIRI "<http://www.uom.gr/ai/TestOntology.owl#Child>"
-- FullIRI "http://www.uom.gr/ai/TestOntology.owl#Child"
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
--
fullIRI :: Parser IRI
fullIRI = FullIRI <$> (lexeme . iriParens $ takeWhileP Nothing (/= '>'))

-- | It parses abbreviated IRIs. Format: 'prefix:term'
-- >>> parseTest abbreviatedIRI "xsd:string"
-- AbbreviatedIRI "xsd" "string"
--
-- >>> parseTest abbreviatedIRI "owl:   user1"
-- ...
-- unexpected space
-- ...
--
abbreviatedIRI :: Parser IRI
abbreviatedIRI = AbbreviatedIRI <$> prefixName <*> anyIdentifier

-- | It parses simple IRIs; a finite sequence of characters matching the PN_LOCAL
-- production of [SPARQL] and not matching any of the keyword terminals of the syntax
--
-- TODO: Simplified to a simple identifier parser
-- >>> parseTest simpleIRI "John"
-- SimpleIRI "John"
--
simpleIRI :: Parser IRI
simpleIRI = SimpleIRI <$> identifier

-- | It parses any of the three different formats of IRIs
-- >>> parseTest iri "John"
-- SimpleIRI "John"
--
-- >>> parseTest iri "owl:John"
-- AbbreviatedIRI "owl" "John"
--
-- >>> parseTest iri "<http://www.uom.gr/ai/TestOntology.owl#Child>"
-- FullIRI "http://www.uom.gr/ai/TestOntology.owl#Child"
--
iri :: Parser IRI
iri = fullIRI <|> try abbreviatedIRI <|> simpleIRI

-- | It parses class IRIs
classIRI :: Parser IRI
classIRI = iri

-- | It parses datatypes
--
-- >>> parseTest datatype "<http://example.iri>"
-- IriDT (FullIRI "http://example.iri")
--
-- >>> parseTest datatype "integer"
-- IntegerDT
--
-- >>> parseTest datatype "xsd:string"
-- IriDT (AbbreviatedIRI "xsd" "string")
--
datatype :: Parser Datatype
datatype =  IriDT <$> try datatypeIRI
        <|> IntegerDT   <$ symbol "integer"
        <|> DecimalDT   <$ symbol "decimal"
        <|> FloatDT     <$ symbol "float"
        <|> StringDT    <$ symbol "string"

datatypeIRI :: Parser IRI
datatypeIRI = iri

objectPropertyIRI :: Parser IRI
objectPropertyIRI = iri

dataPropertyIRI :: Parser IRI
dataPropertyIRI = iri

annotationPropertyIRI :: Parser IRI
annotationPropertyIRI = iri

individual :: Parser Individual
individual =  IRIIndividual  <$> individualIRI
          <|> NodeIndividual <$> nodeID

individualIRI :: Parser IndividualIRI
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
-- TypedLiteralC (TypedL "32" IntegerDT)
--
-- >>> parseTest literal "\"stringLiteralNoLanguage\""
-- StringLiteralNoLang "stringLiteralNoLanguage"
--
-- >>> parseTest literal "\"stringLiteralWithLang\"@en"
-- StringLiteralLang (LiteralWithLang "stringLiteralWithLang" "en")
--
literal :: Parser Literal
literal =  lexeme $ TypedLiteralC <$> try typedLiteral
       <|> StringLiteralLang      <$> try stringLiteralWithLanguage
       <|> StringLiteralNoLang    <$> stringLiteralNoLanguage
       <|> IntegerLiteralC        <$> try integerLiteral
       <|> DecimalLiteralC        <$> try decimalLiteral
       <|> FloatingLiteralC       <$> try floatingPointLiteral

-- | It parses a typed literal
--
-- >>> parseTest typedLiteral "\"32\"^^integer"
-- TypedL "32" IntegerDT
--
-- >>> parseTest typedLiteral "\"Jack\"^^xsd:string"
-- TypedL "Jack" (IriDT (AbbreviatedIRI "xsd" "string"))
--
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue <*> (symbol "^^" *> datatype)

-- | It parses a string value with no language tag
--
-- >>> parseTest stringLiteralNoLanguage "\"hello there\""
-- "hello there"
--
stringLiteralNoLanguage :: Parser Text
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
-- >>> parseTest languageTag "@en"
-- "en"
-- >>> parseTest languageTag "en"
-- ...
-- unexpected 'e'
-- expecting '@'
--
-- TODO: check for valid lang tags
languageTag :: Parser LangTag
languageTag = char '@' *> identifier_ -- (U+40) followed a nonempty sequence of characters matching the langtag production from [BCP 47]

lexicalValue :: Parser Text
lexicalValue = quotedString

-- | It parses a string enclosed in double quotes
--
-- >>> parseTest quotedString "\"this is a quoted string\""
-- "this is a quoted string"
-- >>> parseTest quotedString "\"this is \\\"test \\\" message\""
-- "this is \\\"test \\\" message"
--
-- >>> parseTest quotedString "\"text with\nnewlines\""
-- "text with\nnewlines"
--
quotedString :: Parser Text
quotedString = do
  strings <- char '\"' *> many chars <* char '\"'
  pure . T.pack . concat $ strings
 where
  chars     = (pure <$> nonEscape) <|> escape
  nonEscape = noneOf ("\\\"" :: String) -- all the characters that can be escaped
  escape    = do
    d <- char '\\'
    c <- oneOf ("\\\"0nrvtbf" :: String)
    pure [d, c]


-- | It parses folating point numbers.
-- Valid formats:
--   * 12F
--   * .3f
--   * 12.3f
--   * -12.3F
--
-- >>> parseTest floatingPointLiteral "12F"
-- FloatP 12.0 Nothing
-- >>> parseTest floatingPointLiteral "12.3f"
-- FloatP 12.3 Nothing
-- >>> parseTest floatingPointLiteral "-12.332F"
-- FloatP (-12.332) Nothing
-- >>> parseTest floatingPointLiteral ".3f"
-- FloatP 0.3 Nothing
-- >>> parseTest floatingPointLiteral ".3e10f"
-- FloatP 0.3 (Just 10)
-- >>> parseTest floatingPointLiteral "-12.3e-10F"
-- FloatP (-12.3) (Just (-10))
--
floatingPointLiteral :: Parser FloatPoint
floatingPointLiteral = do
  sgn  <- sign
  dgts <- dig1 <|> dig2
  mExp <- optional exponent
  _    <- symbol "f" <|> symbol "F"
  pure $ FloatP (read . T.unpack $ sgn <> dgts) mExp
 where
  dig1 :: Parser Text
  dig1 = do
    dg'  <- digits
    mDec <- optional $ do
      dg <- symbol "." *> digits
      pure $ "." <> dg
    let dc = fromMaybe "" mDec
    pure $ dg' <> dc
  dig2 :: Parser Text
  dig2 = do
    dgts <- symbol "." *> digits
    pure $ "0." <> dgts


-- | It parses an exponent
--
-- >>> parseTest exponent "e10"
-- 10
-- >>> parseTest exponent "E10"
-- 10
-- >>> parseTest exponent "e+10"
-- 10
-- >>> parseTest exponent "E+10"
-- 10
-- >>> parseTest exponent "e-10"
-- -10
-- >>> parseTest exponent "E-10"
-- -10
--
exponent :: Parser Exponent
exponent = do
  _    <- symbol "e" <|> symbol "E"
  ms   <- sign
  dgts <- digits
  pure . read . T.unpack $ ms <> dgts

-- | It parser decimal values
--
-- >>> parseTest decimalLiteral "10.345"
-- DecimalL 10.345
-- >>> parseTest decimalLiteral "-10.345"
-- DecimalL (-10.345)
-- >>> parseTest decimalLiteral "+10.345"
-- DecimalL 10.345
--
decimalLiteral :: Parser DecimalLiteral
decimalLiteral = do
  mSign <- sign
  dig1  <- digits
  dig2  <- symbol "." *> digits
  pure . DecimalL . read . T.unpack . T.concat $ [mSign, dig1, ".", dig2]

-- | It parser integer values
--
-- >>> parseTest integerLiteral "10"
-- IntegerL 10
-- >>> parseTest integerLiteral "-10"
-- IntegerL (-10)
-- >>> parseTest integerLiteral"+10"
-- IntegerL 10
--
integerLiteral :: Parser IntegerLiteral
integerLiteral = do
  mSign <- sign
  digs  <- digits
  pure . IntegerL . read . T.unpack $ mSign <> digs

entity :: Parser Entity
entity = choice $ fmap (\(s, p) -> symbol s *> p) alts
 where
  alts :: [(Text, Parser Entity)]
  alts =
    [ ("Datatype"          , DatatypeEntity <$> datatype)
    , ("Class"             , ClassEntity <$> classIRI)
    , ("ObjectProperty"    , ObjectPropertyEntity <$> objectPropertyIRI)
    , ("DataProperty"      , DataPropertyEntity <$> dataPropertyIRI)
    , ("AnnotationProperty", AnnotationPropertyEntity <$> annotationPropertyIRI)
    , ("NamedIndividual"   , IndividualEntity <$> individualIRI)
    ]


------------------------------
-- Ontology and Annotations --
------------------------------

-- | It parses annotations
--
-- >>> :{
-- let input :: [Text]
--     input =
--      [ "Annotations: creator \"John\","
--      , "             Annotations: rdfs:comment \"Creation Year\" creationYear 2008,"
--      , "             mainClass Person"
--      ]
-- :}
--
-- >>> parseTest (annotations *> eof) (T.unlines input)
-- ()
--
annotations :: Parser (AnnotatedList Annotation)
annotations = symbol "Annotations:" *> annotatedList annotation

-- | It parses a single annotation
--
-- >>> parseTest annotation ":creator \"john\""
-- Annotation (AbbreviatedIRI "" "creator") (LiteralAT (StringLiteralNoLang "john"))
--
annotation :: Parser Annotation
annotation = Annotation <$> annotationPropertyIRI <*> annotationTarget

-- | It parser node ids, iris or literals
--
-- >>> parseTest annotationTarget "\"john\""
-- LiteralAT (StringLiteralNoLang "john")
--
-- >>> parseTest annotationTarget "John"
-- IriAT (SimpleIRI "John")
--
-- >>> parseTest annotationTarget "_:node"
-- NodeAT (NodeID "node")
--
-- >>> parseTest annotationTarget "<http://some.iri>"
-- IriAT (FullIRI "http://some.iri")
--
annotationTarget :: Parser AnnotationTarget
annotationTarget =  NodeAT    <$> try nodeID
                <|> IriAT     <$> try iri
                <|> LiteralAT <$> try literal

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyD <$> many prefixDeclaration <*> ontology

-- | It parses prefix names. Format: 'Prefix: <name>: <IRI>
--
-- >>> parseTest prefixDeclaration "Prefix: g: <http://ex.com/owl2/families#>"
-- PrefixD "g" (FullIRI "http://ex.com/owl2/families#")
--
-- >>> parseTest prefixDeclaration "Prefix: : <http://ex.com/owl/families#>"
-- PrefixD "" (FullIRI "http://ex.com/owl/families#")
--
prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = PrefixD <$> (symbol "Prefix:" *> lexeme prefixName) <*> fullIRI

ontology :: Parser Ontology
ontology = do
  _       <- symbol "Ontology:"
  ontoIRI <- optional $ OntologyVersionIRI <$> ontologyIRI <*> optional versionIRI -- Maybe (iri, Maybe iri)
  imports <- many importStmt
  annots  <- many annotations
  frames  <- many frame
  pure $ Ontology ontoIRI imports annots frames

ontologyIRI :: Parser IRI
ontologyIRI = iri

versionIRI :: Parser IRI
versionIRI = iri

-- | It parses import statements
--
-- >>> parseTest importStmt "Import: <http://ex.com/owl2/families.owl>"
-- ImportD (FullIRI "http://ex.com/owl2/families.owl")
--
importStmt :: Parser ImportDeclaration
importStmt = ImportD <$> (symbol "Import:" *> iri)

frame :: Parser Frame
frame =  FrameDT <$> datatypeFrame
     <|> FrameC  <$> classFrame
     <|> FrameOP <$> objectPropertyFrame
     <|> FrameDP <$> dataPropertyFrame
     <|> FrameAP <$> annotationPropertyFrame
     <|> FrameI  <$> individualFrame
     <|> FrameM  <$> misc


-------------------------------------------
--- Properties and datatype expressions ---
-------------------------------------------

-- | It parses an object property expression
--
-- >>> parseTest objectPropertyExpression "<http://object-property-iri.com>"
-- OPE (FullIRI "http://object-property-iri.com")
--
-- >>> parseTest objectPropertyExpression "inverse <http://object-property-iri.com>"
-- InverseOPE (FullIRI "http://object-property-iri.com")
--
objectPropertyExpression :: Parser ObjectPropertyExpression
objectPropertyExpression =  OPE        <$> objectPropertyIRI
                        <|> InverseOPE <$> (symbol "inverse" *> objectPropertyIRI)

-- | It parses a data property expression
--
-- >>> parseTest dataPropertyExpression "<http://object-property-iri.com>"
-- FullIRI "http://object-property-iri.com"
--
dataPropertyExpression :: Parser DataPropertyExpression
dataPropertyExpression = dataPropertyIRI

-- | It parses a data range
--
-- >>> parseTest (dataRange *> eof) "integer[>10] and integer[<20] or integer[>100]"
-- ()
--
dataRange :: Parser DataRange
dataRange = DataRange <$> singleOrMany "or" dataConjunction

-- | It parses a data conjunction (i.e. 'and')
--
-- >>> parseTest (dataConjunction *> eof) "integer[<10] and integer[>0]"
-- ()
--
dataConjunction :: Parser DataConjunction
dataConjunction = DataConjunction <$> singleOrMany "and" dataPrimary

-- | It parses a data primary
--
-- >>> parseTest dataPrimary "integer[<0]"
-- DataPrimary (Positive (DatatypeRestrictionDA (DatatypeRestriction IntegerDT (RestrictionExp L_FACET (IntegerLiteralC (IntegerL 0)) :| []))))
--
dataPrimary :: Parser DataPrimary
dataPrimary = do
  neg <- optionalNegation
  da  <- dataAtomic
  pure . DataPrimary $ fmap (const da) neg

-- | It parses an atomic data
--
-- >>> parseTest (dataAtomic *> eof)  "integer[<0]"
-- ()
--
dataAtomic :: Parser DataAtomic
dataAtomic =  DatatypeRestrictionDA <$> try datatypeRestriction
          <|> DatatypeDA            <$> try datatype
          <|> LiteralListDA         <$> enclosedS '{' literalList
          <|> DataRangeDA           <$> enclosedS '(' dataRange

-- | It parsers a non empty list of literal
--
-- >>> parseTest literalList "\"kostas\", 32, \"true\""
-- StringLiteralNoLang "kostas" :| [IntegerLiteralC (IntegerL 32),StringLiteralNoLang "true"]
--
literalList :: Parser (NonEmpty Literal)
literalList = nonEmptyList literal

-- | It parses datatype restrictions
--
-- >>> parseTest (datatypeRestriction *> eof) "integer[> 0, maxLength 2]"
-- ()
--
-- >>> parseTest datatypeRestriction "integer[< 0]"
-- DatatypeRestriction IntegerDT (RestrictionExp L_FACET (IntegerLiteralC (IntegerL 0)) :| [])
--
datatypeRestriction :: Parser DatatypeRestriction
datatypeRestriction = do
  dt <- datatype
  _  <- symbol "["
  rvList <- nonEmptyList (RestrictionExp <$> facet <*> restrictionValue)
  _  <- symbol "]"
  pure $ DatatypeRestriction dt rvList

facet :: Parser Facet
facet =  symbol "length"    $> LENGTH_FACET
     <|> symbol "maxLength" $> MAX_LENGTH_FACET
     <|> symbol "minLength" $> MIN_LENGTH_FACET
     <|> symbol "pattern"   $> PATTERN_FACET
     <|> symbol "langRange" $> LANG_RANGE_FACET
     <|> symbol "<="        $> LE_FACET
     <|> symbol "<"         $> L_FACET
     <|> symbol ">="        $> GE_FACET
     <|> symbol ">"         $> G_FACET

restrictionValue :: Parser Literal
restrictionValue = literal

---------------------
---  Descriptions ---
---------------------

-- | It parses a description
--
-- >>> parseTest (description *> eof) "Man"
-- ()
--
-- >>> parseTest (description *> eof) "Man or Woman"
-- ()
--
-- >>> parseTest (description *> eof) "hasFirstName value \"John\" or Man"
-- ()
--
-- >>> parseTest (description *> eof) "hasFirstName value \"John\" or hasFirstName value \"Jack\"^^xsd:string"
-- ()
--
description :: Parser Description
description = Description <$> singleOrMany "or" conjunction

-- | It parses a conjunction
--
-- >>> parseTest (conjunction *> eof) "hasFirstName value \"John\""
-- ()
--
-- >>> parseTest (conjunction *> eof) "Person and Man"
-- ()
--
-- >>> parseTest conjunction "Person"
-- PrimConj (PrimaryA (Positive (AtomicClass (SimpleIRI "Person"))) :| [])
--
-- >>> parseTest (conjunction *> eof) "owl:Thing that hasFirstName exactly 1"
-- ()
--
-- >>> parseTest (conjunction *> eof) "p some a and p only b"
-- ()
--
conjunction :: Parser Conjunction
conjunction =  try (uncurry ClassConj <$> restrictions)
           <|> try (PrimConj <$> singleOrMany "and" primary)
 where
  restWithNeg = do
    neg  <- optionalNegation
    rst  <- restriction
    pure $ const rst <$> neg
  restrictions = do
    clsIRI <- classIRI <* symbol "that"
    rst    <- restWithNeg
    rsts   <- many $ symbol "and" *> restWithNeg
    pure (clsIRI, rst :| rsts)

-- | It parses a primary
--
-- >>> parseTest (primary *> eof) "hasFirstName value \"John\""
-- ()
--
-- >>> parseTest primary "not Person"
-- PrimaryA (Negative (AtomicClass (SimpleIRI "Person")))
--
-- >>> parseTest (primary *> eof) "not hasFirstName value \"John\""
-- ()
--
primary :: Parser Primary
primary = do
  neg  <- optionalNegation
  PrimaryR . (\x -> fmap (const x) neg) <$> try restriction <|> PrimaryA . (\x -> fmap (const x) neg) <$> try atomic

-- | It parses one of the many differnt type of restrictions on object or data properties
--
-- >>> parseTest (restriction <* eof) "hasFirstName value \"John\""
-- DPRestriction (DPR (SimpleIRI "hasFirstName") (ValueDPR (StringLiteralNoLang "John")))
--
-- >>> parseTest (restriction <* eof) "hasFirstName exactly 1"
-- OPRestriction (OPR (OPE (SimpleIRI "hasFirstName")) (ExactlyOPR 1 Nothing))
--
-- >>> parseTest (restriction *> eof) "hasFirstName only string[minLength 1]"
-- ()
--
restriction :: Parser Restriction
restriction =  OPRestriction <$> try (OPR <$> objectPropertyExpression <*> objectRestriction)
           <|> DPRestriction <$> try (DPR <$> dataPropertyExpression <*> dataRestriction)
 where
  objectRestriction =  try (SomeOPR    <$> (symbol "some"  *> primary))
                   <|> try (OnlyOPR    <$> (symbol "only"  *> primary))
                   <|> try (ValueOPR   <$> (symbol "value" *> individual))
                   <|> symbol "Self"    $> SelfOPR
                   <|> try (MinOPR     <$> (symbol "min"     *> nonNegativeInteger) <*> optional primary)
                   <|> try (MaxOPR     <$> (symbol "max"     *> nonNegativeInteger) <*> optional primary)
                   <|> try (ExactlyOPR <$> (symbol "exactly" *> nonNegativeInteger) <*> optional primary)
  dataRestriction =  try (SomeDPR    <$> (symbol "some"    *> dataPrimary))
                 <|> try (OnlyDPR    <$> (symbol "only"    *> dataPrimary))
                 <|> try (ValueDPR   <$> (symbol "value"   *> literal))
                 <|> try (MinDPR     <$> (symbol "min"     *> nonNegativeInteger) <*> optional dataPrimary)
                 <|> try (MaxDPR     <$> (symbol "max"     *> nonNegativeInteger) <*> optional dataPrimary)
                 <|> try (ExactlyDPR <$> (symbol "exactly" *> nonNegativeInteger) <*> optional dataPrimary)

-- | It parses a class IRI or a list of individual IRIs
--
-- >>> parseTest atomic "<class.iri>"
-- AtomicClass (FullIRI "class.iri")
--
-- >>> parseTest atomic "Person"
-- AtomicClass (SimpleIRI "Person")
--
-- >>> parseTest atomic "{ <class.iri#ind1>, <class.iri#ind2> }"
-- AtomicIndividuals (IRIIndividual (FullIRI "class.iri#ind1") :| [IRIIndividual (FullIRI "class.iri#ind2")])
--
atomic :: Parser Atomic
atomic =  AtomicClass       <$> classIRI
      <|> AtomicIndividuals <$> enclosedS '{' (nonEmptyList individual)
      <|> AtomicDescription <$> enclosedS '(' description


--------------------------------
--- Frames and Miscellaneous ---
--------------------------------

-- | It parses a datatype frame
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [
--         "Datatype: NegInt"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "EquivalentTo: Annotations: rdf:comment \"General Domain\" integer[< 0]"
--       ]
-- :}
--
-- >>> parseTest (datatypeFrame *> eof) (T.unlines input)
-- ()
--
datatypeFrame :: Parser DatatypeFrame
datatypeFrame = do
  dtype   <- symbol "Datatype:" *> datatype
  annots  <- many $ symbol "Annotations:" *> annotatedList annotation
  equiv   <- optional $ AnnotDataRange <$> (symbol "EquivalentTo:" *> optional annotations) <*> dataRange -- TODO: in the specifications the EquivalentTo *should always* followed by the "Annotations:" string. However this may be an error, as a later example the EquivalentTo is not followed by any annotation
  annots' <- many $ symbol "Annotations:" *> annotatedList annotation
  pure $ DatatypeF dtype (annots <> annots') equiv

-- | It parses a class frame
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [
--         "Class: Person"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "  SubClassOf: owl:Thing that hasFirstName exactly 1 and hasFirstName only string[minLength 1]"
--       , "  SubClassOf: hasAge exactly 1 and hasAge only not NegInt"
--       , "  SubClassOf: hasGender exactly 1 and hasGender only {female , male}"
--       , "  SubClassOf: not hates Self"
--       , "  SubClassOf: hasSSN max 1, hasSSN min 1"
--       , "  EquivalentTo: g:People"
--       , "  DisjointWith: g:Rock , g:Mineral"
--       , "  DisjointUnionOf: Child, Adult"
--       , "  HasKey: hasSSN"
--       ]
-- :}
--
-- >>> parseTest (classFrame *> eof) (T.unlines input)
-- ()
--
-- TODO-check-1: in specs `sndChoice` (aka `HasKey`) is an alternative to the others
-- I think this is wrong; and `HasKey` should be included in the list of alternatives
-- TODO-check-2: in specs the annotations of `HasKey` and `DisjointUnionOf` are required,
-- but I think is wrong. I made them optional :)
--
classFrame :: Parser ClassFrame
classFrame = do
  clsIRI <- symbol "Class:" *> classIRI
  blob   <- many choices
  pure $ ClassF clsIRI blob
 where
  choices :: Parser ClassElement
  choices =  AnnotationCE      <$> (symbol "Annotations:" *> annotatedList annotation)
         <|> SubClassOfCE      <$> (symbol "SubClassOf:" *> annotatedList description)
         <|> EquivalentToCE    <$> (symbol "EquivalentTo:" *> annotatedList description)
         <|> DisjointToCE      <$> (symbol "DisjointWith:" *> annotatedList description)
         <|> DisjointUnionOfCE <$> (symbol "DisjointUnionOf:" *> optional annotations)
                               <*> listOfAtLeast2 description
         <|> HasKeyCE          <$> (symbol "HasKey:" *> optional annotations)
                               <*> (NE.fromList <$> some ((ObjectPE <$> objectPropertyExpression)
                                              <|> (DataPE   <$> dataPropertyExpression)))
--  nonEmptyDPE = NonEmptyD <$> many objectPropertyExpression <*> nonEmptyList dataPropertyExpression

-- | It parses an object property
--
-- >>> :{
-- let input :: [Text]
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
-- >>> parseTest (objectPropertyFrame *> eof) (T.unlines input)
-- ()
--
objectPropertyFrame :: Parser ObjectPropertyFrame
objectPropertyFrame = ObjectPropertyF <$> (symbol "ObjectProperty:" *> objectPropertyIRI) <*> many altr
 where
  altr =  AnnotationOPE       <$> (symbol "Annotations:"      *> annotatedList annotation)
      <|> DomainOPE           <$> (symbol "Domain:"           *> annotatedList description)
      <|> RangeOPE            <$> (symbol "Range:"            *> annotatedList description)
      <|> CharacteristicsOPE  <$> (symbol "Characteristics:"  *> annotatedList objectPropertyCharacteristic)
      <|> SubPropertyOfOPE    <$> (symbol "SubPropertyOf:"    *> annotatedList objectPropertyExpression)
      <|> EquivalentToOPE     <$> (symbol "EquivalentTo:"     *> annotatedList objectPropertyExpression)
      <|> DisjointWithOPE     <$> (symbol "DisjointWith:"     *> annotatedList objectPropertyExpression)
      <|> InverseOfOPE        <$> (symbol "InverseOf:"        *> annotatedList objectPropertyExpression)
      <|> SubPropertyChainOPE <$> (symbol "SubPropertyChain:" *> optional annotations) <*>
                                    (atLeast2List' <$> objectPropertyExpression
                                                   <*> nonEmptyList (symbol "o" *> objectPropertyExpression))

-- | It parses one of the permitted object property characteristics
--
-- >>> parseTest (objectPropertyCharacteristic <* eof) "InverseFunctional"
-- INVERSE_FUNCTIONAL
--
-- >>> parseTest objectPropertyCharacteristic "Functionalandmore"
-- FUNCTIONAL
--
-- >>> parseTest objectPropertyCharacteristic "Random"
-- ...
-- unexpected "Random"
-- ...
--
objectPropertyCharacteristic :: Parser ObjectPropertyCharacteristics
objectPropertyCharacteristic =
            symbol "Functional"        $> FUNCTIONAL
        <|> symbol "InverseFunctional" $> INVERSE_FUNCTIONAL
        <|> symbol "Reflexive"         $> REFLEXIVE
        <|> symbol "Irreflexive"       $> IRREFLEXIVE
        <|> symbol "Symmetric"         $> SYMMETRIC
        <|> symbol "Asymmetric"        $> ASYMMETRIC
        <|> symbol "Transitive"        $> TRANSITIVE


-- | It parses an data property
--
-- >>> :{
-- let input :: [Text]
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
-- >>> parseTest (dataPropertyFrame *> eof) (T.unlines input)
-- ()
--
-- TODO-check: 'annotations' in 'characteristics are probably optional
dataPropertyFrame :: Parser DataPropertyFrame
dataPropertyFrame = DataPropertyF <$> (symbol "DataProperty:" *> dataPropertyIRI) <*> many altr
 where
  altr =  AnnotationDPE       <$> (symbol "Annotations:"     *> annotatedList annotation)
      <|> DomainDPE           <$> (symbol "Domain:"          *> annotatedList description)
      <|> RangeDPE            <$> (symbol "Range:"           *> annotatedList dataRange)
      <|> CharacteristicsDPE  <$> (symbol "Characteristics:" *> annotatedList dataPropertyCharacteristic)
      <|> SubPropertyOfDPE    <$> (symbol "SubPropertyOf:"   *> annotatedList dataPropertyExpression)
      <|> EquivalentToDPE     <$> (symbol "EquivalentTo:"    *> annotatedList dataPropertyExpression)
      <|> DisjointWithDPE     <$> (symbol "DisjointWith:"    *> annotatedList dataPropertyExpression)


-- | It parses data property characteristics. Currently only 'functional' property is supported
--
-- >>> parseTest (dataPropertyCharacteristic <* eof) "Functional"
-- FUNCTIONAL_DPE
--
-- >>> parseTest dataPropertyCharacteristic "Functionalandmore"
-- FUNCTIONAL_DPE
--
dataPropertyCharacteristic :: Parser DataPropertyCharacteristics
dataPropertyCharacteristic = symbol "Functional" $> FUNCTIONAL_DPE

-- | It parses an annotation property
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [ "AnnotationProperty: creator"
--       , "Annotations: rdfs:comment \"General domain\", creator John"
--       , "Domain: Person , Woman"
--       , "Range: <integer>" -- FIX www: range cannot be a datatype -> it's IRI
--       , "SubPropertyOf: hasVerifiedAge , hasSomeAge"
--       ]
-- :}
--
-- >>> parseTest (annotationPropertyFrame *> eof) (T.unlines input)
-- ()
--
annotationPropertyFrame :: Parser AnnotationPropertyFrame
annotationPropertyFrame = AnnotationPropertyF <$> (symbol "AnnotationProperty:" *> annotationPropertyIRI)
                                              <*> many altr
 where
  altr =  AnnotationAPE       <$> (symbol "Annotations:"     *> annotatedList annotation)
      <|> DomainAPE           <$> (symbol "Domain:"          *> annotatedList iri)
      <|> RangeAPE            <$> (symbol "Range:"           *> annotatedList iri)
      <|> SubPropertyOfAPE    <$> (symbol "SubPropertyOf:"   *> annotatedList annotationPropertyIRI)

-- | It parses an individual frame
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [ "Individual: John"
--       , "  Annotations: rdfs:creator \"John\""
--       , "  Types: Person, hasFirstName value \"John\" or hasFirstName value \"Jack\"^^xsd:string"
--       , "  Facts: hasWife Mary, not hasChild Susan, hasAge 33, hasChild _:child1"
--       , "  DifferentFrom: Susan"
--       , "  SameAs: Jack , Bob"
--       ]
--     input2 :: [Text]
--     input2 = "Individual: _:child1":(tail input)
-- :}
--
-- >>> parseTest (individualFrame *> eof) (T.unlines input)
-- ()
--
-- >>> parseTest (individualFrame *> eof) (T.unlines input2)
-- ()
--
individualFrame :: Parser IndividualFrame
individualFrame = IndividualF <$> (symbol "Individual:" *> individual) <*> many altr
 where
  altr =  AnnotationIE    <$> (symbol "Annotations:"   *> annotatedList annotation)
      <|> TypeIE          <$> (symbol "Types:"         *> annotatedList description)
      <|> FactIE          <$> (symbol "Facts:"         *> annotatedList fact)
      <|> SameAsIE        <$> (symbol "SameAs:"        *> annotatedList individual)
      <|> DifferentFromIE <$> (symbol "DifferentFrom:" *> annotatedList individual)

fact :: Parser Fact
fact = do
  neg <- optionalNegation
  fct <- (ObjectPropertyFE <$> try objectPropertyFact) <|> (DataPropertyFE <$> try dataPropertyFact)
  pure $ const fct <$> neg

objectPropertyFact :: Parser ObjectPropertyFact
objectPropertyFact = ObjectPropertyFact <$> objectPropertyIRI <*> individual

dataPropertyFact :: Parser DataPropertyFact
dataPropertyFact = DataPropertyFact <$> dataPropertyIRI <*> literal

-- | It parses an class miscelaneous properties
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [ "DisjointClasses: g:Rock, g:Scissor, g:Paper"
--       , "EquivalentProperties: hates, loathes, despises"
--       , "DisjointProperties: hates, loves, indifferent"
--       , "EquivalentProperties: favoriteNumber, g:favouriteNumber, g:favouriteInteger"
--       , "DisjointProperties: favoriteInteger, favouriteReal"
--       , "SameIndividual: John, Jack, Joe, Jim"
--       , "DifferentIndividuals: John, Susan, Mary, Jill"
--       ]
-- :}
--
-- >>> parseTest (many misc *> eof) (T.unlines input)
-- ()
--
-- TODO-check I converted all required annotation to annotated list
misc :: Parser Misc
misc =
    EquivalentClasses
        <$> (symbol "EquivalentClasses:" *> optional annotations) <*> listOfAtLeast2 description
    <|> DisjointClasses
        <$> (symbol "DisjointClasses:"   *> optional annotations) <*> listOfAtLeast2 description
    <|> EquivalentObjectProperties
        <$> (symbol "EquivalentProperties:" *> optional annotations) <*> listOfAtLeast2 objectPropertyExpression
    <|> DisjointObjectProperties
        <$> (symbol "DisjointProperties:" *> optional annotations) <*> listOfAtLeast2 objectPropertyExpression
    <|> EquivalentDataProperties
        <$> (symbol "EquivalentProperties:" *> optional annotations) <*> listOfAtLeast2 dataPropertyExpression
    <|> DisjointDataProperties
        <$> (symbol "DisjointProperties:" *> optional annotations) <*> listOfAtLeast2 dataPropertyExpression
    <|> SameIndividual
        <$> (symbol "SameIndividual:" *> optional annotations) <*> listOfAtLeast2 individual
    <|> DifferentIndividuals
        <$> (symbol "DifferentIndividuals:" *> optional annotations) <*> listOfAtLeast2 individual

-----------------------
--- Generic parsers ---
-----------------------

optionalNegation :: Parser (WithNegation ())
optionalNegation = maybe (Positive ()) (const (Negative ())) <$> (optional . symbol $ "not")

-- | It parser one or more elements parsed by the input parser p and separated by the input string
--
-- >>> parseTest (singleOrMany "," . string $ "test") "test"
-- "test" :| []
--
-- >>> parseTest (singleOrMany "or" . lexeme . string $ "test") "test or test or test"
-- "test" :| ["test","test"]
--
singleOrMany :: Text -> Parser p -> Parser (NonEmpty p)
singleOrMany sep p =
  let multipleP = (:|) <$> p <*> some (symbol sep *> p) in try multipleP <|> (pure <$> p)

-- | It parses non empty lists
--
-- >>> parseTest (nonEmptyList languageTag) "@en, @el, @test"
-- "en" :| ["el","test"]
--
-- >>> parseTest (nonEmptyList languageTag) ""
-- ...
-- unexpected end of input
-- expecting '@'
--
nonEmptyList :: Parser p -> Parser (NonEmpty p)
nonEmptyList p = (:|) <$> lexeme p <*> many (symbol "," *> lexeme p)

-- | It parses lists with at least two elements
--
-- >>> parseTest (listOfAtLeast2 languageTag) "@en, @el, @test"
-- ("en","el") :# ["test"]
--
-- >>> parseTest (listOfAtLeast2 languageTag) "@en"
-- ...
-- unexpected end of input
-- ...
--
listOfAtLeast2 :: Parser p -> Parser (AtLeast2List p)
listOfAtLeast2 p = atLeast2List' <$> p <*> (symbol "," *> nonEmptyList p)

-- | It parses non empty annotated lists
--
-- >>> parseTest (annotatedList description *> eof) "Man, Person"
-- ()
--
annotatedList :: Parser p -> Parser (AnnotatedList p)
annotatedList p =
  let annotationList = (,) <$> optional annotations <*> p
  in  AnnList <$> nonEmptyList annotationList

predifinedPrefixes :: [PrefixDeclaration]
predifinedPrefixes =
  [ PrefixD "rdf"  (FullIRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  , PrefixD "rdfs" (FullIRI "http://www.w3.org/2000/01/rdf-schema#")
  , PrefixD "xsd"  (FullIRI "http://www.w3.org/2001/XMLSchema#")
  , PrefixD "owl"  (FullIRI "http://www.w3.org/2002/07/owl#")
  ]

parseOntologyDoc :: FilePath -> IO (Maybe OntologyDocument)
parseOntologyDoc file =
  putStrLn ("Parsing ontology document: '" <> file <> "'") >>
  T.readFile file >>= parseContent
  where
    parseContent content =
      case parse ontologyDocument file content of
        Left bundle -> do
          putStrLn "Unable to parse file. Reason: "
          putStrLn (errorBundlePretty bundle)
          pure Nothing
        Right doc -> do
          putStrLn "File parsed succesfully"
          pure (Just doc)
