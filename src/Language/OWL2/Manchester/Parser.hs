{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Manchester.Parser where

import           Data.Functor                             ( ($>) )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Text.Megaparsec

import           Language.OWL2.Import                     ( Text )
import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types
import           Language.OWL2.Internal.Parser

-- DocTest setup
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Megaparsec.Char (string)

--------------------------
-- Parser related types --
--------------------------

-- | It parses literals
--
-- >>> parseTest literal "\"32\"^^integer"
-- TypedLiteralC (TypedL "32" (Datatype {unDatatype = AbbreviatedIRI "xsd" "integer"}))
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
-- TypedL "32" (Datatype {unDatatype = AbbreviatedIRI "xsd" "integer"})
--
-- >>> parseTest typedLiteral "\"Jack\"^^xsd:string"
-- TypedL "Jack" (Datatype {unDatatype = AbbreviatedIRI "xsd" "string"})
--
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue <*> (symbol "^^" *> datatype)

-- | It parses class IRIs
classIRI :: Parser IRI
classIRI = iri

-- | It parses datatypes
--
-- >>> parseTest datatype "<http://example.iri>"
-- Datatype {unDatatype = FullIRI "http://example.iri"}
--
-- >>> parseTest datatype "integer"
-- Datatype {unDatatype = AbbreviatedIRI "xsd" "integer"}
--
-- >>> parseTest datatype "xsd:string"
-- Datatype {unDatatype = AbbreviatedIRI "xsd" "string"}
--
datatype :: Parser Datatype
datatype =  Datatype <$> (try datatypeIRI <|> knownDTs)
 where
  knownDTs :: Parser IRI
  knownDTs =  AbbreviatedIRI "xsd" "integer" <$ symbol "integer"
          <|> AbbreviatedIRI "xsd" "decimal" <$ symbol "decimal"
          <|> AbbreviatedIRI "xsd" "float"   <$ symbol "float"
          <|> AbbreviatedIRI "xsd" "string"  <$ symbol "string"

datatypeIRI :: Parser IRI
datatypeIRI = iri

objectPropertyIRI :: Parser IRI
objectPropertyIRI = iri

dataPropertyIRI :: Parser IRI
dataPropertyIRI = iri

entity :: Parser Entity
entity = choice $ fmap (\(s, p) -> symbol s *> p) alts
 where
  alts :: [(Text, Parser Entity)]
  alts =
    [ ("Datatype"          , EntityDatatype           <$> datatype)
    , ("Class"             , EntityClass              <$> classIRI)
    , ("ObjectProperty"    , EntityObjectProperty     <$> objectPropertyIRI)
    , ("DataProperty"      , EntityDataProperty       <$> dataPropertyIRI)
    , ("AnnotationProperty", EntityAnnotationProperty <$> annotationPropertyIRI)
    , ("NamedIndividual"   , EntityIndividual         <$> namedIndividual)
    ]


------------------------------
-- Ontology and Annotations --
------------------------------

mAnnotation :: Parser Annotation
mAnnotation = annotation literal

-- | It parses annotation sections
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
-- >>> parseTest (annotationSection *> eof) (T.unlines input)
-- ()
--
annotationSection :: Parser Annotations
annotationSection = let p = symbol "Annotations:" *> annotatedList mAnnotation
                    in maybe [] NE.toList <$> optional p

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
  annots  <- concat <$> many annotationSection
  axms  <- many axioms
  pure $ Ontology ontoIRI imports annots (concat axms)

-- | It parses import statements
--
-- >>> parseTest importStmt "Import: <http://ex.com/owl2/families.owl>"
-- ImportD (FullIRI "http://ex.com/owl2/families.owl")
--
importStmt :: Parser ImportDeclaration
importStmt = ImportD <$> (symbol "Import:" *> iri)

axioms :: Parser [Axiom]
axioms =  map AxiomDT    <$> datatypeAxiom
      <|> map AxiomC     <$> classAxioms
      <|> map AxiomOP    <$> objectPropertyAxioms
      <|> map AxiomDP    <$> dataPropertyAxioms
      <|> map AxiomAP    <$> annotationPropertyAxioms
      <|> map AxiomI     <$> assertionAxioms
      <|> misc


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
dataRange = do
  lst <- singleOrMany "or" dataConjunction
  pure $ case lst of
    (x :| []) -> x
    (x :| (y:ys)) -> UnionDR $ (x,y) :# ys


-- | It parses a data conjunction (i.e. 'and')
--
-- >>> parseTest (dataConjunction *> eof) "integer[<10] and integer[>0]"
-- ()
--
dataConjunction :: Parser DataRange
dataConjunction = do
  lst <- singleOrMany "and" dataPrimary
  pure $ case lst of
    (x :| []) -> x
    (x :| (y:ys)) -> IntersectionDR $ (x,y) :# ys

-- | It parses a data primary
--
-- >>> parseTest dataPrimary "integer[<0]"
-- RestrictionDR (DatatypeRestriction (Datatype {unDatatype = AbbreviatedIRI "xsd" "integer"}) (RestrictionExp L_FACET (IntegerLiteralC (IntegerL 0)) :| []))
--
dataPrimary :: Parser DataRange
dataPrimary = do
  neg <- optionalNegation
  da  <- dataAtomic
  pure $ case neg of
    Negative _ -> ComplementDR da
    Positive _ -> da

-- | It parses an atomic data
--
-- >>> parseTest (dataAtomic *> eof)  "integer[<0]"
-- ()
--
dataAtomic :: Parser DataRange
dataAtomic =  try datatypeRestriction
          <|> DatatypeDR <$> try datatype
          <|> enclosedS '{' literalList
          <|> enclosedS '(' dataRange

-- | It parsers a non empty list of literal
--
-- >>> parseTest literalList "\"kostas\", 32, \"true\""
-- OneOfDR (StringLiteralNoLang "kostas" :| [IntegerLiteralC (IntegerL 32),StringLiteralNoLang "true"])
--
literalList :: Parser DataRange
literalList = OneOfDR <$> nonEmptyList literal

-- | It parses datatype restrictions
--
-- >>> parseTest (datatypeRestriction *> eof) "integer[> 0, maxLength 2]"
-- ()
--
-- >>> parseTest datatypeRestriction "integer[< 0]"
-- RestrictionDR (DatatypeRestriction (Datatype {unDatatype = AbbreviatedIRI "xsd" "integer"}) (RestrictionExp L_FACET (IntegerLiteralC (IntegerL 0)) :| []))
--
datatypeRestriction :: Parser DataRange
datatypeRestriction = do
  dt <- datatype
  _  <- symbol "["
  rvList <- nonEmptyList (RestrictionExp <$> facet <*> restrictionValue)
  _  <- symbol "]"
  pure . RestrictionDR $ DatatypeRestriction dt rvList

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
description :: Parser ClassExpression
description = do
  conj <- singleOrMany "or" conjunction
  pure $ case conj of
    (x :| [])     -> x
    (x :| (y:ys)) -> CExpObjectUnionOf ((x,y) :# ys)

-- | It parses a conjunction
--
-- >>> parseTest (conjunction *> eof) "hasFirstName value \"John\""
-- ()
--
-- >>> parseTest (conjunction *> eof) "Person and Man"
-- ()
--
-- >>> parseTest conjunction "Person"
-- CExpClass (SimpleIRI "Person")
--
-- >>> parseTest (conjunction *> eof) "owl:Thing that hasFirstName exactly 1"
-- ()
--
-- >>> parseTest (conjunction *> eof) "p some a and p only b"
-- ()
--
-- >>> parseTest (conjunction <* eof) "test:Class1 that not test:Class2"
-- CExpObjectIntersectionOf ((CExpClass (AbbreviatedIRI "test" "Class1"),CExpObjectComplementOf (CExpClass (AbbreviatedIRI "test" "Class2"))) :# [])
--
conjunction :: Parser ClassExpression
conjunction =  try restrictions <|> try prims
 where
  prims = do
    pr <- singleOrMany "and" primary
    pure $ case pr of
      (x :| [])     -> x
      (x :| (y:ys)) -> CExpObjectIntersectionOf $ (x, y) :# ys
  restrictions = do
    clsIRI    <- classIRI <* symbol "that"
    (rst:|xs) <- singleOrMany "and" restWithNeg
    pure . CExpObjectIntersectionOf $ (CExpClass clsIRI, rst) :# xs
  restWithNeg = do
    neg  <- optionalNegation
    rst  <- primary
    pure $ case neg of
      Positive _ -> rst
      Negative _ -> CExpObjectComplementOf rst

-- | It parses a primary
--
-- >>> parseTest (primary *> eof) "hasFirstName value \"John\""
-- ()
--
-- >>> parseTest primary "not Person"
-- CExpObjectComplementOf (CExpClass (SimpleIRI "Person"))
--
-- >>> parseTest (primary *> eof) "not hasFirstName value \"John\""
-- ()
--
primary :: Parser ClassExpression
primary = do
  neg  <- optionalNegation
  expr <- try restriction <|> try atomic
  pure $ case neg of
    Positive _ -> expr
    Negative _ -> CExpObjectComplementOf expr

-- | It parses one of the many differnt type of restrictions on object or data properties
--
-- >>> parseTest (restriction <* eof) "hasFirstName value \"John\""
-- CExpDataHasValue (SimpleIRI "hasFirstName") (StringLiteralNoLang "John")
--
-- >>> parseTest (restriction <* eof) "hasFirstName exactly 1"
-- CExpObjectExactCardinality 1 (OPE (SimpleIRI "hasFirstName")) Nothing
--
-- >>> parseTest (restriction *> eof) "hasFirstName only string[minLength 1]"
-- ()
--
restriction :: Parser ClassExpression
restriction =  try (objectPropertyExpression >>= objectRestriction)
           <|> try (dataPropertyExpression >>= dataRestriction)
 where
  objectRestriction :: ObjectPropertyExpression -> Parser ClassExpression
  objectRestriction o =
        try (CExpObjectSomeValuesFrom o <$> (symbol "some"  *> primary))
    <|> try (CExpObjectAllValuesFrom o   <$> (symbol "only"  *> primary))
    <|> try (CExpObjectHasValue o <$> (symbol "value" *> individual))
    <|> symbol "Self"    $> CExpObjectHasSelf o
    <|> try (CExpObjectMinCardinality   <$> (symbol "min"     *> nonNegativeInteger) <*> pure o <*> optional primary)
    <|> try (CExpObjectMaxCardinality   <$> (symbol "max"     *> nonNegativeInteger) <*> pure o <*> optional primary)
    <|> try (CExpObjectExactCardinality <$> (symbol "exactly" *> nonNegativeInteger) <*> pure o <*> optional primary)
  dataRestriction :: DataPropertyExpression -> Parser ClassExpression
  dataRestriction d =
        try (CExpDataSomeValuesFrom d <$> (symbol "some" *> dataPrimary))
    <|> try (CExpDataAllValuesFrom  d <$> (symbol "only" *> dataPrimary))
    <|> try (CExpDataHasValue d       <$> (symbol "value"   *> literal))
    <|> try (CExpDataMinCardinality   <$> (symbol "min"     *> nonNegativeInteger) <*> pure d <*> optional dataPrimary)
    <|> try (CExpDataMaxCardinality   <$> (symbol "max"     *> nonNegativeInteger) <*> pure d <*> optional dataPrimary)
    <|> try (CExpDataExactCardinality <$> (symbol "exactly" *> nonNegativeInteger) <*> pure d <*> optional dataPrimary)

-- | It parses a class IRI or a list of individual IRIs
--
-- >>> parseTest atomic "<class.iri>"
-- CExpClass (FullIRI "class.iri")
--
-- >>> parseTest atomic "Person"
-- CExpClass (SimpleIRI "Person")
--
-- >>> parseTest atomic "{ <class.iri#ind1>, <class.iri#ind2> }"
-- CExpObjectOneOf (NamedIRI (FullIRI "class.iri#ind1") :| [NamedIRI (FullIRI "class.iri#ind2")])
--
atomic :: Parser ClassExpression
atomic =  CExpClass       <$> classIRI
      <|> CExpObjectOneOf <$> enclosedS '{' (nonEmptyList individual)
      <|> enclosedS '(' description


--------------------------------
--- Axioms and Miscellaneous ---
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
-- >>> parseTest (datatypeAxiom *> eof) (T.unlines input)
-- ()
--
datatypeAxiom :: Parser [DatatypeAxiom]
datatypeAxiom = do
  dtype <- symbol "Datatype:" *> datatype
  axms <- many . choice $ ($ dtype) <$> [annotDTA, equDTA]  --choices
  pure $ concat axms
 where
  annotDTA c = do
    _ <- symbol "Annotations:"
    mAn <- annotatedList mAnnotation
    pure $ spreadAnnotations DatatypeAxiomAnnotation c mAn
  equDTA c = do
    _ <- symbol "EquivalentTo:"
    annots <- annotationSection
    dr <- dataRange
    pure [ DatatypeAxiomEquivalent annots c dr ]
--datatypeAxiom :: Parser DatatypeAxiom
--datatypeAxiom = do
--  dtype   <- symbol "Datatype:" *> datatype
--  annots  <- many $ symbol "Annotations:" *> annotatedList mAnnotation
--  equiv   <- optional $ AnnotDataRange <$> (symbol "EquivalentTo:" *> annotationSection) <*> dataRange -- TODO: in the specifications the EquivalentTo *should always* followed by the "Annotations:" string. However this may be an error, as a later example the EquivalentTo is not followed by any annotation
--  annots' <- many $ symbol "Annotations:" *> annotatedList mAnnotation
--  pure $ DatatypeF dtype (annots <> annots') equiv

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
-- >>> parseTest (classAxioms *> eof) (T.unlines input)
-- ()
--
classAxioms :: Parser [ClassAxiom]
classAxioms = do
  clsIRI <- symbol "Class:" *> classIRI
  let x = CExpClass clsIRI
  axms <- many . choice $ ($ x) <$> [annotCA, subCA, equCA,  disCA, const (dscCA clsIRI), keyCA]  --choices
  pure $ concat axms
 where
  annotCA c = do
    _ <- symbol "Annotations:"
    mAn <- annotatedList mAnnotation
    pure $ spreadAnnotations ClassAxiomAnnotation c mAn
  subCA c = do
    _ <- symbol "SubClassOf:"
    ds <- annotatedList description
    pure $ spreadAnnotations ClassAxiomSubClassOf c ds
  equCA c = do
    _ <- symbol "EquivalentTo:"
    ds <- annotatedList description
    pure $ spreadAnnotationsIfExist ClassAxiomEquivalentClasses c ds 
  disCA c = do
    _ <- symbol "DisjointWith:"
    ds <- annotatedList description
    pure $ spreadAnnotationsIfExist ClassAxiomDisjointClasses c ds
  dscCA c = do
    _ <- symbol "DisjointUnionOf:"
    an <- annotationSection
    ds <- listOfAtLeast2 description
    pure [ClassAxiomDisjointUnion an c ds]
  keyCA c = do
    _ <- symbol "HasKey:"
    an <- annotationSection
    od <- NE.fromList <$> some ((ObjectPE <$> objectPropertyExpression) <|> (DataPE <$> dataPropertyExpression))
    pure [ClassAxiomHasKey an c od]


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
-- >>> parseTest (objectPropertyAxioms *> eof) (T.unlines input)
-- ()
--
objectPropertyAxioms :: Parser [ObjectPropertyAxiom]
objectPropertyAxioms = do
  opIRI <- symbol "ObjectProperty:" *> objectPropertyIRI
  let x = OPE opIRI
  axms <- many . choice $ ($ x) <$> [annotAxiom, domainAxiom, rangeAxiom, charAxiom, subAxiom, subChainAxiom, equAxiom, disAxiom, invAxiom] --choices
  pure $ concat axms
 where
  annotAxiom c = do
    _ <- symbol "Annotations:"
    mAn <- annotatedList mAnnotation
    pure $ spreadAnnotations ObjectPAnnotation c mAn
  domainAxiom c = do
    _ <- symbol "Domain:"
    mAn <- annotatedList description
    pure $ spreadAnnotations ObjectPDomain c mAn
  rangeAxiom c = do
    _ <- symbol "Range:"
    mAn <- annotatedList description
    pure $ spreadAnnotations ObjectPRange c mAn
  charAxiom c = do
    _ <- symbol "Characteristics:"
    chars <- annotatedList objectPropertyCharacteristic
    pure $ spreadAnnotations ObjectPCharacteristics c chars
  subAxiom c = do
    _ <- symbol "SubPropertyOf:"
    chars <- annotatedList objectPropertyExpression
    pure $ spreadAnnotations ObjectPSubProperty c chars
  subChainAxiom c = do
    _ <- symbol "SubPropertyChain:"
    annots <- annotationSection
    hd   <- objectPropertyExpression 
    rest <- nonEmptyList (symbol "o" *> objectPropertyExpression)
    let chain = ObjectPropertyChain $ atLeast2List' hd rest
    pure [ObjectPChainSubProperty annots chain c]
  equAxiom c = do
    _ <- symbol "EquivalentTo:"
    exps <- annotatedList objectPropertyExpression
    pure $ spreadAnnotationsIfExist ObjectPEquivalent c exps
  disAxiom c = do
    _ <- symbol "DisjointWith:"
    exps <- annotatedList objectPropertyExpression
    pure $ spreadAnnotationsIfExist ObjectPDisjoint c exps
  invAxiom c = do
    _ <- symbol "InverseOf:"
    exps <- annotatedList objectPropertyExpression
    pure $ spreadAnnotations ObjectPInverse c exps


noAnnotations :: AnnotatedList a -> Bool
noAnnotations = all (null . fst . unAnnotated) 

removeAnnotations :: AnnotatedList a -> NonEmpty a
removeAnnotations = fmap (snd . unAnnotated) 

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
objectPropertyCharacteristic :: Parser ObjectPropertyCharacteristic
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
-- >>> parseTest (dataPropertyAxioms *> eof) (T.unlines input)
-- ()
--
dataPropertyAxioms :: Parser [DataPropertyAxiom]
dataPropertyAxioms = do
  dpIRI <- symbol "DataProperty:" *> dataPropertyIRI
  axms <- many . choice $ ($ dpIRI) <$> [annotAxiom, domainAxiom, rangeAxiom, charAxiom, subAxiom, equAxiom, disAxiom] --choices
  pure $ concat axms
 where
  annotAxiom c = do
    _ <- symbol "Annotations:"
    annots <- annotatedList mAnnotation
    pure $ spreadAnnotations DataPAnnotation c annots
  domainAxiom c = do
    _ <- symbol "Domain:"
    exps <- annotatedList description
    pure $ spreadAnnotations DataPDomain c exps
  rangeAxiom c = do
    _ <- symbol "Range:"
    drs <- annotatedList dataRange
    pure $ spreadAnnotations DataPRange c drs
  charAxiom c = do
    _ <- symbol "Characteristics:"
    chars <- annotatedList dataPropertyCharacteristic
    pure $ spreadAnnotations DataPCharacteristics c chars
  subAxiom c = do
    _ <- symbol "SubPropertyOf:"
    exps <- annotatedList dataPropertyExpression
    pure $ spreadAnnotations DataPSubProperty c exps
  equAxiom c = do
    _ <- symbol "EquivalentTo:"
    exps <- annotatedList dataPropertyExpression
    pure $ spreadAnnotationsIfExist DataPEquivalent c exps
  disAxiom c = do
    _ <- symbol "DisjointWith:"
    exps <- annotatedList dataPropertyExpression
    pure $ spreadAnnotationsIfExist DataPDisjoint c exps

-- | A small utility function with specific functionallity, defined in order to avoid repetition
spreadAnnotationsIfExist :: (Annotations -> AtLeast2List a -> b) -> a -> AnnotatedList a -> [b]
spreadAnnotationsIfExist c e als = 
    if noAnnotations als
    then pure $ c [] (atLeast2List' e (removeAnnotations als)) 
    else (\(Annotated (a, v)) -> c a (atLeast2List e v [])) <$> NE.toList als

spreadAnnotations :: (Annotations -> a1 -> a2 -> b) -> a1 -> NonEmpty (Annotated a2) -> [b]
spreadAnnotations c e als = (\(Annotated (a, v)) -> c a e v) <$> NE.toList als

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
--       , "Annotations: rdfs:label _:someNode"
--       , "Domain: Person , Woman"
--       , "Range: <integer>" -- FIX www: range cannot be a datatype -> it's IRI
--       , "SubPropertyOf: hasVerifiedAge , hasSomeAge"
--       ]
-- :}
--
-- >>> parseTest (annotationPropertyAxioms *> eof) (T.unlines input)
-- ()
--
annotationPropertyAxioms :: Parser [AnnotationPropertyAxiom]
annotationPropertyAxioms = do
  apIRI <- symbol "AnnotationProperty:" *> annotationPropertyIRI
  axms <- many . choice $ ($ apIRI) <$> [annotAxiom, domainAxiom, rangeAxiom, subAxiom] --choices
  pure $ concat axms
 where
  annotAxiom c = do
    _ <- symbol "Annotations:"
    annots <- annotatedList mAnnotation
    pure $ spreadAnnotations AnnotationPAnnotation c annots
  domainAxiom c = do
    _ <- symbol "Domain:"
    iris <- annotatedList iri
    pure $ spreadAnnotations AnnotationPDomain c iris
  rangeAxiom c = do
    _ <- symbol "Range:"
    iris <- annotatedList iri
    pure $ spreadAnnotations AnnotationPRange c iris
  subAxiom c = do
    _ <- symbol "SubPropertyOf:"
    exps <- annotatedList annotationPropertyIRI
    pure $ spreadAnnotations AnnotationPSubProperty c exps

-- | It parses an individual frame
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [ "Individual: John"
--       , "  Annotations: rdfs:creator \"John\""
--       , "  Types: Person, hasFirstName value \"John\" or hasFirstName value \"Jack\"^^xsd:string"
--       , "  Facts: hasWife Mary, not hasChild Susan, hasAge 33, hasChild _:child1"
--       , "  DifferentFrom: Susan, Tom"
--       , "  SameAs: Annotations: rdfs:creator \"John\" Jack , Bob"
--       ]
--     input2 :: [Text]
--     input2 = "Individual: _:child1":(tail input)
-- :}
--
-- >>> parseTest (length <$> assertionAxioms <* eof) (T.unlines input)
-- 10
--
-- >>> parseTest (length <$> assertionAxioms <* eof) (T.unlines input2)
-- 10
--
assertionAxioms :: Parser [AssertionAxiom]
assertionAxioms = do
  ind <- symbol "Individual:" *> individual
  axms <- many . choice $ ($ ind) <$> [annotAxiom, domainAxiom, rangeAxiom, sameAxiom, diffAxiom] --choices
  pure $ concat axms
 where
  annotAxiom c = do
    _ <- symbol "Annotations:"
    annots <- annotatedList mAnnotation
    pure $ spreadAnnotations AssertionAnnotation c annots
  domainAxiom c = do
    _ <- symbol "Types:"
    classes <- annotatedList description
    pure $ spreadAnnotations AssertionClass c classes
  rangeAxiom c = do
    _ <- symbol "Facts:"
    facts <- annotatedList fact
    pure $ mapper <$> NE.toList facts
     where
      mapper (Annotated (a, ObjectPropertyFact o i)) = AssertionObjectProperty a (OPE o) c i
      mapper (Annotated (a, NegativeObjectPropertyFact o i)) = AssertionNegativeObjectProperty a (OPE o) c i
      mapper (Annotated (a, DataPropertyFact d v)) = AssertionDataProperty a d c v
      mapper (Annotated (a, NegativeDataPropertyFact d v)) = AssertionNegativeDataProperty a d c v
  sameAxiom c = do
    _ <- symbol "SameAs:"
    exps <- annotatedList individual
    pure $ spreadAnnotationsIfExist AssertionSameIndividuals c exps
  diffAxiom c = do
    _ <- symbol "DifferentFrom:"
    exps <- annotatedList individual
    pure $ spreadAnnotationsIfExist AssertionDifferentIndividuals c exps


fact :: Parser FactElement
fact = do
  neg <- optionalNegation
  try (objectPropertyFact neg) <|> try (dataPropertyFact neg)

objectPropertyFact :: WithNegation () -> Parser FactElement
objectPropertyFact o = do
   opIRI <- objectPropertyIRI
   ind   <- individual
   pure $ case o of
     Positive _ -> ObjectPropertyFact opIRI ind
     Negative _ -> NegativeObjectPropertyFact opIRI ind

dataPropertyFact :: WithNegation () -> Parser FactElement
dataPropertyFact o = do
   dpIRI <- dataPropertyIRI
   lit   <- literal
   pure $ case o of
     Positive _ -> DataPropertyFact dpIRI lit
     Negative _ -> NegativeDataPropertyFact dpIRI lit

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
-- >>> parseTest (misc *> eof) (T.unlines input)
-- ()
--
misc :: Parser [Axiom]
misc = many . choice $ try <$> [equClM, disjClM, equOPM, disjOPM, equDPM, disjDPM, sameIndM, diffIndM]  --choices
 where
  equClM = do
    _      <- symbol "EquivalentClasses:"
    AxiomC <$> (ClassAxiomEquivalentClasses <$> annotationSection <*> listOfAtLeast2 description)
  disjClM = do
    _      <- symbol "DisjointClasses:"
    AxiomC <$> (ClassAxiomDisjointClasses <$> annotationSection <*> listOfAtLeast2 description)
  equOPM = do
    _      <- symbol "EquivalentProperties:"
    AxiomOP <$> (ObjectPEquivalent <$> annotationSection <*> listOfAtLeast2 objectPropertyExpression)
  disjOPM = do
    _      <- symbol "DisjointProperties:"
    AxiomOP <$> (ObjectPDisjoint <$> annotationSection <*> listOfAtLeast2 objectPropertyExpression)
  equDPM = do
    _      <- symbol "EquivalentProperties:"
    AxiomDP <$> (DataPEquivalent <$> annotationSection <*> listOfAtLeast2 dataPropertyExpression)
  disjDPM = do
    _      <- symbol "DisjointProperties:"
    AxiomDP <$> (DataPDisjoint <$> annotationSection <*> listOfAtLeast2 dataPropertyExpression)
  sameIndM = do
    _      <- symbol "SameIndividual:"
    AxiomI <$> (AssertionSameIndividuals <$> annotationSection <*> listOfAtLeast2 individual)
  diffIndM = do
    _      <- symbol "DifferentIndividuals:"
    AxiomI <$> (AssertionDifferentIndividuals <$> annotationSection <*> listOfAtLeast2 individual)

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


-- | It parses non empty annotated lists
--
-- >>> parseTest (annotatedList description *> eof) "Man, Person"
-- ()
--
annotatedList :: Parser p -> Parser (AnnotatedList p)
annotatedList p =
  let annotatedElement = Annotated <$> ((,) <$> annotationSection <*> p)
  in  nonEmptyList annotatedElement


