{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Manchester.Parser
  ( exportOntologyDoc
  , exportOntologyDocToFile
  , parseOntology
  , parseOntologyDoc
  , predifinedPrefixes
  )
where

import           Data.Functor                             ( ($>) )
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                               ( fromMaybe )
import           Text.Megaparsec

import           Language.OWL2.Import                     ( Text )
import qualified Language.OWL2.Import            as T

import           Language.OWL2.Types
import           Language.OWL2.Internal.Parser

-- DocTest setup
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Megaparsec.Char (string)

----------------------------------------------
-- Parser related types & utility functions --
----------------------------------------------
type AnnotatedList a = NonEmpty (Annotated a)

--------------------
-- Utility types ---
--------------------

data FactElement
    = ObjectPropertyFact ObjectPropertyIRI Individual
    | NegativeObjectPropertyFact ObjectPropertyIRI Individual
    | DataPropertyFact DataPropertyIRI Literal
    | NegativeDataPropertyFact DataPropertyIRI Literal deriving (Show)

-- | It parses literals
--
-- >>> parseTest literal "\"32\"^^integer"
-- TypedLiteralC {_typedLiteral = TypedL {_literalvalue = "32", _literalType = Datatype {_unDatatype = AbbreviatedIRI {_prefixName = "xsd", _prefixValue = "integer"}}}}
--
-- >>> parseTest literal "\"stringLiteralNoLanguage\""
-- StringLiteralNoLang {_noLangLiteral = "stringLiteralNoLanguage"}
--
-- >>> parseTest literal "\"stringLiteralWithLang\"@en"
-- StringLiteralLang {_literalWithLang = LiteralWithLang {_literalText = "stringLiteralWithLang", _langTag = "en"}}
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
-- TypedL {_literalvalue = "32", _literalType = Datatype {_unDatatype = AbbreviatedIRI {_prefixName = "xsd", _prefixValue = "integer"}}}
--
-- >>> parseTest typedLiteral "\"Jack\"^^xsd:string"
-- TypedL {_literalvalue = "Jack", _literalType = Datatype {_unDatatype = AbbreviatedIRI {_prefixName = "xsd", _prefixValue = "string"}}}
--
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue <*> (symbol "^^" *> datatype)

-- | It parses class IRIs
classIRI :: Parser IRI
classIRI = iri

-- | It parses datatypes
--
-- >>> parseTest datatype "<http://example.iri>"
-- Datatype {_unDatatype = FullIRI {_iriName = "http://example.iri"}}
--
-- >>> parseTest datatype "integer"
-- Datatype {_unDatatype = AbbreviatedIRI {_prefixName = "xsd", _prefixValue = "integer"}}
--
-- >>> parseTest datatype "xsd:string"
-- Datatype {_unDatatype = AbbreviatedIRI {_prefixName = "xsd", _prefixValue = "string"}}
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

------------------------------
-- Ontology and Annotations --
------------------------------

mAnnotation :: Parser Annotation
mAnnotation = annotation literal

-- | It parses annotation sections
-- Notice: This parser (as optional) should never be combined with "some", "many" or any other
-- similar parser, to avoid infinite loops. Use "many annotationSection'" instead
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
annotationSection = fromMaybe [] <$> optional annotationSection'

-- >>> :{
-- let input :: [Text]
--     input =
--      [ "Annotations: creator \"John\","
--      , "             Annotations: rdfs:comment \"Creation Year\" creationYear 2008,"
--      , "             mainClass Person"
--      ]
-- :}
--
-- >>> parseTest (annotationSection' *> eof) (T.unlines input)
-- ()
--
-- >>> parseTest (some annotationSection' *> eof) (T.unlines $ input <> input)
-- ()
--
annotationSection' :: Parser Annotations
annotationSection' = let p = symbol "Annotations:" *> annotatedList mAnnotation
                    in NE.toList <$> p

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyD <$> many prefixDeclaration <*> ontologyP

-- | It parses prefix names. Format: 'Prefix: <name>: <IRI>
--
-- >>> parseTest prefixDeclaration "Prefix: g: <http://ex.com/owl2/families#>"
-- PrefixD {_declerationPrefix = "g", _declerationIRI = FullIRI {_iriName = "http://ex.com/owl2/families#"}}
--
-- >>> parseTest prefixDeclaration "Prefix: : <http://ex.com/owl/families#>"
-- PrefixD {_declerationPrefix = "", _declerationIRI = FullIRI {_iriName = "http://ex.com/owl/families#"}}
--
prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = PrefixD <$> (symbol "Prefix:" *> lexeme prefixName) <*> fullIRI

ontologyP :: Parser Ontology
ontologyP = do
  _       <- symbol "Ontology:"
  ontoIRI <- optional $ OntologyVersionIRI <$> ontologyIRI <*> optional versionIRI -- Maybe (iri, Maybe iri)
  imps    <- many importStmt
  annots  <- concat <$> many annotationSection'
  axms    <- concat <$> many axiomsP --TODO problem here!!
  pure $ Ontology ontoIRI imps annots axms

-- | It parses import statements
--
-- >>> parseTest importStmt "Import: <http://ex.com/owl2/families.owl>"
-- ImportD {_importIRI = FullIRI {_iriName = "http://ex.com/owl2/families.owl"}}
--
importStmt :: Parser ImportDeclaration
importStmt = ImportD <$> (symbol "Import:" *> iri)

axiomsP :: Parser [Axiom]
axiomsP =  datatypeAxiom
      <|> classAxioms
      <|> objectPropertyAxioms
      <|> dataPropertyAxioms
      <|> annotationPropertyAxioms
      <|> assertionAxioms
      <|> pure <$> misc -- Axiom -> [Axiom]


-------------------------------------------
--- Properties and datatype expressions ---
-------------------------------------------

-- | It parses an object property expression
--
-- >>> parseTest objectPropertyExpression "<http://object-property-iri.com>"
-- OPE {_objectPropertyIRI = FullIRI {_iriName = "http://object-property-iri.com"}}
--
-- >>> parseTest objectPropertyExpression "inverse (<http://object-property-iri.com>)"
-- InverseOPE {_objectPropertyIRI = FullIRI {_iriName = "http://object-property-iri.com"}}
--
-- >>> parseTest objectPropertyExpression "inverse (test-ont:object-property)"
-- InverseOPE {_objectPropertyIRI = AbbreviatedIRI {_prefixName = "test-ont", _prefixValue = "object-property"}}
--
objectPropertyExpression :: Parser ObjectPropertyExpression
objectPropertyExpression =  OPE        <$> objectPropertyIRI
                        <|> InverseOPE <$> (symbol "inverse" *> parens objectPropertyIRI)

-- | It parses a data property expression
--
-- >>> parseTest dataPropertyExpression "<http://object-property-iri.com>"
-- FullIRI {_iriName = "http://object-property-iri.com"}
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
-- RestrictionDR {_restriction = DatatypeRestriction {_restrDatatype = Datatype {_unDatatype = AbbreviatedIRI {_prefixName = "xsd", _prefixValue = "integer"}}, _restrExpr = RestrictionExp {_restrFacet = L_FACET, _restrLiteral = IntegerLiteralC {_intLiteral = IntegerL {_ivalue = 0}}} :| []}}
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
-- OneOfDR {_literals = StringLiteralNoLang {_noLangLiteral = "kostas"} :| [IntegerLiteralC {_intLiteral = IntegerL {_ivalue = 32}},StringLiteralNoLang {_noLangLiteral = "true"}]}
--
literalList :: Parser DataRange
literalList = OneOfDR <$> nonEmptyList literal

-- | It parses datatype restrictions
--
-- >>> parseTest (datatypeRestriction *> eof) "integer[> 0, maxLength 2]"
-- ()
--
-- >>> parseTest datatypeRestriction "integer[< 0]"
-- RestrictionDR {...}
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
-- CExpClass {_classIRI = SimpleIRI {_simpleIRI = "Person"}}
--
-- >>> parseTest (conjunction *> eof) "owl:Thing that hasFirstName exactly 1"
-- ()
--
-- >>> parseTest (conjunction *> eof) "p some a and p only b"
-- ()
--
-- >>> parseTest (conjunction <* eof) "test:Class1 that not test:Class2"
-- CExpObjectIntersectionOf {_objIntrxExprs = (CExpClass {_classIRI = AbbreviatedIRI {_prefixName = "test", _prefixValue = "Class1"}},CExpObjectComplementOf {_objComplExpr = CExpClass {_classIRI = AbbreviatedIRI {_prefixName = "test", _prefixValue = "Class2"}}}) :# []}
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
-- CExpObjectComplementOf {_objComplExpr = CExpClass {_classIRI = SimpleIRI {_simpleIRI = "Person"}}}
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
-- CExpDataHasValue {_dataHasSubject = SimpleIRI {_simpleIRI = "hasFirstName"}, _dataHasValue = StringLiteralNoLang {_noLangLiteral = "John"}}
--
-- >>> parseTest (restriction <* eof) "hasFirstName exactly 1"
-- CExpObjectExactCardinality {_objExactNo = 1, _objExactSubject = OPE {_objectPropertyIRI = SimpleIRI {_simpleIRI = "hasFirstName"}}, _objExactObject = Nothing}
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
        try (CExpDataSomeValuesFrom (singleton d) <$> (symbol "some" *> dataPrimary))
    <|> try (CExpDataAllValuesFrom  (singleton d) <$> (symbol "only" *> dataPrimary))
    <|> try (CExpDataHasValue d       <$> (symbol "value"   *> literal))
    <|> try (CExpDataMinCardinality   <$> (symbol "min"     *> nonNegativeInteger) <*> pure d <*> optional dataPrimary)
    <|> try (CExpDataMaxCardinality   <$> (symbol "max"     *> nonNegativeInteger) <*> pure d <*> optional dataPrimary)
    <|> try (CExpDataExactCardinality <$> (symbol "exactly" *> nonNegativeInteger) <*> pure d <*> optional dataPrimary)

-- | It parses a class IRI or a list of individual IRIs
--
-- >>> parseTest atomic "<class.iri>"
-- CExpClass {_classIRI = FullIRI {_iriName = "class.iri"}}
--
-- >>> parseTest atomic "Person"
-- CExpClass {_classIRI = SimpleIRI {_simpleIRI = "Person"}}
--
-- >>> parseTest atomic "{ <class.iri#ind1>, <class.iri#ind2> }"
-- CExpObjectOneOf {_objOneOf = NamedIRI {_namedIRI = FullIRI {_iriName = "class.iri#ind1"}} :| [NamedIRI {_namedIRI = FullIRI {_iriName = "class.iri#ind2"}}]}
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
datatypeAxiom :: Parser [Axiom]
datatypeAxiom = do
  dtype <- symbol "Datatype:" *> datatype
  let dtDeclaration = mkAxiom $ DeclarationAxiom (EntityDatatype dtype)
  axms <- many . choice $ ($ dtype) <$> [annotDTA, equDTA]  --choices
  pure $ dtDeclaration:concat axms
 where
  annotDTA = annotationAxiom . NamedIRI . _unDatatype
  equDTA c = do
    _ <- symbol "EquivalentTo:"
    exps <- annotatedList dataRange
    pure $ spreadAnnotations DatatypeAxiomDefinition c exps

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
classAxioms :: Parser [Axiom]
classAxioms = do
  clsIRI <- symbol "Class:" *> classIRI
  let x = CExpClass clsIRI
      classDeclaration = mkAxiom $ DeclarationAxiom (EntityClass clsIRI)
  axms <- many . choice $ ($ x) <$> [annotCA, subCA, equCA,  disCA, dscCA, keyCA]  --choices
  pure $ classDeclaration:concat axms
 where
  annotCA (CExpClass cIRI) = annotationAxiom $ NamedIRI cIRI
  annotCA _ = pure []
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
  dscCA (CExpClass cIRI) = do
    _ <- symbol "DisjointUnionOf:"
    an <- annotationSection
    ds <- listOfAtLeast2 description
    pure [mkAxiomWithAnnotations an (ClassAxiomDisjointUnion cIRI ds)]
  dscCA _ = pure []
  keyCA c = do
    _ <- symbol "HasKey:"
    an <- annotationSection
    od <- NE.fromList <$> some ((ObjectPE <$> objectPropertyExpression) <|> (DataPE <$> dataPropertyExpression))
    pure [mkAxiomWithAnnotations an (ClassAxiomHasKey c od)]


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
--       , "  InverseOf: hasSpouse, inverse (hasSpouse)"
--       , "  SubPropertyChain: Annotations: rdfs:comment \"property chain\" hasChild o hasParent"
--       ]
-- :}
--
-- >>> parseTest (many objectPropertyAxioms *> eof) (T.unlines (input <> input))
-- ()
--
objectPropertyAxioms :: Parser [Axiom]
objectPropertyAxioms = do
  x <- symbol "ObjectProperty:" *> objectPropertyExpression
  axms <- many . choice $ ($ x) <$> [annotAxiom, domainAxiom, rangeAxiom, charAxiom, subAxiom, subChainAxiom, equAxiom, disAxiom, invAxiom]
  pure $ declarationIfNamed x <> concat axms
 where
  declarationIfNamed (OPE i) = [mkAxiom $ DeclarationAxiom (EntityObjectProperty i)]
  declarationIfNamed _ = []
  annotAxiom (OPE opIRI) = annotationAxiom $ NamedIRI opIRI
  annotAxiom _ = fail "It parses only IRIs"
  domainAxiom c = do
    _ <- symbol "Domain:"
    mAn <- annotatedList description
    pure $ spreadAnnotations ObjectPropAxiomDomain c mAn
  rangeAxiom c = do
    _ <- symbol "Range:"
    mAn <- annotatedList description
    pure $ spreadAnnotations ObjectPropAxiomRange c mAn
  charAxiom c = do
    _ <- symbol "Characteristics:"
    chars <- annotatedList objectPropertyCharacteristic
    pure $ spreadAnnotations ObjectPropAxiomCharacteristics c chars
  subAxiom c = do
    _ <- symbol "SubPropertyOf:"
    chars <- annotatedList objectPropertyExpression
    pure $ spreadAnnotations ObjectPropAxiomSubProperty c chars
  subChainAxiom c = do
    _ <- symbol "SubPropertyChain:"
    annots <- annotationSection
    hd   <- objectPropertyExpression 
    rest <- nonEmptyList (symbol "o" *> objectPropertyExpression)
    let chain = ObjectPropertyChain $ atLeast2List' hd rest
    pure [mkAxiomWithAnnotations annots (ObjectPropAxiomChainSubProperty chain c)]
  equAxiom c = do
    _ <- symbol "EquivalentTo:"
    exps <- annotatedList objectPropertyExpression
    pure $ spreadAnnotationsIfExist ObjectPropAxiomEquivalent c exps
  disAxiom c = do
    _ <- symbol "DisjointWith:"
    exps <- annotatedList objectPropertyExpression
    pure $ spreadAnnotationsIfExist ObjectPropAxiomDisjoint c exps
  invAxiom c = do
    _ <- symbol "InverseOf:"
    exps <- annotatedList objectPropertyExpression
    pure $ spreadAnnotations ObjectPropAxiomInverse c exps


noAnnotations :: AnnotatedList a -> Bool
noAnnotations = all (null . fst . _unAnnotated) 

removeAnnotations :: AnnotatedList a -> NonEmpty a
removeAnnotations = fmap (snd . _unAnnotated) 

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
dataPropertyAxioms :: Parser [Axiom]
dataPropertyAxioms = do
  dpIRI <- symbol "DataProperty:" *> dataPropertyIRI
  let dpDeclaration = mkAxiom $ DeclarationAxiom (EntityDataProperty dpIRI)
  axms <- many . choice $ ($ dpIRI) <$> [annotAxiom, domainAxiom, rangeAxiom, charAxiom, subAxiom, equAxiom, disAxiom] --choices
  pure $ dpDeclaration:concat axms
 where
  annotAxiom = annotationAxiom . NamedIRI
  domainAxiom c = do
    _ <- symbol "Domain:"
    exps <- annotatedList description
    pure $ spreadAnnotations DataPropAxiomDomain c exps
  rangeAxiom c = do
    _ <- symbol "Range:"
    drs <- annotatedList dataRange
    pure $ spreadAnnotations DataPropAxiomRange c drs
  charAxiom c = do
    _ <- symbol "Characteristics:"
    chars <- annotatedList dataPropertyCharacteristic
    pure $ spreadAnnotations DataPropAxiomCharacteristics c chars
  subAxiom c = do
    _ <- symbol "SubPropertyOf:"
    exps <- annotatedList dataPropertyExpression
    pure $ spreadAnnotations DataPropAxiomSubProperty c exps
  equAxiom c = do
    _ <- symbol "EquivalentTo:"
    exps <- annotatedList dataPropertyExpression
    pure $ spreadAnnotationsIfExist DataPropAxiomEquivalent c exps
  disAxiom c = do
    _ <- symbol "DisjointWith:"
    exps <- annotatedList dataPropertyExpression
    pure $ spreadAnnotationsIfExist DataPropAxiomDisjoint c exps

-- | A small utility function with specific functionallity, defined in order to avoid repetition
spreadAnnotationsIfExist :: (a -> NonEmpty a -> AxiomValue) -> a -> AnnotatedList a -> [Axiom]
spreadAnnotationsIfExist c e als = 
    if noAnnotations als
    then pure $ mkAxiom (c e (removeAnnotations als))
    else (\(Annotated (a, v)) -> mkAxiomWithAnnotations a (c e (singleton v))) <$> NE.toList als

spreadAnnotations :: (a1 -> a2 -> AxiomValue) -> a1 -> NonEmpty (Annotated a2) -> [Axiom]
spreadAnnotations c e als = (\(Annotated (a, v)) -> mkAxiomWithAnnotations a (c e v)) <$> NE.toList als

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
annotationPropertyAxioms :: Parser [Axiom]
annotationPropertyAxioms = do
  apIRI <- symbol "AnnotationProperty:" *> annotationPropertyIRI
  let apDeclaration = mkAxiom $ DeclarationAxiom (EntityAnnotationProperty apIRI)
  axms <- many . choice $ ($ apIRI) <$> [annotAxiom, domainAxiom, rangeAxiom, subAxiom] --choices
  pure $ apDeclaration:concat axms
 where
  annotAxiom = annotationAxiom . NamedIRI
  domainAxiom c = do
    _ <- symbol "Domain:"
    iris <- annotatedList iri
    pure $ spreadAnnotations AnnotationAxiomDomain c iris
  rangeAxiom c = do
    _ <- symbol "Range:"
    iris <- annotatedList iri
    pure $ spreadAnnotations AnnotationAxiomRange c iris
  subAxiom c = do
    _ <- symbol "SubPropertyOf:"
    exps <- annotatedList annotationPropertyIRI
    pure $ spreadAnnotations AnnotationAxiomSubProperty c exps

-- | Generic annotation axim parser
--
-- >>> :{
-- let input :: [Text]
--     input =
--       [
--         "Annotations:"
--       , "  rdfs:comment \"Annot comment\","
--       , "  owl:priorVersion _:genid2147483672"
--       ]
-- :}
--
-- >>> parseTest (annotationAxiom (NamedIRI (FullIRI "http://object-property-iri.com")) *> eof) (T.unlines input)
-- ()
annotationAxiom :: TotalIRI -> Parser [Axiom]
annotationAxiom i = do
    _ <- symbol "Annotations:"
    annots <- annotatedList mAnnotation
    pure $ spreadAnnotations AnnotationAxiomAssertion i annots

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
-- 11
--
-- >>> parseTest (length <$> assertionAxioms <* eof) (T.unlines input2)
-- 10
--
assertionAxioms :: Parser [Axiom]
assertionAxioms = do
  ind <- symbol "Individual:" *> individual
  axms <- many . choice $ ($ ind) <$> [annotAxiom, domainAxiom, rangeAxiom, sameAxiom, diffAxiom] --choices
  pure $ declarationIfNamed ind <> concat axms
 where
  declarationIfNamed (NamedIRI i) = [mkAxiom (DeclarationAxiom (EntityIndividual i))]
  declarationIfNamed _ = []
  annotAxiom = annotationAxiom
  domainAxiom c = do
    _ <- symbol "Types:"
    classes <- annotatedList description
    pure $ spreadAnnotations AssertionAxiomClass c classes
  rangeAxiom c = do
    _ <- symbol "Facts:"
    facts <- annotatedList fact
    pure $ mapper <$> NE.toList facts
     where
      mapper (Annotated (a, ObjectPropertyFact o i)) = Axiom a $ AssertionAxiomObjectProperty (OPE o) c i
      mapper (Annotated (a, NegativeObjectPropertyFact o i)) = Axiom a $ AssertionAxiomNegativeObjectProperty (OPE o) c i
      mapper (Annotated (a, DataPropertyFact d v)) = Axiom a $ AssertionAxiomDataProperty d c v
      mapper (Annotated (a, NegativeDataPropertyFact d v)) = Axiom a $ AssertionAxiomNegativeDataProperty d c v
  sameAxiom c = do
    _ <- symbol "SameAs:"
    exps <- annotatedList individual
    pure $ spreadAnnotationsIfExist AssertionAxiomSameIndividuals c exps
  diffAxiom c = do
    _ <- symbol "DifferentFrom:"
    exps <- annotatedList individual
    pure $ spreadAnnotationsIfExist AssertionAxiomDifferentIndividuals c exps

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
-- >>> parseTest ((length <$> many misc) <* eof) (T.unlines input)
-- 7
--
misc :: Parser Axiom
misc = choice [equClM, disjClM, equOPM, disjOPM, equDPM, disjDPM, sameIndM, diffIndM]  --choices
 where
  equClM = do
    _ <- symbol "EquivalentClasses:"
    annots <- annotationSection
    (x, xs) <- extract filterClassIRI <$> listOfAtLeast2 description
    pure $ Axiom annots $ ClassAxiomEquivalentClasses x xs
  disjClM = do
    _ <- symbol "DisjointClasses:"
    annots <- annotationSection
    (x, xs) <- extract filterClassIRI <$> listOfAtLeast2 description
    pure $ Axiom annots $ ClassAxiomDisjointClasses x xs
  equOPM = do
    _ <- symbol "EquivalentProperties:"
    annots <- annotationSection
    (x, xs) <- extract filterObjectPropIRI <$> listOfAtLeast2 objectPropertyExpression
    pure $ Axiom annots $ ObjectPropAxiomEquivalent x xs
  disjOPM = do
    _ <- symbol "DisjointProperties:"
    annots <- annotationSection
    (x, xs) <- extract filterObjectPropIRI <$> listOfAtLeast2 objectPropertyExpression
    pure $ Axiom annots $ ObjectPropAxiomDisjoint x xs
  equDPM = do
    _ <- symbol "EquivalentProperties:"
    annots <- annotationSection
    (x, xs) <- extract (const True) <$> listOfAtLeast2 dataPropertyExpression
    pure $ Axiom annots $ DataPropAxiomEquivalent x xs
  disjDPM = do
    _ <- symbol "DisjointProperties:"
    annots <- annotationSection
    (x, xs) <- extract (const True) <$> listOfAtLeast2 dataPropertyExpression
    pure $ Axiom annots $ DataPropAxiomDisjoint x xs
  sameIndM = do
    _ <- symbol "SameIndividual:"
    annots <- annotationSection
    (x, xs) <- extract filterNamedIRI <$> listOfAtLeast2 individual
    pure $ Axiom annots $ AssertionAxiomSameIndividuals x xs
  diffIndM = do
    _ <- symbol "DifferentIndividuals:"
    annots <- annotationSection
    (x, xs) <- extract filterNamedIRI <$> listOfAtLeast2 individual
    pure $ Axiom annots $ AssertionAxiomDifferentIndividuals x xs

predifinedPrefixes :: [PrefixDeclaration]
predifinedPrefixes =
  [ PrefixD "rdf"  (FullIRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  , PrefixD "rdfs" (FullIRI "http://www.w3.org/2000/01/rdf-schema#")
  , PrefixD "xsd"  (FullIRI "http://www.w3.org/2001/XMLSchema#")
  , PrefixD "owl"  (FullIRI "http://www.w3.org/2002/07/owl#")
  ]

-- | It parses non empty annotated lists
--
-- >>> parseTest (annotatedList description *> eof) "Man, Person"
-- ()
--
annotatedList :: Parser p -> Parser (AnnotatedList p)
annotatedList p =
  let annotatedElement = Annotated <$> ((,) <$> annotationSection <*> p)
  in  nonEmptyList annotatedElement


------------------
-- External API --
------------------

-- | It tries to parse an ontology in Manchester format
--
parseOntology :: Text -- ^ the text to parse
              -> Either Text OntologyDocument -- ^ the parse error message or the parse ontology doc 
parseOntology text = case parse ontologyDocument "(stdin)" text of
  Left bundle -> Left . T.pack . errorBundlePretty $ bundle
  Right doc   -> Right doc
 

-- | It tries to parse an ontology file in Manchester format
--
parseOntologyDoc :: FilePath                    -- ^ the file path
                 -> IO (Maybe OntologyDocument) -- ^ the parsed ontology doc or nothing if the parse fails
parseOntologyDoc file =
  putStrLn ("Parsing ontology document: '" <> file <> "'") >>
  T.readFile file >>= parseContent
  where
    parseContent content =
      case parseOntology content of
        Left errorMsg -> do
          putStrLn "Unable to parse file. Reason: "
          putStrLn . T.unpack $ errorMsg
          pure Nothing
        Right doc -> do
          putStrLn "File parsed succesfully"
          pure (Just doc)

-- | Exports the ontology document in Manchester format
--
exportOntologyDoc :: OntologyDocument -- ^ The ontology document
                  -> Text             -- ^ the output in Manchster format
exportOntologyDoc = undefined -- T.pack . show . MP.pretty


-- | Exports the ontology document in a file in Manchster format
--
exportOntologyDocToFile :: FilePath         -- ^ The file to save the ontology
                        -> OntologyDocument -- ^ The ontology document
                        -> IO ()
exportOntologyDocToFile f ontDoc = T.writeFile f (exportOntologyDoc ontDoc)

-- parseAndSave :: FilePath -> IO ()
-- parseAndSave f = do
--   (Just ontDoc) <- parseOntologyDoc f
--   writeFile "output.man.owl" (show . MP.pretty $ ontDoc)

