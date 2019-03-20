{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Functional.Parser where

import           Prelude                           hiding ( exponent )
import           Data.Functor                             ( ($>) )
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as M
import           Text.Megaparsec

import           Language.OWL2.Import                     ( Text )
import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types

import           Language.OWL2.Internal.Parser

-- DocTest setup
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either

--------------------------
-- Parser related types --
--------------------------

-- | It parses literals
--
-- >>> parseTest (literal *> eof) "\"32\"^^integer"
-- ...
-- unexpected '^'
-- ...
--
-- >>> parseTest (literal *> eof) "\"32\"^^xsd:integer"
-- ()
--
-- >>> parseTest (literal *> eof) "\"stringLiteralNoLanguage\""
-- ()
--
-- >>> parseTest (literal *> eof) "\"stringLiteralWithLang\"@en"
-- ()
--
literal :: Parser Literal
literal =  lexeme $ TypedLiteralC <$> try typedLiteral 
       <|> StringLiteralLang      <$> try stringLiteralWithLanguage
       <|> StringLiteralNoLang    <$> try stringLiteralNoLanguage

-- | It parses a typed literal
--
-- >>> parseTest typedLiteral "\"32\"^^integer"
-- ...
-- keyword "integer" cannot be an identifier
--
-- >>> parseTest (literal *> eof) "\"32\"^^xsd:integer"
-- ()
--
-- >>> parseTest (typedLiteral *> eof) "\"Jack\"^^xsd:string"
-- ()
--
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue
                      <*> (symbol "^^" *> datatype)

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyD <$> many prefixDeclaration <*> ontology

-- | It parses a prefix declaration
--
-- >>> parseTest (prefixDeclaration *> eof) "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)"
-- ()
--
-- >>> parseTest (prefixDeclaration *> eof) "Prefix(:=<http://www.w3.org/2002/07/owl#>)"
-- ()
--
prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = do
  _ <- symbol "Prefix"
  parens $ PrefixD <$> prefixName <*> (symbol "=" *> fullIRI)

ontology :: Parser Ontology
ontology = do
  _ <- symbol "Ontology"
  parens $ do
    ver <- optional $ OntologyVersionIRI <$> ontologyIRI <*> try (optional versionIRI) -- Maybe (iri, Maybe iri)
    imprs <- many directImport
    annots <- ontologyAnnotations
    axms <- axioms
    pure $ Ontology ver imprs annots axms

-- | It parses import ontology declarations
--
-- >>> parseTest (directImport *> eof) "Import(<http://www.w3.org/2002/07/owl#>)"
-- ()
--
directImport :: Parser ImportDeclaration
directImport = ImportD <$> (symbol "Import" *> parens iri)

ontologyAnnotations :: Parser [Annotated Annotation]
ontologyAnnotations = fannotations

axioms :: Parser [Axiom]
axioms = many axiom 

-- | It parses  declarations
--
-- >>> parseTest (declaration *> eof) "Declaration(Class(<http://www.uom.gr/ai/TestOntology.owl#Child>))"
-- ()
--
-- >>> parseTest (declaration *> eof) "Declaration( NamedIndividual( a:Peter ))"
-- ()
--
declaration :: Parser Declaration
declaration = do
  _ <- symbol "Declaration"
  parens $ Declaration <$> axiomAnnotations <*> entity

entity :: Parser Entity
entity =  EntityClass              <$> (symbol "Class"              *> parens clazz)
      <|> EntityDatatype           <$> (symbol "Datatype"           *> parens datatype)
      <|> EntityObjectProperty     <$> (symbol "ObjectProperty"     *> parens objectProperty)
      <|> EntityDataProperty       <$> (symbol "DataProperty"       *> parens dataProperty)
      <|> EntityAnnotationProperty <$> (symbol "AnnotationProperty" *> parens annotationProperty)
      <|> EntityIndividual         <$> (symbol "NamedIndividual"    *> parens namedIndividual)

axiomAnnotations :: Parser [Annotated Annotation]
axiomAnnotations = fannotations

fannotation :: Parser (Annotated Annotation)
fannotation = do
  _ <- symbol "Annotation"
  parens $  do
     annots <- fannotations
     a      <- annotation literal
     pure $ Annotated (annots, a)

fannotations :: Parser [Annotated Annotation]
fannotations = many fannotation

annotationAnnotations :: Parser [Annotated Annotation]
annotationAnnotations = fannotations

annotationAxiom :: Parser AnnotationPropertyAxiom
annotationAxiom =  subAnnotationPropertyOf
               <|> annotationPropertyDomain
               <|> annotationPropertyRange

-- | It parses annotation assertions
--
-- >> parseTest (annotationAssertion *> eof) "AnnotationAssertion(rdfs:comment test-ont:kostasAnnot \"comment\")"
-- ()
-- >> parseTest (annotationAssertion *> eof) "AnnotationAssertion(owl:priorVersion _:node1 _:node2)"
-- ()
--
-- TODO: doctest inactive
-- annotationAssertion :: Parser AnnotationPropertyFrame
-- annotationAssertion = do
--   symbol "AnnotationAssertion"
--   parens $ do
--     annots   <- axiomAnnotations
--     property <- annotationProperty
--     subject  <- annotationSubject
--     target   <- annotationValue literal
--     let annotList = NE.fromList [Annotated (annots, Annotation property target)]
--     pure $ AnnotationPropertyF property {-TODO: remove: subject-} [AnnotationAPE annotList]

annotationSubject :: Parser TotalIRI
annotationSubject = totalIRI

-- | It parses sub property annotation assertions
--
-- >>> parseTest (subAnnotationPropertyOf *> eof) "SubAnnotationPropertyOf(test-ont:kostasAnnot rdfs:comment)"
-- ()
--
subAnnotationPropertyOf :: Parser AnnotationPropertyAxiom
subAnnotationPropertyOf = do
  _ <- symbol "SubAnnotationPropertyOf"
  parens $ AnnotationPSubProperty
        <$> axiomAnnotations
        <*> subAnnotationProperty
        <*> superAnnotationProperty

subAnnotationProperty :: Parser AnnotationPropertyIRI
subAnnotationProperty = annotationProperty

superAnnotationProperty :: Parser AnnotationPropertyIRI
superAnnotationProperty = annotationProperty

annotationPropertyDomain :: Parser AnnotationPropertyAxiom
annotationPropertyDomain = do
  _ <- symbol "AnnotationPropertyDomain"
  parens $ AnnotationPDomain
        <$> axiomAnnotations
        <*> annotationProperty
        <*> iri

annotationPropertyRange :: Parser AnnotationPropertyAxiom
annotationPropertyRange = do
  _ <- symbol "AnnotationPropertyRange"
  parens $ AnnotationPRange
        <$> axiomAnnotations
        <*> annotationProperty
        <*> iri

datatype:: Parser Datatype
datatype = Datatype <$> iri

-- | It parses an object property expression
--
-- >>> parseTest objectPropertyExpression "<http://object-property-iri.com>"
-- OPE (FullIRI "http://object-property-iri.com")
--
-- >>> parseTest objectPropertyExpression "ObjectInverseOf(<http://object-property-iri.com>)"
-- InverseOPE (FullIRI "http://object-property-iri.com")
--
objectPropertyExpression :: Parser ObjectPropertyExpression
objectPropertyExpression =  OPE        <$> objectProperty
                        <|> InverseOPE <$> (symbol "ObjectInverseOf" *> parens objectProperty)

dataPropertyExpression :: Parser DataPropertyIRI
dataPropertyExpression = dataProperty

dataRange :: Parser DataRange
dataRange =  DatatypeDR <$> datatype
         <|> dataIntersectionOf
         <|> dataUnionOf
         <|> dataComplementOf
         <|> dataOneOf
         <|> datatypeRestriction

dataIntersectionOf :: Parser DataRange
dataIntersectionOf = do
  _ <- symbol "DataIntersectionOf"
  elems <- parens $ atLeast2List' <$> dataRange <*> (NE.fromList <$> some dataRange)
  pure $ IntersectionDR elems

dataUnionOf :: Parser DataRange
dataUnionOf = do
  _ <- symbol "DataUnionOf"
  elems <- parens $ atLeast2List' <$> dataRange <*> (NE.fromList <$> some dataRange)
  pure $ UnionDR elems

dataComplementOf :: Parser DataRange
dataComplementOf = do
  _ <- symbol "DataComplementOf"
  parens $ ComplementDR <$> dataRange

dataOneOf :: Parser DataRange
dataOneOf = do
  _ <- symbol "DataOneOf"
  elm <- parens $ NE.fromList <$> some literal
  pure $ OneOfDR elm

datatypeRestriction :: Parser DataRange
datatypeRestriction = do
  _ <- symbol "DatatypeRestriction"
  parens $ do
    dt <- datatype
    lst <- some $ RestrictionExp <$> constrainingFacet <*> restrictionValue
    pure . RestrictionDR $ DatatypeRestriction dt (NE.fromList lst)

constrainingFacet :: Parser Facet
constrainingFacet
    =  symbol "xsd:length"       $> LENGTH_FACET
   <|> symbol "xsd:maxLength"    $> MAX_LENGTH_FACET
   <|> symbol "xsd:minLength"    $> MIN_LENGTH_FACET
   <|> symbol "xsd:pattern"      $> PATTERN_FACET
   <|> symbol "xsd:langRange"    $> LANG_RANGE_FACET
   <|> symbol "xsd:maxInclusive" $> LE_FACET
   <|> symbol "xsd:maxExclusive" $> L_FACET
   <|> symbol "xsd:minInclusive" $> GE_FACET
   <|> symbol "xsd:minExclusive" $> G_FACET

restrictionValue :: Parser Literal
restrictionValue = literal

classExpression :: Parser ClassExpression
classExpression = lexeme alternatives
 where
  alternatives =   objectOneOf
               <|> objectIntersectionOf
               <|> objectUnionOf
               <|> objectComplementOf     
               <|> objectSomeValuesFrom   
               <|> objectAllValuesFrom    
               <|> objectHasValue         
               <|> objectHasSelf          
               <|> objectMinCardinality   
               <|> objectMaxCardinality   
               <|> objectExactCardinality 
               <|> dataSomeValuesFrom     
               <|> dataAllValuesFrom      
               <|> dataHasValue           
               <|> dataMinCardinality     
               <|> dataMaxCardinality     
               <|> dataExactCardinality   
               <|> CExpClass <$> clazz 


objectIntersectionOf :: Parser ClassExpression
objectIntersectionOf = do
  _ <- symbol "ObjectIntersectionOf"
  parens $ CExpObjectIntersectionOf <$> doubleOrMany "" classExpression

objectUnionOf :: Parser ClassExpression
objectUnionOf = do
  _ <- symbol "ObjectUnionOf"
  parens $ CExpObjectUnionOf <$> doubleOrMany "" classExpression

objectComplementOf :: Parser ClassExpression
objectComplementOf = do
  _ <- symbol "ObjectComplementOf"
  parens $ CExpObjectComplementOf <$> classExpression

objectOneOf :: Parser ClassExpression
objectOneOf = do
  _ <- symbol "ObjectOneOf"
  parens $ CExpObjectOneOf <$> singleOrMany "" individual

objectSomeValuesFrom :: Parser ClassExpression
objectSomeValuesFrom = do
  _ <- symbol "ObjectSomeValuesFrom"
  parens $ CExpObjectSomeValuesFrom <$> objectPropertyExpression <*> classExpression

objectAllValuesFrom :: Parser ClassExpression
objectAllValuesFrom = do
  _ <- symbol "ObjectAllValuesFrom"
  parens $ CExpObjectAllValuesFrom <$> objectPropertyExpression <*> classExpression
    
objectHasValue :: Parser ClassExpression
objectHasValue = do
  _ <- symbol "ObjectHasValue"
  parens $ CExpObjectHasValue <$> objectPropertyExpression <*> individual

objectHasSelf :: Parser ClassExpression
objectHasSelf = do
  _ <- symbol "ObjectHasSelf"
  parens $ CExpObjectHasSelf <$> objectPropertyExpression

objectCardinality
  :: Text
  -> (Int -> ObjectPropertyExpression -> Maybe ClassExpression -> ClassExpression)
  -> Parser ClassExpression
objectCardinality l c = do
  _ <- symbol l
  parens $ c <$> nonNegativeInteger <*> objectPropertyExpression <*> optional classExpression

objectMinCardinality :: Parser ClassExpression
objectMinCardinality = objectCardinality "ObjectMinCardinality" CExpObjectMinCardinality

objectMaxCardinality :: Parser ClassExpression
objectMaxCardinality = objectCardinality "ObjectMaxCardinality" CExpObjectMaxCardinality

objectExactCardinality :: Parser ClassExpression
objectExactCardinality = objectCardinality "ObjectExactCardinality" CExpObjectMinCardinality

-- TODO: Protege does not seem to support multiple data property expressions in a single "some"
-- e.g. Was not possible to parse: "EquivalentClasses(test-ont:Test1 DataSomeValuesFrom(test-ont:dataProp1 test-ont:dataPro2 xsd:integer)) 
dataSomeValuesFrom :: Parser ClassExpression
dataSomeValuesFrom = do
  _ <- symbol "DataSomeValuesFrom"
  parens $ CExpDataSomeValuesFrom <$> dataPropertyExpression <*> dataRange

-- TODO: Protege does not seem to support multiple data property expressions in a single "some"
-- e.g. Was not possible to parse: "EquivalentClasses(test-ont:Test1 DataAllValuesFrom(test-ont:dataProp1 test-ont:dataPro2 xsd:integer)) 
dataAllValuesFrom :: Parser ClassExpression
dataAllValuesFrom = do
  _ <- symbol "DataAllValuesFrom"
  parens $ CExpDataAllValuesFrom <$> dataPropertyExpression <*> dataRange

dataHasValue :: Parser ClassExpression
dataHasValue = do
  _ <- symbol "DataHasValue"
  parens $ CExpDataHasValue <$> dataPropertyExpression <*> literal

dataCardinality
  :: Text
  -> (Int -> DataPropertyExpression -> Maybe DataRange -> ClassExpression)
  -> Parser ClassExpression
dataCardinality l c = do
  _ <- symbol l
  parens $ c <$> nonNegativeInteger <*> dataPropertyExpression <*> optional dataRange

dataMinCardinality :: Parser ClassExpression
dataMinCardinality = dataCardinality "DataMinCardinality" CExpDataMinCardinality

dataMaxCardinality :: Parser ClassExpression
dataMaxCardinality = dataCardinality "DataMaxCardinality" CExpDataMaxCardinality

dataExactCardinality :: Parser ClassExpression
dataExactCardinality = dataCardinality "DataExactCardinality" CExpDataExactCardinality

axiom :: Parser Axiom
axiom =  undefined
--declaration $> ()
--     <|> classAxiom $> ()
--     <|> objectPropertyAxiom $> ()
--     <|> dataPropertyAxiom $> ()
--     <|> datatypeDefinition $> ()
--     <|> hasKey $> ()
--     <|> assertion $> ()
--     <|> annotationAxiom $> ()

------------------
-- Class Axioms --
------------------
classAxiom :: Parser ClassAxiom
classAxiom =  subClassOf
          <|> equivalentClasses
          <|> disjointClasses
          <|> disjointUnion
          <|> hasKey

subClassOf :: Parser ClassAxiom
subClassOf = do
  _ <- symbol "SubClassOf"
  parens $ ClassAxiomSubClassOf
        <$> axiomAnnotations
        <*> subClassExpression
        <*> superClassExpression

subClassExpression :: Parser ClassExpression
subClassExpression = classExpression

superClassExpression :: Parser ClassExpression
superClassExpression = classExpression

equivalentClasses :: Parser ClassAxiom
equivalentClasses = do
  _ <- symbol "EquivalentClasses"
  parens $ ClassAxiomEquivalentClasses
        <$> axiomAnnotations
        <*> doubleOrMany "" classExpression

disjointClasses :: Parser ClassAxiom
disjointClasses = do
  _ <- symbol "DisjointClasses"
  parens $ ClassAxiomDisjointClasses
        <$> axiomAnnotations
        <*> doubleOrMany "" classExpression

disjointUnion :: Parser ClassAxiom
disjointUnion = do
  _ <- symbol "DisjointUnion"
  parens $ ClassAxiomDisjointUnion
        <$> axiomAnnotations
        <*> clazz
        <*> doubleOrMany "" classExpression

hasKey :: Parser ClassAxiom
hasKey = do
  _ <- symbol "HasKey"
  parens $ ClassAxiomHasKey
        <$> axiomAnnotations
        <*> classExpression
        <*> (NE.fromList <$> some ((ObjectPE <$> objectPropertyExpression) <|> (DataPE <$> dataPropertyExpression)))

----------------------------
-- Object Property Axioms --
----------------------------
objectPropertyAxiom :: Parser ObjectPropertyAxiom
objectPropertyAxiom =  subObjectPropertyOf
                   <|> equivalentObjectProperties
                   <|> disjointObjectProperties
                   <|> inverseObjectProperties
                   <|> objectPropertyDomain
                   <|> objectPropertyRange
                   <|> objectPropertyCharactersistics

-- | Parses Object SubProperty axioms
--
-- >>> parseTest (subObjectPropertyOf *> eof) "SubObjectPropertyOf( ObjectPropertyChain( a:hasFather a:hasBrother ) a:hasUncle )"
-- ()
-- >>> parseTest (subObjectPropertyOf *> eof) "SubObjectPropertyOf( a:hasUncle a:hasRelative )"
-- ()
--
subObjectPropertyOf :: Parser ObjectPropertyAxiom
subObjectPropertyOf = do
  _ <- symbol "SubObjectPropertyOf"
  parens $ do
    annots <- axiomAnnotations
    sub <- subObjectPropertyExpression
    sup <- superObjectPropertyExpression
    pure $ case sub of
      Left s  -> ObjectPSubProperty annots s sup
      Right x -> ObjectPChainSubProperty annots x sup

-- | Parses either an object expression or an object property chain
--
-- >>> parseTest (isRight <$> subObjectPropertyExpression <* eof) "ObjectPropertyChain( a:hasFather a:hasBrother )"
-- True
-- >>> parseTest (isLeft <$> subObjectPropertyExpression <* eof) "a:hasUncle"
-- True
--
subObjectPropertyExpression :: Parser (Either ObjectPropertyExpression ObjectPropertyChain)
subObjectPropertyExpression =  Left  <$> objectPropertyExpression
                           <|> Right <$> propertyExpressionChain

propertyExpressionChain :: Parser ObjectPropertyChain
propertyExpressionChain = do
  _ <- symbol "ObjectPropertyChain"
  parens $ ObjectPropertyChain <$> doubleOrMany "" objectPropertyExpression

superObjectPropertyExpression :: Parser ObjectPropertyExpression
superObjectPropertyExpression = objectPropertyExpression

equivalentObjectProperties :: Parser ObjectPropertyAxiom
equivalentObjectProperties = do
  _ <- symbol "EquivalentObjectProperties"
  parens $ ObjectPEquivalent <$> axiomAnnotations <*> doubleOrMany "" objectPropertyExpression

disjointObjectProperties :: Parser ObjectPropertyAxiom
disjointObjectProperties = do
  _ <- symbol "DisjointObjectProperties"
  parens $ ObjectPDisjoint <$> axiomAnnotations <*> doubleOrMany "" objectPropertyExpression

objectPropertyDomain :: Parser ObjectPropertyAxiom
objectPropertyDomain = do
  _ <- symbol "ObjectPropertyDomain"
  parens $ ObjectPDomain <$> axiomAnnotations <*> objectPropertyExpression <*> classExpression

objectPropertyRange :: Parser ObjectPropertyAxiom
objectPropertyRange = do
  _ <- symbol "ObjectPropertyRange"
  parens $ ObjectPRange <$> axiomAnnotations <*> objectPropertyExpression <*> classExpression

inverseObjectProperties :: Parser ObjectPropertyAxiom
inverseObjectProperties = do
  _ <- symbol "InverseObjectProperties"
  parens $ ObjectPInverse <$> axiomAnnotations <*> objectPropertyExpression <*> objectPropertyExpression

objectPropertyCharactersistics :: Parser ObjectPropertyAxiom
objectPropertyCharactersistics = do
  chc <- choice . fmap symbol $ M.keys objectPropertyCharacteristics
  parens $ do
    annots <- axiomAnnotations
    ope    <- objectPropertyExpression
    pure $ ObjectPCharacteristics annots ope (objectPropertyCharacteristics M.! chc)

objectPropertyCharacteristics :: Map Text ObjectPropertyCharacteristic
objectPropertyCharacteristics = M.fromList
  [ ("FunctionalObjectProperty"       , FUNCTIONAL)
  , ("InverseFunctionalObjectProperty", INVERSE_FUNCTIONAL)
  , ("ReflexiveObjectProperty"        , REFLEXIVE)
  , ("IrreflexiveObjectProperty"      , IRREFLEXIVE)
  , ("SymmetricObjectProperty"        , SYMMETRIC)
  , ("AsymmetricObjectProperty"       , ASYMMETRIC)
  , ("TransitiveObjectProperty"       , TRANSITIVE)
  ]

--------------------------
-- Data Property Axioms --
--------------------------
dataPropertyAxiom :: Parser DataPropertyAxiom
dataPropertyAxiom =  subDataPropertyOf
                 <|> equivalentDataProperties
                 <|> disjointDataProperties
                 <|> dataPropertyDomain
                 <|> dataPropertyRange
                 <|> functionalDataProperty

subDataPropertyOf :: Parser DataPropertyAxiom
subDataPropertyOf = do
  _ <- symbol "SubDataPropertyOf"
  parens $ DataPSubProperty <$> axiomAnnotations <*> subDataPropertyExpression <*> superDataPropertyExpression

subDataPropertyExpression :: Parser DataPropertyIRI
subDataPropertyExpression = dataPropertyExpression

superDataPropertyExpression :: Parser DataPropertyIRI
superDataPropertyExpression = dataPropertyExpression

equivalentDataProperties :: Parser DataPropertyAxiom
equivalentDataProperties = do
  _ <- symbol "EquivalentDataProperties"
  parens $ DataPEquivalent <$> axiomAnnotations <*> doubleOrMany "" dataPropertyExpression

disjointDataProperties :: Parser DataPropertyAxiom
disjointDataProperties = do
  _ <- symbol "DisjointDataProperties"
  parens $ DataPDisjoint <$> axiomAnnotations <*> doubleOrMany "" dataPropertyExpression

dataPropertyDomain :: Parser DataPropertyAxiom
dataPropertyDomain = do
  _ <- symbol "DataPropertyDomain"
  parens $ DataPDomain <$> axiomAnnotations <*> dataPropertyExpression <*> classExpression

dataPropertyRange :: Parser DataPropertyAxiom
dataPropertyRange = do
  _ <- symbol "DataPropertyRange"
  parens $ DataPRange <$> axiomAnnotations <*> dataPropertyExpression <*> dataRange

functionalDataProperty :: Parser DataPropertyAxiom
functionalDataProperty = do
  _ <- symbol "FunctionalDataProperty"
  parens $ DataPCharacteristics <$> axiomAnnotations <*> dataPropertyExpression <*> pure FUNCTIONAL_DPE

datatypeDefinition :: Parser DatatypeAxiom
datatypeDefinition = do
  _ <- symbol "DatatypeDefinition"
  parens $ DatatypeAxiomEquivalent <$> axiomAnnotations <*> datatype <*> dataRange

assertion :: Parser AssertionAxiom
assertion =  sameIndividual
         <|> differentIndividuals
         <|> classAssertion
         <|> objectPropertyAssertion
         <|> negativeObjectPropertyAssertion
         <|> dataPropertyAssertion
         <|> negativeDataPropertyAssertion

sourceIndividual :: Parser Individual
sourceIndividual = individual

targetIndividual :: Parser Individual
targetIndividual = individual

targetValue :: Parser Literal
targetValue = literal

sameIndividual :: Parser AssertionAxiom
sameIndividual = do
  _ <- symbol "SameIndividual"
  parens $ AssertionSameIndividuals <$> axiomAnnotations <*> doubleOrMany "" individual

differentIndividuals :: Parser AssertionAxiom
differentIndividuals = do
  _ <- symbol "DifferentIndividuals"
  parens $ AssertionDifferentIndividuals <$> axiomAnnotations <*> doubleOrMany "" individual

classAssertion :: Parser AssertionAxiom
classAssertion = do
  _ <- symbol "ClassAssertion"
  parens $ do
    annots <- axiomAnnotations
    ce     <- classExpression
    ind    <- individual
    pure $ AssertionClass annots ind ce

objectPropertyAssertion :: Parser AssertionAxiom
objectPropertyAssertion = do
  _ <- symbol "ObjectPropertyAssertion"
  parens $ AssertionObjectProperty
        <$> axiomAnnotations
        <*> objectPropertyExpression
        <*> sourceIndividual
        <*> targetIndividual

negativeObjectPropertyAssertion :: Parser AssertionAxiom
negativeObjectPropertyAssertion = do
  _ <- symbol "NegativeObjectPropertyAssertion"
  parens $ AssertionNegativeObjectProperty
        <$> axiomAnnotations
        <*> objectPropertyExpression
        <*> sourceIndividual
        <*> targetIndividual

dataPropertyAssertion :: Parser AssertionAxiom
dataPropertyAssertion = do
  _ <- symbol "DataPropertyAssertion"
  parens $ AssertionDataProperty
        <$> axiomAnnotations
        <*> dataPropertyExpression
        <*> sourceIndividual
        <*> targetValue

negativeDataPropertyAssertion :: Parser AssertionAxiom
negativeDataPropertyAssertion = do
  _ <- symbol "NegativeDataPropertyAssertion"
  parens $ AssertionNegativeDataProperty
        <$> axiomAnnotations
        <*> dataPropertyExpression
        <*> sourceIndividual
        <*> targetValue

------------------
-- Parser utils --
------------------
parseOntologyDoc :: FilePath -> IO (Maybe ())
parseOntologyDoc file =
  putStrLn ("Parsing ontology document: '" <> file <> "'") >>
  T.readFile file >>= parseContent
  where
    parseContent content =
      case parse (ontologyDocument <* eof) file content of
        Left bundle -> do
          putStrLn "Unable to parse file. Reason: "
          putStrLn (errorBundlePretty bundle)
          pure Nothing
        Right doc -> do
          putStrLn "File parsed succesfully"
          pure (Just ())

