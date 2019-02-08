{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Functional.Parser where

import           Prelude                           hiding ( exponent )
import           Data.Functor                             ( ($>) )
import           Text.Megaparsec

import           Language.OWL2.Import                     ( Text )
import           Language.OWL2.Types

import           Language.OWL2.Manchester.Parser          ( Parser
                                                          , symbol
                                                          , parens
                                                          , prefixName
                                                          , fullIRI
                                                          , ontologyIRI
                                                          , versionIRI
                                                          , iri
                                                          , literal
                                                          , nodeID
                                                          , nonNegativeInteger
                                                          )
-- DocTest setup
--
-- $setup
-- >>> :set -XOverloadedStrings

--------------------------
-- Parser related types --
--------------------------

ontologyDocument :: Parser ()
ontologyDocument = do
  many prefixDeclaration
  ontology
  pure ()

-- | It parses a prefix declaration
--
-- >>> parseTest (prefixDeclaration *> eof) "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)"
-- ()
--
-- >>> parseTest (prefixDeclaration *> eof) "Prefix(:=<http://www.w3.org/2002/07/owl#>)"
-- ()
--
prefixDeclaration :: Parser ()
prefixDeclaration = do
  symbol "Prefix"
  parens $ do
    prefixName
    symbol "="
    fullIRI
  pure ()

ontology :: Parser ()
ontology = do
  symbol "Ontology"
  parens $ do
    optional $ do
      ontologyIRI
      optional versionIRI -- Maybe (iri, Maybe iri)
    many directImport
    ontologyAnnotations
    axioms
  pure ()


-- | It parses import ontology declarations
--
-- >>> parseTest (directImport *> eof) "Import(<http://www.w3.org/2002/07/owl#>)"
-- ()
--
directImport :: Parser ()
directImport = do
  symbol "Import"
  parens iri
  pure ()

ontologyAnnotations :: Parser [()]
ontologyAnnotations = many annotation

axioms :: Parser [()]
axioms = many axiom

declaration :: Parser ()
declaration = do
  symbol "Declaration"
  parens $ do
    axiomAnnotations
    entity
  pure ()

entity :: Parser ()
entity =  symbol "Class" *> parens clazz $> ()
      <|> symbol "Datatype" *> parens datatype $> ()
      <|> symbol "ObjectProperty" *> parens objectProperty $> ()
      <|> symbol "DataProperty" *> parens dataProperty $> ()
      <|> symbol "AnnotationProperty" *> parens annotationProperty $> ()
      <|> symbol "NamedIndividual" *> parens namedIndividual $> ()

annotationSubject :: Parser ()
annotationSubject =  iri $> ()
                 <|> anonymousIndividual $> ()

annotationValue :: Parser ()
annotationValue =  anonymousIndividual $> ()
               <|> iri $> ()
               <|> literal $> ()

axiomAnnotations :: Parser [()]
axiomAnnotations = many annotation

annotation :: Parser ()
annotation = do
  symbol "Annotation"
  parens $ do
    annotationAnnotations
    annotationProperty
    annotationValue
  pure ()

annotationAnnotations :: Parser [()]
annotationAnnotations = many annotation

annotationAxiom :: Parser ()
annotationAxiom =  annotationAssertion $> ()
               <|> subAnnotationPropertyOf $> ()
               <|> annotationPropertyDomain $> ()
               <|> annotationPropertyRange $> ()

annotationAssertion :: Parser ()
annotationAssertion = do
  symbol "AnnotationAssertion"
  parens $ do
    axiomAnnotations
    annotationProperty
    annotationSubject
    annotationValue
  pure ()

subAnnotationPropertyOf :: Parser ()
subAnnotationPropertyOf = do
  symbol "SubAnnotationPropertyOf"
  parens $ do
    axiomAnnotations
    subAnnotationProperty
    superAnnotationProperty
  pure ()

subAnnotationProperty :: Parser IRI
subAnnotationProperty = annotationProperty

superAnnotationProperty :: Parser IRI
superAnnotationProperty = annotationProperty

annotationPropertyDomain :: Parser ()
annotationPropertyDomain = do
  symbol "AnnotationPropertyDomain"
  parens $ do
    axiomAnnotations
    annotationProperty
    iri
  pure ()

annotationPropertyRange :: Parser ()
annotationPropertyRange = do
  symbol "AnnotationPropertyRange"
  parens $ do
    axiomAnnotations
    annotationProperty
    iri
  pure ()

clazz :: Parser IRI
clazz = iri

datatype:: Parser IRI
datatype = iri

objectProperty :: Parser IRI
objectProperty = iri

dataProperty :: Parser IRI
dataProperty = iri

annotationProperty :: Parser IRI
annotationProperty = iri

individual :: Parser ()
individual =  namedIndividual $> ()
          <|> anonymousIndividual $> ()

namedIndividual :: Parser IRI
namedIndividual = iri

anonymousIndividual :: Parser NodeID
anonymousIndividual = nodeID

objectPropertyExpression :: Parser ()
objectPropertyExpression =  objectProperty $> ()
                        <|> inverseObjectProperty $> ()

inverseObjectProperty :: Parser ()
inverseObjectProperty = do
  symbol "ObjectInverseOf"
  parens objectProperty
  pure ()

dataPropertyExpression :: Parser IRI
dataPropertyExpression = dataProperty

dataRange :: Parser ()
dataRange =  datatype $> ()
         <|> dataIntersectionOf $> ()
         <|> dataUnionOf $> ()
         <|> dataComplementOf $> ()
         <|> dataOneOf $> ()
         <|> datatypeRestriction $> ()

dataIntersectionOf :: Parser ()
dataIntersectionOf = do
  symbol "DataIntersectionOf"
  parens $ do
    dataRange
    some dataRange
  pure ()

dataUnionOf :: Parser ()
dataUnionOf = do
  symbol "DataUnionOf"
  parens $ do
    dataRange
    some dataRange
  pure ()

dataComplementOf :: Parser ()
dataComplementOf = do
  symbol "DataComplementOf"
  parens dataRange
  pure ()

dataOneOf :: Parser ()
dataOneOf = do
  symbol "DataOneOf"
  parens $ some literal
  pure ()

datatypeRestriction :: Parser ()
datatypeRestriction = do
  symbol "DatatypeRestriction"
  parens $ do
    datatype
    constrainingFacet
    restrictionValue
    many $ do 
      constrainingFacet
      restrictionValue
  pure ()

constrainingFacet :: Parser IRI
constrainingFacet = iri

restrictionValue :: Parser Literal
restrictionValue = literal

classExpression :: Parser ()
classExpression =  clazz $> ()
               <|> objectIntersectionOf $> ()
               <|> objectUnionOf $> ()
               <|> objectComplementOf $> ()
               <|> objectOneOf $> ()
               <|> objectSomeValuesFrom $> ()
               <|> objectAllValuesFrom $> ()
               <|> objectHasValue $> ()
               <|> objectHasSelf $> ()
               <|> objectMinCardinality $> ()
               <|> objectMaxCardinality $> ()
               <|> objectExactCardinality $> ()
               <|> dataSomeValuesFrom $> ()
               <|> dataAllValuesFrom $> ()
               <|> dataHasValue $> ()
               <|> dataMinCardinality $> ()
               <|> dataMaxCardinality $> ()
               <|> dataExactCardinality$> ()

objectIntersectionOf :: Parser ()
objectIntersectionOf = do
  symbol "ObjectIntersectionOf"
  parens $ do
    classExpression
    some classExpression
  pure ()

objectUnionOf :: Parser ()
objectUnionOf = do
  symbol "ObjectUnionOf"
  parens $ do
    classExpression
    some classExpression
  pure ()

objectComplementOf :: Parser ()
objectComplementOf = do
  symbol "ObjectComplementOf"
  parens classExpression
  pure ()

objectOneOf :: Parser ()
objectOneOf = do
  symbol "ObjectOneOf"
  some individual
  pure ()

objectSomeValuesFrom :: Parser ()
objectSomeValuesFrom = do
  symbol "ObjectSomeValuesFrom"
  parens $ do
    objectPropertyExpression
    classExpression
  pure ()

objectAllValuesFrom :: Parser ()
objectAllValuesFrom = do
  symbol "ObjectAllValuesFrom"
  parens $ do
    objectPropertyExpression
    classExpression
  pure ()
    
objectHasValue :: Parser ()
objectHasValue = do
  symbol "ObjectHasValue"
  parens $ do
    objectPropertyExpression
    individual
  pure ()

objectHasSelf :: Parser ()
objectHasSelf = do
  symbol "ObjectHasSelf"
  parens objectPropertyExpression
  pure ()

objectCardinality :: Text -> Parser ()
objectCardinality l = do
  symbol l
  parens $ do
    nonNegativeInteger
    objectPropertyExpression
    optional classExpression
  pure ()

objectMinCardinality :: Parser ()
objectMinCardinality = objectCardinality "ObjectMinCardinality"

objectMaxCardinality :: Parser ()
objectMaxCardinality = objectCardinality "ObjectMaxCardinality"

objectExactCardinality :: Parser ()
objectExactCardinality = objectCardinality "ObjectExactCardinality"

dataSomeValuesFrom :: Parser ()
dataSomeValuesFrom = do
  symbol "DataSomeValuesFrom"
  parens $ do
    some dataPropertyExpression
    dataRange
  pure ()

dataAllValuesFrom :: Parser ()
dataAllValuesFrom = do
  symbol "DataAllValuesFrom"
  parens $ do
    some dataPropertyExpression
    dataRange
  pure ()

dataHasValue :: Parser ()
dataHasValue = do
  symbol "DataHasValue"
  parens $ do
    dataPropertyExpression
    literal
  pure ()

dataCardinality :: Text -> Parser ()
dataCardinality l = do
  symbol l
  parens $ do
    nonNegativeInteger
    dataPropertyExpression
    optional dataRange
  pure ()

dataMinCardinality :: Parser ()
dataMinCardinality = dataCardinality "DataMinCardinality"

dataMaxCardinality :: Parser ()
dataMaxCardinality = dataCardinality "DataMaxCardinality"

dataExactCardinality :: Parser ()
dataExactCardinality = dataCardinality "DataExactCardinality"

axiom :: Parser ()
axiom =  declaration $> ()
     <|> classAxiom $> ()
     <|> objectPropertyAxiom $> ()
     <|> dataPropertyAxiom $> ()
     <|> datatypeDefinition $> ()
     <|> hasKey $> ()
     <|> assertion $> ()
     <|> annotationAxiom $> ()

classAxiom :: Parser ()
classAxiom =  subClassOf $> ()
          <|> equivalentClasses $> ()
          <|> disjointClasses $> ()
          <|> disjointUnion $> ()

subClassOf :: Parser ()
subClassOf = do
  symbol "SubClassOf"
  parens $ do
    axiomAnnotations
    subClassExpression
    superClassExpression
  pure ()

subClassExpression :: Parser ()
subClassExpression = classExpression

superClassExpression :: Parser ()
superClassExpression = classExpression

equivalentClasses :: Parser ()
equivalentClasses = do
  symbol "EquivalentClasses"
  parens $ do
    axiomAnnotations
    classExpression
    some classExpression
  pure ()

disjointClasses :: Parser ()
disjointClasses = do
  symbol "DisjointClasses"
  parens $ do
    axiomAnnotations
    classExpression
    some classExpression
  pure ()

disjointUnion :: Parser ()
disjointUnion = do
  symbol "DisjointUnion"
  parens $ do
    axiomAnnotations
    clazz
    disjointClassExpressions
  pure ()

disjointClassExpressions :: Parser ()
disjointClassExpressions = do
  classExpression
  some classExpression
  pure ()

objectPropertyAxiom :: Parser ()
objectPropertyAxiom =  subObjectPropertyOf $> ()
                   <|> equivalentObjectProperties $> ()
                   <|> disjointObjectProperties $> ()
                   <|> inverseObjectProperties $> ()
                   <|> objectPropertyDomain $> ()
                   <|> objectPropertyRange $> ()
                   <|> functionalObjectProperty $> ()
                   <|> inverseFunctionalObjectProperty $> ()
                   <|> reflexiveObjectProperty $> ()
                   <|> irreflexiveObjectProperty $> ()
                   <|> symmetricObjectProperty $> ()
                   <|> asymmetricObjectProperty $> ()
                   <|> transitiveObjectProperty $> ()

subObjectPropertyOf :: Parser ()
subObjectPropertyOf = do
  symbol "SubObjectPropertyOf"
  parens $ do
    axiomAnnotations
    subObjectPropertyExpression
    superObjectPropertyExpression
  pure ()

subObjectPropertyExpression :: Parser ()
subObjectPropertyExpression =  objectPropertyExpression $> ()
                           <|> propertyExpressionChain $> ()

propertyExpressionChain :: Parser ()
propertyExpressionChain = do
  symbol "ObjectPropertyChain"
  parens $ do
    objectPropertyExpression
    some objectPropertyExpression
  pure ()

superObjectPropertyExpression :: Parser ()
superObjectPropertyExpression = undefined -- objectPropertyExpression

equivalentObjectProperties :: Parser ()
equivalentObjectProperties = do
  symbol "EquivalentObjectProperties"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    some objectPropertyExpression
  pure ()

disjointObjectProperties :: Parser ()
disjointObjectProperties = do
  symbol "DisjointObjectProperties"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    some objectPropertyExpression
  pure ()

objectPropertyDomain :: Parser ()
objectPropertyDomain = do
  symbol "ObjectPropertyDomain"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    classExpression
  pure ()

objectPropertyRange :: Parser ()
objectPropertyRange = do
  symbol "ObjectPropertyRange"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    classExpression
  pure ()

inverseObjectProperties :: Parser ()
inverseObjectProperties = do
  symbol "InverseObjectProperties"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    objectPropertyExpression
  pure ()

genericObjectProperty :: Text -> Parser ()
genericObjectProperty p = do
  symbol p
  parens $ do
    axiomAnnotations
    objectPropertyExpression
  pure ()

functionalObjectProperty :: Parser ()
functionalObjectProperty = genericObjectProperty "FunctionalObjectProperty"

inverseFunctionalObjectProperty :: Parser ()
inverseFunctionalObjectProperty = genericObjectProperty "InverseFunctionalObjectProperty"

reflexiveObjectProperty :: Parser ()
reflexiveObjectProperty = genericObjectProperty "ReflexiveObjectProperty"

irreflexiveObjectProperty :: Parser ()
irreflexiveObjectProperty = genericObjectProperty "IrreflexiveObjectProperty"

symmetricObjectProperty :: Parser ()
symmetricObjectProperty = genericObjectProperty "SymmetricObjectProperty"

asymmetricObjectProperty :: Parser ()
asymmetricObjectProperty = genericObjectProperty "AsymmetricObjectProperty"

transitiveObjectProperty :: Parser ()
transitiveObjectProperty = genericObjectProperty "TransitiveObjectProperty"

dataPropertyAxiom :: Parser ()
dataPropertyAxiom =  subDataPropertyOf $> ()
                 <|> equivalentDataProperties $> ()
                 <|> disjointDataProperties $> ()
                 <|> dataPropertyDomain $> ()
                 <|> dataPropertyRange $> ()
                 <|> functionalDataProperty $> ()

subDataPropertyOf :: Parser ()
subDataPropertyOf = do
  symbol "SubDataPropertyOf"
  parens $ do
    axiomAnnotations
    subDataPropertyExpression
    superDataPropertyExpression
  pure ()

subDataPropertyExpression :: Parser IRI
subDataPropertyExpression = dataPropertyExpression

superDataPropertyExpression :: Parser IRI
superDataPropertyExpression = dataPropertyExpression

equivalentDataProperties :: Parser ()
equivalentDataProperties = do
  symbol "EquivalentDataProperties"
  parens $ do
    axiomAnnotations
    dataPropertyExpression
    some dataPropertyExpression
  pure ()

disjointDataProperties :: Parser ()
disjointDataProperties = do
  symbol "DisjointDataProperties"
  parens $ do
    axiomAnnotations
    dataPropertyExpression
    some dataPropertyExpression
  pure ()

dataPropertyDomain :: Parser ()
dataPropertyDomain = do
  symbol "DataPropertyDomain"
  parens $ do
    axiomAnnotations
    dataPropertyExpression
    classExpression
  pure ()

dataPropertyRange :: Parser ()
dataPropertyRange = do
  symbol "DataPropertyRange"
  parens $ do
    axiomAnnotations
    dataPropertyExpression
    dataRange
  pure ()

functionalDataProperty :: Parser ()
functionalDataProperty = do
  symbol "FunctionalDataProperty"
  parens $ do
    axiomAnnotations
    dataPropertyExpression
  pure ()

datatypeDefinition :: Parser ()
datatypeDefinition = do
  symbol "DatatypeDefinition"
  parens $ do
    axiomAnnotations
    datatype
    dataRange
  pure ()

hasKey :: Parser ()
hasKey = do
  symbol "HasKey"
  parens $ do
    axiomAnnotations
    classExpression
    parens $ many objectPropertyExpression
    parens $ many dataPropertyExpression
  pure ()

assertion :: Parser ()
assertion =  sameIndividual $> ()
         <|> differentIndividuals $> ()
         <|> classAssertion $> ()
         <|> objectPropertyAssertion $> ()
         <|> negativeObjectPropertyAssertion $> ()
         <|> dataPropertyAssertion $> ()
         <|> negativeDataPropertyAssertion $> ()

sourceIndividual :: Parser ()
sourceIndividual = individual

targetIndividual :: Parser ()
targetIndividual = individual

targetValue :: Parser Literal
targetValue = literal

sameIndividual :: Parser ()
sameIndividual = do
  symbol "SameIndividual"
  parens $ do
    axiomAnnotations
    individual
    some individual
  pure ()

differentIndividuals :: Parser ()
differentIndividuals = do
  symbol "DifferentIndividuals"
  parens $ do
    axiomAnnotations
    individual
    some individual
  pure ()

classAssertion :: Parser ()
classAssertion = do
  symbol "ClassAssertion"
  parens $ do
    axiomAnnotations
    classExpression
    individual
  pure ()

objectPropertyAssertion :: Parser ()
objectPropertyAssertion = do
  symbol "ObjectPropertyAssertion"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    sourceIndividual
    targetIndividual
  pure ()

negativeObjectPropertyAssertion :: Parser ()
negativeObjectPropertyAssertion = do
  symbol "NegativeObjectPropertyAssertion"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    sourceIndividual
    targetIndividual
  pure ()

dataPropertyAssertion :: Parser ()
dataPropertyAssertion = do
  symbol "dataPropertyAssertion"
  parens $ do
    axiomAnnotations
    dataPropertyExpression
    sourceIndividual
    targetValue
  pure ()

negativeDataPropertyAssertion :: Parser ()
negativeDataPropertyAssertion = do
  symbol "NegativeDataPropertyAssertion"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    sourceIndividual
    targetIndividual
  pure ()


