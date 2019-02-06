{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Functional.Parser where

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

import           Language.OWL2.Manchester.Parser ( Parser, symbol, parens, prefixName, fullIRI
                                                 , ontologyIRI, versionIRI, iri
                                                 , literal, nodeID
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

axiom :: Parser ()
axiom =  declaration $> ()
     <|> classAxiom $> ()
     <|> objectPropertyAxiom $> ()
     <|> dataPropertyAxiom $> ()
     <|> datatypeDefinition $> ()
     <|> hasKey $> ()
     <|> assertion $> ()
     <|> annotationAxiom $> ()

axiomAnnotations :: Parser [()]
axiomAnnotations = many annotation

annotationValue :: Parser ()
annotationValue =  anonymousIndividual $> ()
               <|> iri $> ()
               <|> literal $> ()

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
    dataRange
    many dataRange
  pure ()

dataUnionOf :: Parser ()
dataUnionOf = do
  symbol "DataUnionOf"
  parens $ do
    dataRange
    dataRange
    many dataRange
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

classAxiom :: Parser ()
classAxiom =  subclassOf $> ()
          <|> equivalentClasses $> ()
          <|> disjointClasses $> ()
          <|> disjointUnion $> ()

objectPropertyAxiom :: Parser ()
objectPropertyAxiom =  subOjectPropertyOf $> ()
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

dataPropertyAxiom :: Parser ()
dataPropertyAxiom =  subDataPropertyOf $> ()
                 <|> equivalentDataProperties $> ()
                 <|> disjointDataProperties $> ()
                 <|> dataPropertyDomain $> ()
                 <|> dataPropertyRange $> ()
                 <|> functionalDataProperty $> ()

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

subclassOf :: Parser ()
subclassOf = do
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
    classExpression
    many classExpression
  pure ()

disjointClasses :: Parser ()
disjointClasses = do
  symbol "DisjointClasses"
  parens $ do
    axiomAnnotations
    classExpression
    classExpression
    many classExpression
  pure ()

disjointUnion :: Parser ()
disjointUnion = do
  symbol "DisjointUnion"
  parens $ do
    axiomAnnotations
    clazz
    disjointClassExpressions
  pure ()

annotationSubject:: Parser ()
annotationSubject =  iri $> ()
                 <|> anonymousIndividual $> ()

subOjectPropertyOf :: Parser ()
subOjectPropertyOf = do
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
    objectPropertyExpression
    many objectPropertyExpression
  pure ()

superObjectPropertyExpression :: Parser ()
superObjectPropertyExpression = undefined -- objectPropertyExpression

equivalentObjectProperties :: Parser ()
equivalentObjectProperties = do
  symbol "EquivalentObjectProperties"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    objectPropertyExpression
    many objectPropertyExpression
  pure ()

disjointObjectProperties :: Parser ()
disjointObjectProperties = do
  symbol "DisjointObjectProperties"
  parens $ do
    axiomAnnotations
    objectPropertyExpression
    objectPropertyExpression
    many objectPropertyExpression
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

subDataPropertyOf :: Parser ()
subDataPropertyOf = pure ()

equivalentDataProperties :: Parser ()
equivalentDataProperties = pure ()

disjointDataProperties :: Parser ()
disjointDataProperties = pure ()

dataPropertyDomain :: Parser ()
dataPropertyDomain = pure ()

dataPropertyRange :: Parser ()
dataPropertyRange = pure ()

functionalDataProperty :: Parser ()
functionalDataProperty = pure ()

classExpression :: Parser ()
classExpression = pure ()

sameIndividual :: Parser ()
sameIndividual = pure ()

differentIndividuals :: Parser ()
differentIndividuals = pure ()

classAssertion :: Parser ()
classAssertion = pure ()

objectPropertyAssertion :: Parser ()
objectPropertyAssertion = pure ()

negativeObjectPropertyAssertion :: Parser ()
negativeObjectPropertyAssertion = pure ()

dataPropertyAssertion :: Parser ()
dataPropertyAssertion = pure ()

negativeDataPropertyAssertion :: Parser ()
negativeDataPropertyAssertion = pure ()

disjointClassExpressions :: Parser ()
disjointClassExpressions = pure ()

