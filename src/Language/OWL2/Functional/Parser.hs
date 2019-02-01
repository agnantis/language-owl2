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

import           Language.OWL2.Manchester.Parser hiding (prefixDeclaration, ontology, annotation, entity)
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

annotationProperty :: Parser IRI
annotationProperty = iri

annotationValue :: Parser ()
annotationValue = do
  pure ()

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

objectProperty :: Parser IRI
objectProperty = iri

dataProperty :: Parser IRI
dataProperty = iri

namedIndividual :: Parser IRI
namedIndividual = iri

individual :: Parser ()
individual =  namedIndividual $> ()
          <|> anonymousIndividual $> ()

classAxiom :: Parser ()
classAxiom =  subclassOf $> ()
          <|> equivalentClasses $> ()
          <|> disjointClasses $> ()
          <|> disjointUnion $> ()

objectPropertyAxiom :: Parser ()
objectPropertyAxiom = do
  pure ()

dataPropertyAxiom :: Parser ()
dataPropertyAxiom = do
  pure ()

datatypeDefinition :: Parser ()
datatypeDefinition = do
  pure ()

hasKey :: Parser ()
hasKey = do
  pure ()

assertion :: Parser ()
assertion = do
  pure ()

subclassOf :: Parser ()
subclassOf = do
  pure ()

equivalentClasses :: Parser ()
equivalentClasses = do
  pure ()

disjointClasses :: Parser ()
disjointClasses = do
  pure ()

disjointUnion :: Parser ()
disjointUnion = do
  pure ()

annotationSubject:: Parser ()
annotationSubject= do
  pure ()

anonymousIndividual :: Parser NodeID
anonymousIndividual = nodeID

