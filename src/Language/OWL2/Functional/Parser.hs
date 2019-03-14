{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Functional.Parser where

import           Prelude                           hiding ( exponent )
import           Data.Functor                             ( ($>) )
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
prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = do
  symbol "Prefix"
  parens $ PrefixD <$> prefixName <*> (symbol "=" *> fullIRI)

ontology :: Parser ()
ontology = do
  symbol "Ontology"
  parens $ do
    optional $ do
      ontologyIRI
      try (optional versionIRI) -- Maybe (iri, Maybe iri)
    many directImport
    ontologyAnnotations
    axioms
  pure ()

-- | It parses import ontology declarations
--
-- >>> parseTest (directImport *> eof) "Import(<http://www.w3.org/2002/07/owl#>)"
-- ()
--
directImport :: Parser ImportDeclaration
directImport = ImportD <$> (symbol "Import" *> parens iri)

ontologyAnnotations :: Parser [Annotated Annotation]
ontologyAnnotations = fannotations

axioms :: Parser [()]
axioms = many axiom

-- | It parses  declarations
--
-- >>> parseTest (declaration *> eof) "Declaration(Class(<http://www.uom.gr/ai/TestOntology.owl#Child>))"
-- ()
--
declaration :: Parser ()
declaration = do
  symbol "Declaration"
  parens $ do
    axiomAnnotations
    entity
  pure ()

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
  symbol "Annotation"
  parens $  do
     annots <- fannotations
     a      <- annotation literal
     pure $ Annotated (annots, a)

fannotations :: Parser [Annotated Annotation]
fannotations = many fannotation

annotationAnnotations :: Parser [Annotated Annotation]
annotationAnnotations = fannotations

annotationAxiom :: Parser AnnotationPropertyFrame
annotationAxiom =  annotationAssertion
               <|> subAnnotationPropertyOf
               <|> annotationPropertyDomain
               <|> annotationPropertyRange

-- | It parses annotation assertions
--
-- >>> parseTest (annotationAssertion *> eof) "AnnotationAssertion(rdfs:comment test-ont:kostasAnnot \"comment\")"
-- ()
-- >>> parseTest (annotationAssertion *> eof) "AnnotationAssertion(owl:priorVersion _:node1 _:node2)"
-- ()
--
annotationAssertion :: Parser AnnotationPropertyFrame
annotationAssertion = do
  symbol "AnnotationAssertion"
  parens $ do
    annots   <- axiomAnnotations
    property <- annotationProperty
    subject  <- annotationSubject
    target   <- annotationValue literal
    let annotList = NE.fromList [Annotated (annots, Annotation property target)]
    pure $ AnnotationPropertyF subject [AnnotationAPE annotList]

annotationSubject :: Parser AnnotationPropertyIRI
annotationSubject = annotationPropertyIRI

-- | It parses sub property annotation assertions
--
-- >>> parseTest (subAnnotationPropertyOf *> eof) "SubAnnotationPropertyOf(test-ont:kostasAnnot rdfs:comment)"
-- ()
--
subAnnotationPropertyOf :: Parser AnnotationPropertyFrame
subAnnotationPropertyOf = do
  symbol "SubAnnotationPropertyOf"
  parens $ do
    annots   <- axiomAnnotations
    subAnn   <- subAnnotationProperty
    superAnn <- superAnnotationProperty
    let annotList = NE.fromList [Annotated (annots, superAnn)] 
    pure $ AnnotationPropertyF subAnn [SubPropertyOfAPE annotList]

subAnnotationProperty :: Parser AnnotationPropertyIRI
subAnnotationProperty = annotationProperty

superAnnotationProperty :: Parser AnnotationPropertyIRI
superAnnotationProperty = annotationProperty

annotationPropertyDomain :: Parser AnnotationPropertyFrame
annotationPropertyDomain = do
  symbol "AnnotationPropertyDomain"
  parens $ do
    annots <- axiomAnnotations
    prop   <- annotationProperty
    iri'   <- iri
    let annotList = NE.fromList [Annotated (annots, iri')] 
    pure $ AnnotationPropertyF prop [DomainAPE annotList]

annotationPropertyRange :: Parser AnnotationPropertyFrame
annotationPropertyRange = do
  symbol "AnnotationPropertyRange"
  parens $ do
    annots <- axiomAnnotations
    prop   <- annotationProperty
    iri'   <- iri
    let annotList = NE.fromList [Annotated (annots, iri')] 
    pure $ AnnotationPropertyF prop [RangeAPE annotList]

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
  symbol "DataIntersectionOf"
  elems <- parens $ atLeast2List' <$> dataRange <*> (NE.fromList <$> some dataRange)
  pure $ IntersectionDR elems

dataUnionOf :: Parser DataRange
dataUnionOf = do
  symbol "DataUnionOf"
  elems <- parens $ atLeast2List' <$> dataRange <*> (NE.fromList <$> some dataRange)
  pure $ UnionDR elems

dataComplementOf :: Parser DataRange
dataComplementOf = do
  symbol "DataComplementOf"
  parens $ ComplementDR <$> dataRange

dataOneOf :: Parser DataRange
dataOneOf = do
  symbol "DataOneOf"
  elem <- parens $ NE.fromList <$> some literal
  pure $ OneOfDR elem

datatypeRestriction :: Parser DataRange
datatypeRestriction = do
  symbol "DatatypeRestriction"
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
  symbol "ObjectIntersectionOf"
  parens $ CExpObjectIntersectionOf <$> doubleOrMany "" classExpression

objectUnionOf :: Parser ClassExpression
objectUnionOf = do
  symbol "ObjectUnionOf"
  parens $ CExpObjectUnionOf <$> doubleOrMany "" classExpression

objectComplementOf :: Parser ClassExpression
objectComplementOf = do
  symbol "ObjectComplementOf"
  parens $ CExpObjectComplementOf <$> classExpression

objectOneOf :: Parser ClassExpression
objectOneOf = do
  symbol "ObjectOneOf"
  parens $ CExpObjectOneOf <$> singleOrMany "" individual

objectSomeValuesFrom :: Parser ClassExpression
objectSomeValuesFrom = do
  symbol "ObjectSomeValuesFrom"
  parens $ CExpObjectSomeValuesFrom <$> objectPropertyExpression <*> classExpression

objectAllValuesFrom :: Parser ClassExpression
objectAllValuesFrom = do
  symbol "ObjectAllValuesFrom"
  parens $ CExpObjectAllValuesFrom <$> objectPropertyExpression <*> classExpression
    
objectHasValue :: Parser ClassExpression
objectHasValue = do
  symbol "ObjectHasValue"
  parens $ CExpObjectHasValue <$> objectPropertyExpression <*> individual

objectHasSelf :: Parser ClassExpression
objectHasSelf = do
  symbol "ObjectHasSelf"
  parens $ CExpObjectHasSelf <$> objectPropertyExpression

objectCardinality
  :: Text
  -> (Int -> ObjectPropertyExpression -> Maybe ClassExpression -> ClassExpression)
  -> Parser ClassExpression
objectCardinality l c = do
  symbol l
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
  symbol "DataSomeValuesFrom"
  parens $ CExpDataSomeValuesFrom <$> dataPropertyExpression <*> dataRange

-- TODO: Protege does not seem to support multiple data property expressions in a single "some"
-- e.g. Was not possible to parse: "EquivalentClasses(test-ont:Test1 DataAllValuesFrom(test-ont:dataProp1 test-ont:dataPro2 xsd:integer)) 
dataAllValuesFrom :: Parser ClassExpression
dataAllValuesFrom = do
  symbol "DataAllValuesFrom"
  parens $ CExpDataAllValuesFrom <$> dataPropertyExpression <*> dataRange

dataHasValue :: Parser ClassExpression
dataHasValue = do
  symbol "DataHasValue"
  parens $ CExpDataHasValue <$> dataPropertyExpression <*> literal

dataCardinality
  :: Text
  -> (Int -> DataPropertyExpression -> Maybe DataRange -> ClassExpression)
  -> Parser ClassExpression
dataCardinality l c = do
  symbol l
  parens $ c <$> nonNegativeInteger <*> dataPropertyExpression <*> optional dataRange

dataMinCardinality :: Parser ClassExpression
dataMinCardinality = dataCardinality "DataMinCardinality" CExpDataMinCardinality

dataMaxCardinality :: Parser ClassExpression
dataMaxCardinality = dataCardinality "DataMaxCardinality" CExpDataMaxCardinality

dataExactCardinality :: Parser ClassExpression
dataExactCardinality = dataCardinality "DataExactCardinality" CExpDataExactCardinality

axiom :: Parser ()
axiom =  declaration $> ()
     <|> classAxiom $> ()
     <|> objectPropertyAxiom $> ()
     <|> dataPropertyAxiom $> ()
     <|> datatypeDefinition $> ()
     <|> hasKey $> ()
     <|> assertion $> ()
     <|> annotationAxiom $> ()

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
  symbol "SubClassOf"
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
  symbol "EquivalentClasses"
  parens $ ClassAxiomEquivalentClasses
        <$> axiomAnnotations
        <*> doubleOrMany "" classExpression

disjointClasses :: Parser ClassAxiom
disjointClasses = do
  symbol "DisjointClasses"
  parens $ ClassAxiomDisjointClasses
        <$> axiomAnnotations
        <*> doubleOrMany "" classExpression

disjointUnion :: Parser ClassAxiom
disjointUnion = do
  symbol "DisjointUnion"
  parens $ ClassAxiomDisjointUnion
        <$> axiomAnnotations
        <*> clazz
        <*> doubleOrMany "" classExpression

hasKey :: Parser ClassAxiom
hasKey = do
  symbol "HasKey"
  parens $ ClassAxiomHasKey
        <$> axiomAnnotations
        <*> classExpression
        <*> (NE.fromList <$> some ((ObjectPE <$> objectPropertyExpression) <|> (DataPE <$> dataPropertyExpression)))

----------------------------
-- Object Property Axioms --
----------------------------
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

superObjectPropertyExpression :: Parser ObjectPropertyExpression
superObjectPropertyExpression = objectPropertyExpression

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

subDataPropertyExpression :: Parser DataPropertyIRI
subDataPropertyExpression = dataPropertyExpression

superDataPropertyExpression :: Parser DataPropertyIRI
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

assertion :: Parser ()
assertion =  sameIndividual $> ()
         <|> differentIndividuals $> ()
         <|> classAssertion $> ()
         <|> objectPropertyAssertion $> ()
         <|> negativeObjectPropertyAssertion $> ()
         <|> dataPropertyAssertion $> ()
         <|> negativeDataPropertyAssertion $> ()

sourceIndividual :: Parser Individual
sourceIndividual = individual

targetIndividual :: Parser Individual
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
  symbol "DataPropertyAssertion"
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
    dataPropertyExpression
    sourceIndividual
    targetIndividual
  pure ()


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

