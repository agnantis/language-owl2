{-# LANGUAGE OverloadedStrings #-}

module Language.OWL2.Functional.Parser
  ( exportOntologyDoc
  , exportOntologyDocToFile
  , parseOntologyDoc
  )
where

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
import           Language.OWL2.Functional.Pretty as FP

-- DocTest setup
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either

-- | It parses literals
--
-- >>> parseTest (literal *> eof) "\"32\"^^integer"
-- ...
-- unexpected '^'
-- ...
--
-- >>> parseTest (literal <* eof) "\"32\"^^xsd:integer"
-- TypedLiteralC ...
--
-- >>> parseTest (literal <* eof) "\"stringLiteralNoLanguage\""
-- StringLiteralNoLang ...
--
-- >>> parseTest (literal <* eof) "\"stringLiteralWithLang\"@en"
-- StringLiteralLang ...
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
-- >>> parseTest (typedLiteral <* eof) "\"32\"^^xsd:integer"
-- TypedL ...
--
-- >>> parseTest (typedLiteral <* eof) "\"Jack\"^^xsd:string"
-- TypedL ...
--
typedLiteral :: Parser TypedLiteral
typedLiteral = TypedL <$> lexicalValue
                      <*> (symbol "^^" *> datatype)

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyD <$> many prefixDeclaration <*> ontologyP

-- | It parses a prefix declaration
--
-- >>> parseTest (prefixDeclaration <* eof) "Prefix(owl:=<http://www.w3.org/2002/07/owl#>)"
-- PrefixD ...
--
-- >>> parseTest (prefixDeclaration <* eof) "Prefix(:=<http://www.w3.org/2002/07/owl#>)"
-- PrefixD ...
--
prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = do
  _ <- symbol "Prefix"
  parens $ PrefixD <$> prefixName <*> (symbol "=" *> fullIRI)

ontologyP :: Parser Ontology
ontologyP = do
  _ <- symbol "Ontology"
  parens $ do
    ver <- optional $ OntologyVersionIRI <$> ontologyIRI <*> try (optional versionIRI)
    Ontology <$> pure ver <*> many directImport <*> ontologyAnnotations <*> axiomsP

-- | It parses import ontology declarations
--
-- >>> parseTest (directImport <* eof) "Import(<http://www.w3.org/2002/07/owl#>)"
-- ImportD ...
--
directImport :: Parser ImportDeclaration
directImport = ImportD <$> (symbol "Import" *> parens iri)

ontologyAnnotations :: Parser [Annotated Annotation]
ontologyAnnotations = fannotations

axiomsP :: Parser [Axiom]
axiomsP = many axiom 

-- | It parses  declarations
--
-- >>> parseTest (declaration <* eof) "Declaration(Class(<http://www.uom.gr/ai/TestOntology.owl#Child>))"
-- DeclarationAxiom ...
--
-- >>> parseTest (declaration <* eof) "Declaration( NamedIndividual( a:Peter ))"
-- DeclarationAxiom ...
--
-- >>> parseTest (declaration <* eof) "Declaration(AnnotationProperty(test-ont:userAnnot))"
-- DeclarationAxiom ...
--
declaration :: Parser Axiom
declaration = do
  _ <- symbol "Declaration"
  parens $ DeclarationAxiom <$> axiomAnnotations <*> entity

entity :: Parser Entity
entity =  EntityClass              <$> (symbol "Class"              *> parens clazz)
      <|> EntityDatatype           <$> (symbol "Datatype"           *> parens datatype)
      <|> EntityObjectProperty     <$> (symbol "ObjectProperty"     *> parens objectProperty)
      <|> EntityDataProperty       <$> (symbol "DataProperty"       *> parens dataProperty)
      <|> EntityAnnotationProperty <$> (symbol "AnnotationProperty" *> parens annotationProperty)
      <|> EntityIndividual         <$> (symbol "NamedIndividual"    *> parens namedIndividual)

axiomAnnotations :: Parser [Annotated Annotation]
axiomAnnotations = fannotations

-- | It parses an annotation
--
-- >>> parseTest (fannotation <* eof) "Annotation( rdfs:comment \"Male people are people.\" )"
-- Annotated ...
--
-- >>> parseTest (fannotation <* eof) "Annotation  ( rdfs:comment \"Male people are people.\" )"
-- Annotated ...
--
fannotation :: Parser (Annotated Annotation)
fannotation = do
  _ <- symbol "Annotation"
  parens $  do
     annots <- fannotations
     a      <- annotation literal
     pure $ Annotated (annots, a)

fannotations :: Parser [Annotated Annotation]
fannotations = many $ try fannotation

-- | It parses annotation axioms
--
annotationAxiom :: Parser Axiom
annotationAxiom =  subAnnotationPropertyOf
               <|> annotationPropertyDomain
               <|> annotationPropertyRange

-- | It parses annotation assertions
--
-- >>> parseTest (annotationAssertion <* eof) "AnnotationAssertion(rdfs:comment test-ont:kostasAnnot \"comment\")"
-- AnnotationAxiomAssertion ...
-- >>> parseTest (annotationAssertion <* eof) "AnnotationAssertion(owl:priorVersion _:node1 _:node2)"
-- AnnotationAxiomAssertion ...
--
annotationAssertion :: Parser Axiom
annotationAssertion = do
  _ <- symbol "AnnotationAssertion"
  parens $ do
    annots <- axiomAnnotations
    prop   <- annotationProperty
    subj   <- annotationSubject
    val    <- annotationValue literal
    pure $ AnnotationAxiomAssertion annots subj (Annotation prop val)

annotationSubject :: Parser TotalIRI
annotationSubject = totalIRI

-- | It parses sub property annotation assertions
--
-- >>> parseTest (subAnnotationPropertyOf <* eof) "SubAnnotationPropertyOf(test-ont:kostasAnnot rdfs:comment)"
-- AnnotationAxiomSubProperty ...
--
subAnnotationPropertyOf :: Parser Axiom
subAnnotationPropertyOf = do
  _ <- symbol "SubAnnotationPropertyOf"
  parens $ AnnotationAxiomSubProperty
        <$> axiomAnnotations
        <*> subAnnotationProperty
        <*> superAnnotationProperty

subAnnotationProperty :: Parser AnnotationPropertyIRI
subAnnotationProperty = annotationProperty

superAnnotationProperty :: Parser AnnotationPropertyIRI
superAnnotationProperty = annotationProperty

-- | It parses an annotation property domain
--
annotationPropertyDomain :: Parser Axiom
annotationPropertyDomain = do
  _ <- symbol "AnnotationPropertyDomain"
  parens $ AnnotationAxiomDomain
        <$> axiomAnnotations
        <*> annotationProperty
        <*> iri

-- | It parses an annotation property range
--
annotationPropertyRange :: Parser Axiom
annotationPropertyRange = do
  _ <- symbol "AnnotationPropertyRange"
  parens $ AnnotationAxiomRange
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

-- | It parses a data property expression which is always an IRI; There are no compound data
-- property expressions
--
dataPropertyExpression :: Parser DataPropertyIRI
dataPropertyExpression = dataProperty

-- | It parses a data range
--
--
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
  parens $ CExpDataSomeValuesFrom <$> singleOrMany "" dataPropertyExpression <*> dataRange

-- TODO: Protege does not seem to support multiple data property expressions in a single "some"
-- e.g. Was not possible to parse: "EquivalentClasses(test-ont:Test1 DataAllValuesFrom(test-ont:dataProp1 test-ont:dataPro2 xsd:integer)) 
dataAllValuesFrom :: Parser ClassExpression
dataAllValuesFrom = do
  _ <- symbol "DataAllValuesFrom"
  parens $ CExpDataAllValuesFrom <$> singleOrMany "" dataPropertyExpression <*> dataRange

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
axiom =  declaration
     <|> classAxiom
     <|> objectPropertyAxiom
     <|> dataPropertyAxiom
     <|> datatypeDefinition
     <|> hasKey
     <|> assertion
     <|> annotationAxiom

------------------
-- Class Axioms --
------------------
classAxiom :: Parser Axiom
classAxiom =  subClassOf
          <|> equivalentClasses
          <|> disjointClasses
          <|> disjointUnion
          <|> hasKey

subClassOf :: Parser Axiom
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

equivalentClasses :: Parser Axiom
equivalentClasses = do
  _ <- symbol "EquivalentClasses"
  parens $ do
    annots <- axiomAnnotations
    (x, xs) <- extract filterClassIRI <$> doubleOrMany "" classExpression
    pure $ ClassAxiomEquivalentClasses annots x xs

disjointClasses :: Parser Axiom
disjointClasses = do
  _ <- symbol "DisjointClasses"
  parens $ do
    annots <- axiomAnnotations
    (x, xs) <- extract filterClassIRI <$> doubleOrMany "" classExpression
    pure $ ClassAxiomDisjointClasses annots x xs

disjointUnion :: Parser Axiom
disjointUnion = do
  _ <- symbol "DisjointUnion"
  parens $ ClassAxiomDisjointUnion
        <$> axiomAnnotations
        <*> clazz
        <*> doubleOrMany "" classExpression

hasKey :: Parser Axiom
hasKey = do
  _ <- symbol "HasKey"
  parens $ ClassAxiomHasKey
        <$> axiomAnnotations
        <*> classExpression
        <*> (NE.fromList <$> some ((ObjectPE <$> objectPropertyExpression) <|> (DataPE <$> dataPropertyExpression)))

----------------------------
-- Object Property Axioms --
----------------------------
objectPropertyAxiom :: Parser Axiom
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
subObjectPropertyOf :: Parser Axiom
subObjectPropertyOf = do
  _ <- symbol "SubObjectPropertyOf"
  parens $ do
    annots <- axiomAnnotations
    sub <- subObjectPropertyExpression
    sup <- superObjectPropertyExpression
    pure $ case sub of
      Left s  -> ObjectPropAxiomSubProperty annots s sup
      Right x -> ObjectPropAxiomChainSubProperty annots x sup

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

equivalentObjectProperties :: Parser Axiom
equivalentObjectProperties = do
  _ <- symbol "EquivalentObjectProperties"
  parens $ do
    annots <- axiomAnnotations
    (x, xs) <- extract filterObjectPropIRI <$> doubleOrMany "" objectPropertyExpression
    pure $ ObjectPropAxiomEquivalent annots x xs

disjointObjectProperties :: Parser Axiom
disjointObjectProperties = do
  _ <- symbol "DisjointObjectProperties"
  parens $ do
    annots <- axiomAnnotations
    (x, xs) <- extract filterObjectPropIRI <$> doubleOrMany "" objectPropertyExpression
    pure $ ObjectPropAxiomDisjoint annots x xs

objectPropertyDomain :: Parser Axiom
objectPropertyDomain = do
  _ <- symbol "ObjectPropertyDomain"
  parens $ ObjectPropAxiomDomain <$> axiomAnnotations <*> objectPropertyExpression <*> classExpression

objectPropertyRange :: Parser Axiom
objectPropertyRange = do
  _ <- symbol "ObjectPropertyRange"
  parens $ ObjectPropAxiomRange <$> axiomAnnotations <*> objectPropertyExpression <*> classExpression

inverseObjectProperties :: Parser Axiom
inverseObjectProperties = do
  _ <- symbol "InverseObjectProperties"
  parens $ ObjectPropAxiomInverse <$> axiomAnnotations <*> objectPropertyExpression <*> objectPropertyExpression

objectPropertyCharactersistics :: Parser Axiom
objectPropertyCharactersistics = do
  chc <- choice . fmap symbol $ M.keys objectPropertyCharacteristics
  parens $ do
    annots <- axiomAnnotations
    ope    <- objectPropertyExpression
    pure $ ObjectPropAxiomCharacteristics annots ope (objectPropertyCharacteristics M.! chc)

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

-- | It parses all data property related axioms
--
--
-- >>> parseTest (subDataPropertyOf <* eof) "SubDataPropertyOf( a:hasLastName a:hasName  )"
-- DataPropAxiomSubProperty ...
--
-- >>> parseTest (equivalentDataProperties <* eof) "EquivalentDataProperties( a:hasName a:hasAddress )"
-- DataPropAxiomEquivalent ...
--
-- >>> parseTest (disjointDataProperties <* eof) "DisjointDataProperties( a:hasName a:hasAddress )"
-- DataPropAxiomDisjoint ...
--
-- >>> parseTest (dataPropertyDomain <* eof) "DataPropertyDomain( a:hasName a:Person )"
-- DataPropAxiomDomain ...
--
-- >>> parseTest (dataPropertyRange <* eof) "DataPropertyRange( a:hasName xsd:string )"
-- DataPropAxiomRange ...
--
-- >>> parseTest (functionalDataProperty<* eof) "FunctionalDataProperty( a:hasAge )"
-- DataPropAxiomCharacteristics ...
--
dataPropertyAxiom :: Parser Axiom
dataPropertyAxiom =  subDataPropertyOf
                 <|> equivalentDataProperties
                 <|> disjointDataProperties
                 <|> dataPropertyDomain
                 <|> dataPropertyRange
                 <|> functionalDataProperty

-- | It parses a sub data property declaration
--
-- >>> parseTest (subDataPropertyOf <* eof) "SubDataPropertyOf( a:hasLastName a:hasName  )"
-- DataPropAxiomSubProperty ...
--
subDataPropertyOf :: Parser Axiom
subDataPropertyOf = do
  _ <- symbol "SubDataPropertyOf"
  parens $ DataPropAxiomSubProperty <$> axiomAnnotations <*> subDataPropertyExpression <*> superDataPropertyExpression

subDataPropertyExpression :: Parser DataPropertyIRI
subDataPropertyExpression = dataPropertyExpression

superDataPropertyExpression :: Parser DataPropertyIRI
superDataPropertyExpression = dataPropertyExpression

-- | It parses an equivalent data property declaration
--
-- >>> parseTest (equivalentDataProperties <* eof) "EquivalentDataProperties( a:hasName a:hasAddress )"
-- DataPropAxiomEquivalent ...
--
equivalentDataProperties :: Parser Axiom
equivalentDataProperties = do
  _ <- symbol "EquivalentDataProperties"
  parens $ do
    annots <- axiomAnnotations
    (x, xs) <- extract (const True) <$> doubleOrMany "" dataPropertyExpression
    pure $ DataPropAxiomEquivalent annots x xs

-- | It parses a disjoint data property declaration
--
-- >>> parseTest (disjointDataProperties <* eof) "DisjointDataProperties( a:hasName a:hasAddress )"
-- DataPropAxiomDisjoint ...
--
disjointDataProperties :: Parser Axiom
disjointDataProperties = do
  _ <- symbol "DisjointDataProperties"
  parens $ do
    annots <- axiomAnnotations
    (x, xs) <- extract (const True) <$> doubleOrMany "" dataPropertyExpression
    pure $ DataPropAxiomDisjoint annots x xs

-- | It parses a data property domain declaration
--
-- >>> parseTest (dataPropertyDomain <* eof) "DataPropertyDomain( a:hasName a:Person )"
-- DataPropAxiomDomain ...
--
dataPropertyDomain :: Parser Axiom
dataPropertyDomain = do
  _ <- symbol "DataPropertyDomain"
  parens $ DataPropAxiomDomain <$> axiomAnnotations <*> dataPropertyExpression <*> classExpression

-- | It parses a data property range declaration
--
-- >>> parseTest (dataPropertyRange <* eof) "DataPropertyRange( a:hasName xsd:string )"
-- DataPropAxiomRange ...
--
dataPropertyRange :: Parser Axiom
dataPropertyRange = do
  _ <- symbol "DataPropertyRange"
  parens $ DataPropAxiomRange <$> axiomAnnotations <*> dataPropertyExpression <*> dataRange

-- | It parses a functional data property declaration
--
-- >>> parseTest (functionalDataProperty<* eof) "FunctionalDataProperty( a:hasAge )"
-- DataPropAxiomCharacteristics ...
--
functionalDataProperty :: Parser Axiom
functionalDataProperty = do
  _ <- symbol "FunctionalDataProperty"
  parens $ DataPropAxiomCharacteristics <$> axiomAnnotations <*> dataPropertyExpression <*> pure FUNCTIONAL_DPE

datatypeDefinition :: Parser Axiom
datatypeDefinition = do
  _ <- symbol "DatatypeDefinition"
  parens $ DatatypeAxiomDefinition <$> axiomAnnotations <*> datatype <*> dataRange

assertion :: Parser Axiom
assertion =  sameIndividual
         <|> differentIndividuals
         <|> classAssertion
         <|> objectPropertyAssertion
         <|> negativeObjectPropertyAssertion
         <|> dataPropertyAssertion
         <|> negativeDataPropertyAssertion
         <|> annotationAssertion

sourceIndividual :: Parser Individual
sourceIndividual = individual

targetIndividual :: Parser Individual
targetIndividual = individual

targetValue :: Parser Literal
targetValue = literal

sameIndividual :: Parser Axiom
sameIndividual = do
  _ <- symbol "SameIndividual"
  parens $ do
    annots  <- axiomAnnotations
    (i, is) <- extract filterNamedIRI <$> doubleOrMany "" individual
    pure $ AssertionAxiomSameIndividuals annots i is

differentIndividuals :: Parser Axiom
differentIndividuals = do
  _ <- symbol "DifferentIndividuals"
  parens $ do
    annots  <- axiomAnnotations
    (i, is) <- extract filterNamedIRI <$> doubleOrMany "" individual
    pure $ AssertionAxiomDifferentIndividuals annots i is

classAssertion :: Parser Axiom
classAssertion = do
  _ <- symbol "ClassAssertion"
  parens $ do
    annots <- axiomAnnotations
    ce     <- classExpression
    ind    <- individual
    pure $ AssertionAxiomClass annots ind ce

objectPropertyAssertion :: Parser Axiom
objectPropertyAssertion = do
  _ <- symbol "ObjectPropertyAssertion"
  parens $ AssertionAxiomObjectProperty
        <$> axiomAnnotations
        <*> objectPropertyExpression
        <*> sourceIndividual
        <*> targetIndividual

negativeObjectPropertyAssertion :: Parser Axiom
negativeObjectPropertyAssertion = do
  _ <- symbol "NegativeObjectPropertyAssertion"
  parens $ AssertionAxiomNegativeObjectProperty
        <$> axiomAnnotations
        <*> objectPropertyExpression
        <*> sourceIndividual
        <*> targetIndividual

dataPropertyAssertion :: Parser Axiom
dataPropertyAssertion = do
  _ <- symbol "DataPropertyAssertion"
  parens $ AssertionAxiomDataProperty
        <$> axiomAnnotations
        <*> dataPropertyExpression
        <*> sourceIndividual
        <*> targetValue

negativeDataPropertyAssertion :: Parser Axiom
negativeDataPropertyAssertion = do
  _ <- symbol "NegativeDataPropertyAssertion"
  parens $ AssertionAxiomNegativeDataProperty
        <$> axiomAnnotations
        <*> dataPropertyExpression
        <*> sourceIndividual
        <*> targetValue

------------------
-- Parser utils --
------------------

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
 

-- | It tries to parse an ontology file in functional format
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

-- | Exports the ontology document in Functional format
--
exportOntologyDoc :: OntologyDocument -- ^ The ontology document
                  -> Text             -- ^ the output in Manchster format
exportOntologyDoc = T.pack . show . FP.pretty


-- | Exports the ontology document in a file in Functional format
--
exportOntologyDocToFile :: FilePath         -- ^ The file to save the ontology
                        -> OntologyDocument -- ^ The ontology document
                        -> IO ()
exportOntologyDocToFile f ontDoc = T.writeFile f (exportOntologyDoc ontDoc)

--parseAndSave :: FilePath -> IO ()
--parseAndSave f = do
--  (Just ontDoc) <- parseOntologyDoc f
--  writeFile "output.hs.owl" (show . FP.pretty $ ontDoc)

