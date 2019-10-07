{-# LANGUAGE QuasiQuotes #-}

module Language.OWL2.Examples.QuasiQ where

import Language.OWL2.TH.TH
import Language.OWL2.Types


-- | Reads an ontology document using the 'ontologydoc' quosi-quoation
-- 
sampleOntologyDoc :: OntologyDocument
sampleOntologyDoc = [ontologydoc|

# Full ontology document
Prefix(:=<http://www.semanticweb.org/konstantine/ontologies/2019/1/test-ont#>)
Prefix(dc:=<http://purl.org/dc/elements/1.1/>)
Prefix(owl:=<http://www.w3.org/2002/07/owl#>)
Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)
Prefix(xml:=<http://www.w3.org/XML/1998/namespace>)
Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)
Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
Prefix(test-ont:=<http://www.semanticweb.org/konstantine/ontologies/2019/1/test-ont#>)

Ontology(<http://www.semanticweb.org/konstantine/ontologies/2019/1/test-ont>
  Annotation(rdfs:comment "Ontology comment!")
  Annotation(rdfs:label "Ontology label!")
  
  Declaration(Annotation(rdfs:comment "declaration of class Test1") Class(test-ont:Test1))
  Declaration(Class(test-ont:Test11))
  Declaration(Class(test-ont:Test12))
  Declaration(Class(test-ont:Test2))
  Declaration(Class(test-ont:Test3))
  Declaration(Class(test-ont:Test4))
  Declaration(Class(test-ont:Test5))
  Declaration(Class(test-ont:Test6))
  Declaration(Class(test-ont:Test7))
  Declaration(Class(test-ont:Test8))
  Declaration(Class(test-ont:ind1))
  Declaration(ObjectProperty(test-ont:ObjectProp1))
  Declaration(ObjectProperty(test-ont:ObjectProp2))
  Declaration(ObjectProperty(test-ont:ObjectProp3))
  Declaration(ObjectProperty(test-ont:objectProp4))
  Declaration(DataProperty(test-ont:dataProp1))
  Declaration(DataProperty(test-ont:dataProp2))
  Declaration(DataProperty(test-ont:dataProp3))
  Declaration(NamedIndividual(test-ont:ind1))
  Declaration(NamedIndividual(test-ont:ind2))
  Declaration(AnnotationProperty(test-ont:kostasAnnot))
  Declaration(AnnotationProperty(rdfs:comment))
  Declaration(AnnotationProperty(rdfs:label))
  Declaration(AnnotationProperty(rdfs:seeAlso))
  Declaration(AnnotationProperty(owl:priorVersion))
  Declaration(Datatype(test-ont:test_dt))
  Declaration(Datatype(rdf:PlainLiteral))
  Declaration(Datatype(rdfs:Literal))
  Declaration(Datatype(xsd:integer))
  
  AnnotationAssertion(rdfs:comment test-ont:kostasAnnot "Annot comment")
  AnnotationAssertion(owl:priorVersion test-ont:kostasAnnot _:genid2147483648)
  SubAnnotationPropertyOf(test-ont:kostasAnnot rdfs:comment)
  SubAnnotationPropertyOf(test-ont:kostasAnnot rdfs:seeAlso)
  AnnotationPropertyRange(test-ont:kostasAnnot test-ont:Test1)
  
  DatatypeDefinition(test-ont:test_dt DatatypeRestriction(rdfs:Literal xsd:maxLength "10"^^rdfs:Literal))
  
  EquivalentClasses(Annotation(rdfs:comment "comment") Annotation(rdfs:label "label") test-ont:Test1 test-ont:Test4)
  EquivalentClasses(test-ont:Test1 ObjectIntersectionOf(ObjectIntersectionOf(test-ont:Test2 test-ont:Test3) ObjectOneOf(test-ont:ind1 test-ont:ind2)))
  EquivalentClasses(test-ont:Test1 ObjectSomeValuesFrom(ObjectInverseOf(test-ont:ObjectProp1) test-ont:Test11))
  EquivalentClasses(DataSomeValuesFrom(test-ont:dataProp1 xsd:real) test-ont:Test2 test-ont:Test1)
  
  AnnotationAssertion(rdfs:comment test-ont:Test11 "This is Test1 annotation comment")
  AnnotationAssertion(Annotation(rdfs:comment "kostas1") rdfs:label test-ont:Test11 "Thos is Test11 label")
  
  SubClassOf(Annotation(rdfs:comment "test12 is subclass of test2") test-ont:Test12 test-ont:Test2)
  
  EquivalentClasses(test-ont:Test5 test-ont:Test7 test-ont:Test8)
  
  EquivalentClasses(test-ont:Test6 ObjectIntersectionOf(test-ont:Test2 test-ont:Test3 test-ont:Test5))
  
  AnnotationAssertion(rdfs:comment test-ont:ind1 "Individual Annotation for ind1 XXX")
  
  ClassAssertion(test-ont:Test4 test-ont:ind1)
  
  SubObjectPropertyOf(ObjectInverseOf(test-ont:ObjectProp2) ObjectInverseOf(test-ont:ObjectProp1))
  SubObjectPropertyOf(ObjectPropertyChain(test-ont:ObjectProp2 test-ont:ObjectProp3) test-ont:objectProp4)
  AnnotationAssertion(rdfs:label _:genid2147483649 "Annot label")
)
|]

