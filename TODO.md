# TODOs

- [X] Replace String with Text for the parser
- [X] If I want to follow the format of the `language-java` parser, I may need to reconstruct my project:
  - Move module `Types` -> `Language.Owl2.[Manchester|Functional].Syntax`
  - Move module `OwlParser` -> `Language.Owl2.Functional.Parser`
  - Create module `Language.Owl2.Functional.Pretty`
- [ ] User `modern-uri` library for uri parsing. It supports URI (but not IRI) parsing through the megaparsec library
- [ ] Implement OWL-API


### OWL API

Some functionalities that can/should be implements

- Getters
  - [ ] Get all axioms related to a specific resource (i.e., IRI)
  - [ ] Get all entities based on type (e.g. get all classes, all properties)
  - [ ] Get all direct and indirect subclasses (through reasoner?)

- Setters
  - [ ] Add a new entity (e.g. class, property)
  - [ ] Add a new axiom (related to an entity)


# Comments

- Axioms
  - Declaration (?f)
    - Class
    - Datatype
    - ObjectProperty
    - DataProperty
    - AnnotationProperty
    - NamedIndividual
  - ClassAxiom
    - SubClassOf
    - EquivalentClasses
    - DisjointClasses
    - DisjointUnion
  - ObjectProperyAxiom
    - SubObjectPropertyOf 
    - EquivalentObjectProperties 
    - DisjointObjectProperties 
    - InverseObjectProperties 
    - ObjectPropertyDomain 
    - ObjectPropertyRange 
    - FunctionalObjectProperty 
    - InverseFunctionalObjectProperty 
    - ReflexiveObjectProperty 
    - IrreflexiveObjectProperty 
    - SymmetricObjectProperty 
    - AsymmetricObjectProperty 
    - TransitiveObjectProperty
  - DataPropertyAxiom
    - SubDataPropertyOf 
    - EquivalentDataProperties 
    - DisjointDataProperties 
    - DataPropertyDomain 
    - DataPropertyRange 
    - FunctionalDataProperty
  - DatatypeDefinition
  - HasKey
  - Assertion
    - SameIndividual 
    - DifferentIndividuals 
    - ClassAssertion 
    - ObjectPropertyAssertion 
    - NegativeObjectPropertyAssertion 
    - DataPropertyAssertion 
    - NegativeDataPropertyAssertion
  - AnnotationAxiom
    - AnnotationAssertion 
    - SubAnnotationPropertyOf 
    - AnnotationPropertyDomain 
    - AnnotationPropertyRange
  - Individual (?m)
    - Types
    - Facts
    - SameAs
    - DifferentFrom
  - misc (?m)
    - EquivalentClasses
    - DisjointClasses
    - EquivalentProperties
      - Data
      - Object
    - DisjointProperties
      - Data
      - Object
    - SameIndividual
    - DifferentIndividuals
