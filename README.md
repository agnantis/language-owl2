# Language-OWL2

A Haskell parser and pretty printer for various dialects of the OWL 2 Web Ontology Language.
Its current version is able to parse and pretty print the [Manchester](https://www.w3.org/TR/owl2-manchester-syntax) and the [Functional](https://www.w3.org/TR/owl2-syntax/) syntax of OWL2.

**DISCLAIMER**: This is a work in progress and by no means complete


## Technologies/Tools

- Language: [Haskell](https://www.haskell.org/)
- Environment/Build Tools: [Nix](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure)/[Cabal](https://cabal.readthedocs.io/en/latest/)
- Parser: [megaparsec](http://hackage.haskell.org/package/megaparsec) 


## Possible issues in Manchester syntax specification

- There are many _keys_ that are *required* to be annotated. However:
  - Annotations usually are optional and
  - In the example at the end of the specification page, there are no annotations for these cases, something that increases my suspicion that the grammar is not correct for the following cases:
    - `dataFrame` -> `EquivalentTo:`
    - `classFrame` -> `DisjointUnionOf:`
    - `classFrame` -> `HasKey:`
    - `objectPropertyFrame` -> `SubPropertyChain:`
    - `dataPropertyFrame` -> `Characteristics:`
    - `misc` -> all cases
- Based on the grammar, a `classFrame` is defined by either a `HasKey` _construct_ or by many _sub-constructs_ of a list of 5 different _sub-constructs_. However, in the example, there is the `Class: Person` which contains both `HasKey` and the other _sub-constructs_ as well. This means that the `HasKey` in the grammar should not be an _alternative_ (i.e. `|`) but it should be an optional (i.e. `[...]`) 
- The list of alternatives in the `annotationPropertyFrame` is probably wrong: The closing '}' should include all alternatives and not only the _annotations_ 
- Should `annotations <NT>2List` be equal to `<NT>AnnotatedList` where there are at least 2 `<NT>` in it? **NO** (see [Update](#update1))
  - If that is the case, then the AST is not correct, as the first example will be able to parse a list of `NT`s which may be accompanied by a list of annotations (i.e. the annotations are applied to the list), while in the second example, *each* `NT` can be accompanied by a list of annotations
  - If this is not the case, then I think that the `annotations` should be optional (i.e. `[annotations]`). As it is now all the annotations included in the types are required!
  - In my parser, I followed the second solution and converted all `annotations to optional
  - <a name="update1"></a>*Update*: It seems that `annotations <NT>2List` is not equal to `<NT>AnnotatedList`; The elements that use the first syntax (e.g. all the `misc`) define the annotations for the element itself and not its members, so there must be only a single annotation list (i.e. `annotations`) and not an annotation per member. However the `annottation` should have been inside square brakets as annotaitons are optional (tested in Protege)
  - Based on the syntax, a `conjunction` like `classIRI that { :ind1, :ind2 }` is not permitted, as the `classIRI` should be followed by the keyword `that` and then a `restriction` (with an option `not` between them). However `Protege` parses this syntax without an issue, while _Functional Syntax_ seems to allow that as well.
    - Also the `classIRI` does not seem to be correct either, as in place of a _class iri_ we can use other descriptions as well. E.g. `(classIRI1 or classIRI2) that { :ind1, :ind2 }` can be successfully parsed 
  - Based on its syntax tree an `ObjectPropertyFrame` should describe axioms of an `objectPropertyIRI`. However, OWL2 (at least based on Functional Syntax) seems to support arbitrary `ObjectPropertyExpression`. For example in Manchester syntax you cannot defined the following axiom:
  - The inverse of the object-property-A is sub-property of the inverse of object-property-B
  - In _Functional_ you can model this as:
    ```
    SubObjectPropertyOf(ObjectInverseOf(test-ont:ObjectProp2) ObjectInverseOf(test-ont:ObjectProp1))
    ```
  - Note: Protege is able to parse it and represent it on the UI (albeit, to the _Usage_ window only), but trying to save the ontology in Manchester format, the axiom will be discarder
- Based on the syntax a `DataFrame` can have at most one `EquivalentTo` data range. However:
  - Functional syntax supports equivalence with more than a single data range
  - Protege accepts more than a single data range and print them in a comma separated list when Manchester format is selected:
    ```
    EquivalentTo:
       owl:real,
       not xsd:negativeInteger
    ``` 
  - However when annotations are being attahced to the equivalent data ranges, Protege does not save them in the exproted (Manchester formatted) file. Also, Protege is not able to parse a (Manchester formatted) file, which contains annotations in this place:
    ```
    EquivalentTo:
       Annotations: rdfs:comment "Some comment" not xsd:negativeInteger
    ``` 
  


## Possible issues in Functional syntax specification

- Protege does not seem to support multiple data property expressions in a single `DataAllValuesFrom` class expression. For example it was not possible to parse:
  ```
  EquivalentClasses(test-ont:Test1 DataAllValuesFrom(test-ont:dataProp1 test-ont:dataPro2 xsd:integer))
  ```
  - However the the specifications define this constructor as:
    ```
    DataSomeValuesFrom := 'DataSAllValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
    ```
  - The same applies for `DataDomeValuesFrom` class expression


## Possible issues in Protege

### Manchester format

- It seems that even though Protege can parse _General class axioms_ in Manchester format, it does not save them back
  - Steps to reproduce:
    1. Add the line `EquivalentClasses: cls1 or cls2, cls3 and cls4` to the file
    2. Import the ontology. You can see the assertion in the _General class axioms_ window
    3. Make any change in order to trigger Protege to save the file
    4. Save the ontology in Manchester format
    5. Open the file with an editor; The assertion is not there
