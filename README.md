# OWL-Parser

A Haskell parser for the [Manchester Syntax](https://www.w3.org/TR/owl2-manchester-syntax) of OWL 2 Web Ontology Language

This a work in progress and by no means complete


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
- Should `annotations <NT>2List` be equal to `<NT>AnnotatedList` where there are at least 2 `<NT>` in it? If that is the case, ten the AST is not correct, as the first example will be able to parse a list of `NT`s which may be accompanied by a list of annotations (i.e. the annotations are applied to the list), while in the second example, *each* `NT` can be accompanied by a list of annotations
