# TODOs

- [X] Replace String with Text for the parser
- [ ] If I want to follow the format of the `language-java` parser, I may need to reconstruct my project:
  - Move module `Types` -> `Language.Owl2.[Manchester|Functional].Syntax`
  - Move module `OwlParser` -> `Language.Owl2.Functional.Parser`
  - Create module `Language.Owl2.Functional.Pretty`
- [ ] User `modern-uri` library for uri parsing. It supports URI (but not IRI) parsing through the megaparsec library

