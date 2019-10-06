{-# LANGUAGE QuasiQuotes #-}

module Language.OWL2.Examples.QuasiQ where

import Language.OWL2.TH.TH
import Language.OWL2.Types


-- | Reads an ontology document using the'ontologydoc' quosi-quoation
-- 
sampleOntologyDoc :: OntologyDocument
sampleOntologyDoc = [ontologydoc|

   Prefix(:=<http://www.semanticweb.org/konstantine/ontologies/2019/1/test-ont#>)
   Prefix(dc:=<http://purl.org/dc/elements/1.1/>)
   Prefix(test-ont:=<http://www.semanticweb.org/konstantine/ontologies/2019/1/test-ont#>)
   
   Ontology(<http://www.semanticweb.org/konstantine/ontologies/2019/1/test-ont>
)|]

