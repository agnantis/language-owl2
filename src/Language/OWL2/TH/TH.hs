{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE  QuasiQuotes #-}
--test :: String
--test = [expr|test some|]

module Language.OWL2.TH.TH where

import           Data.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.OWL2.Import                     ( Text )
import qualified Language.OWL2.Import          as T
import           Language.OWL2.Types
import qualified Language.OWL2.Functional.Parser as FP
import qualified Language.OWL2.Manchester.Parser as MP

------------------
-- Quasi Quotes --
------------------

-- | 'ontologydocf' quasi-quoatiation can be used for parsing ontology documents
-- in functional format
type SourcePos = (FilePath, Int, Int)

ontologydoc :: QuasiQuoter
ontologydoc = QuasiQuoter 
  { quoteExp = quoteExpOntologyDoc FP.parseOntology
  , quotePat = error "Usage as a pettern is not supported"
  , quoteDec = error "Usage as a declaration is not supported"
  , quoteType = error "Usage as a type is not supported"
  }

ontologydocm :: QuasiQuoter
ontologydocm = QuasiQuoter 
  { quoteExp = quoteExpOntologyDoc MP.parseOntology
  , quotePat = error "Usage as a pettern is not supported"
  , quoteDec = error "Usage as a declaration is not supported"
  , quoteType = error "Usage as a type is not supported"
  }

quoteExpOntologyDoc :: (SourcePos -> Text -> Either Text OntologyDocument) -> String -> Q Exp
quoteExpOntologyDoc parser txt = do
  Loc{..} <- location
  ontologyD <- pOntologyDoc parser (loc_filename, fst loc_start, snd loc_start) txt
  dataToExpQ (fmap liftText . cast) ontologyD


pOntologyDoc :: Monad m => (SourcePos -> Text -> Either Text OntologyDocument) -> SourcePos -> String -> m OntologyDocument
pOntologyDoc parser pos txt =
  case parser pos (T.pack txt) of
    Left errMsg -> fail $ T.unpack errMsg
    Right ontDoc -> return ontDoc

-- QQ utilities

-- | Lifts a text datatype to Q
liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

