module OwlParser where

-- import Data.Text hiding (empty)
-- import Data.Char (digitToInt)

import Prelude hiding (exponent)
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Parses white space and line comments
sc :: Parser ()
sc = L.space space1 lineComment empty
  where lineComment = L.skipLineComment "#"

-- | Parses the actual lexeme and then any remaining space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

iriParens :: Parser a -> Parser a
iriParens = between (symbol "<") (symbol (">"))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol (")"))

keywords :: [String]
keywords = ["integer", "decimal", "float", "string", "Datatype", "Class", "ObjectProperty", "DataProperty", "AnnotationProperty", "NamedInvividual"]

ontologyKeywords :: [String]
ontologyKeywords = ["Annotations:", "Prefix:", "Ontology:", "Import:"]

propertyKeywords :: [String]
propertyKeywords = ["inverse", "or", "and", "not", "length", "minLength", "maxLength", "pattern", "<=", "<", ">=", ">"]

-- | Parses the symbol and then any remaining space
--
-- >>> parseTest (symbol "a symbol") "a symbol  "
-- "a symbol"
symbol :: String -> Parser String
symbol = L.symbol sc

-- | It parses zero
--
-- >>> parseTest zero "0"
-- '0'
zero :: Parser Char
zero = char '0'

-- | It parses non positive integer
--
-- >>> parseTest nonZero "1"
-- '1'
nonZero :: Parser Char
nonZero = notFollowedBy zero *> digitChar

digit :: Parser Char
digit = zero <|> nonZero

digits :: Parser String
digits = some digit

-- | It parses positive integer
--
-- >>> parseTest positiveInteger "13"
-- 13
positiveInteger :: Parser Integer
positiveInteger = do
  fd  <- nonZero
  rem <- many digit
  return $ read (fd : rem)

-- | It parses non negativeinteger
--
-- >>> parseTest nonNegativeInteger "13"
-- 13
-- >>> parseTest nonNegativeInteger "0"
-- 0
nonNegativeInteger :: Parser Integer
nonNegativeInteger = (0 <$ zero) <|> positiveInteger

-- | It may parse a sign or no sign at all
--
-- >>> parseTest sign "+"
-- "" 
-- >>> parseTest sign "-"
-- "-"
-- >>> parseTest sign ""
-- ""
sign :: Parser String
sign = do
  mSign <- optional $ "" <$ symbol "+" <|> symbol "-"
  return $ fromMaybe "" mSign

fullIRI :: Parser String
fullIRI = iriParens undefined

prefixName :: Parser String
prefixName = undefined

abbreviatedIRI :: Parser String
abbreviatedIRI = undefined

simpleIRI :: Parser String
simpleIRI = undefined

iri :: Parser String
iri = fullIRI <|> abbreviatedIRI <|> simpleIRI

classIRI :: Parser String
classIRI = iri

dataType :: Parser String
dataType = dataTypeIRI <|> symbol "integer" <|> symbol "decimal" <|> symbol "float" <|> symbol "string"

dataTypeIRI :: Parser String
dataTypeIRI = iri

objectPropertyIRI :: Parser String
objectPropertyIRI = iri

dataPropertyIRI :: Parser String
dataPropertyIRI = iri

annotationPropertyIRI :: Parser String
annotationPropertyIRI = iri

individual :: Parser String
individual = individualIRI <|> nodeID

individualIRI :: Parser String
individualIRI = iri

nodeID :: Parser String
nodeID = undefined

literal :: Parser String
literal = typedLiteral <|> stringLiteralNoLanguage <|> stringLiteralWithLanguage <|> integerLiteral <|> decimalLiteral <|> (fmap show floatingPointLiteral)

typedLiteral :: Parser String
typedLiteral = do
  val <- lexicalValue 
  symbol "^^"
  dt <- dataType
  return "<typedLiteral>"


stringLiteralNoLanguage :: Parser String
stringLiteralNoLanguage = quotedString

stringLiteralWithLanguage :: Parser String
stringLiteralWithLanguage = do
   qString <- quotedString
   lTag <- languageTag
   return "<stringLiteralWithLanguage>"

languageTag :: Parser String
languageTag = symbol "@" >> return "<languageTag>" -- (U+40) followed a nonempty sequence of characters matching the langtag production from [BCP 47]

lexicalValue :: Parser String
lexicalValue = quotedString

-- | It parses a string enclosed in double quotes
--
-- >>> parseTest quotedString "\"this is a quoted string\""
-- "this is a quoted string"
-- >>> parseTest quotedString "\"this is \\\"test \\\" message\"" 
-- "this is \\\"test \\\" message"
quotedString :: Parser String
quotedString = do
    char '\"'
    strings <- many chars
    char '\"'
    return $ concat strings
  where
    chars = fmap return nonEscape <|> escape
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f" -- all the characters that can be escaped
    escape = do
      d <- char '\\'
      c <- oneOf "\\\"0nrvtbf"
      return [d,c]


data FloatPoint = Float Double (Maybe Exponent)
newtype Exponent = Exponent Integer

instance Show FloatPoint where
  show (Float n me) = show n ++ (maybe "" show me)
instance Show Exponent where
  show (Exponent i) = 'e':((if i > 0 then "+" else "") ++ show i)

-- | It parses folating point numbers.
-- Valid formats:
--   * 12F
--   * .3f
--   * 12.3f
--   * -12.3F
--
-- >>> parseTest floatingPointLiteral "12F"
-- 12.0
-- >>> parseTest floatingPointLiteral "12.3f"
-- 12.3
-- >>> parseTest floatingPointLiteral "-12.332F"
-- -12.332
-- >>> parseTest floatingPointLiteral ".3f"
-- 0.3
-- >>> parseTest floatingPointLiteral ".3e10f"
-- 0.3e+10
-- >>> parseTest floatingPointLiteral "-12.3e-10F"
-- -12.3e-10
floatingPointLiteral :: Parser FloatPoint
floatingPointLiteral = do
  sgn <- sign
  dgts <- dig1 <|> dig2
  mExp <- optional exponent
  symbol "f" <|> symbol "F"
  return $ Float (read (sgn ++ dgts)) mExp
 where
  dig1 = do
    dg' <- digits
    mDec <- optional $ do
         symbol "."
         dg <- digits
         return $ '.':dg
    let dc = fromMaybe "" mDec
    return $ dg' ++ dc
  dig2 = do
    _ <- symbol "."
    dgts <- digits
    return $ "0." ++ dgts
    
  
-- | It parses an exponent
--
-- >>> parseTest exponent "e10"
-- e+10
-- >>> parseTest exponent "E10"
-- e+10
-- >>> parseTest exponent "e+10"
-- e+10
-- >>> parseTest exponent "E+10"
-- e+10
-- >>> parseTest exponent "e-10"
-- e-10
-- >>> parseTest exponent "E-10"
-- e-10
exponent :: Parser Exponent
exponent = do
  symb <- symbol "e" <|> symbol "E"
  ms <- sign
  dgts <- digits
  return . Exponent . read $ ms ++ dgts

decimalLiteral :: Parser String
decimalLiteral = do
  mSign <- sign
  dig1 <- digits 
  dig2 <- symbol "." >> digits
  return "<decimalLiteral>"

integerLiteral :: Parser String
integerLiteral = do
  mSign <- sign
  digs <- digits
  return "<integerLiteral>"

entity :: Parser String
entity = choice $ fmap (uncurry classParser) alts
  where classParser :: String -> Parser String -> Parser String
        classParser s p = do
          smb <- symbol s
          name <- p
          return $ "<" ++ s ++ ">"
        alts :: [(String, Parser String)] 
        alts = [ ("Datatype", dataType)
               , ("Class", classIRI)
               , ("ObjectProperty", objectPropertyIRI)
               , ("DataProperty",dataPropertyIRI)
               , ("AnnotationProperty", annotationPropertyIRI)
               , ("NamedIndividual", individualIRI)
               ]

