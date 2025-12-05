{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-001
module VHDL.Lexer
  ( -- * Lexer
    Parser
  , sc
  , lexeme
  , symbol
  , keyword
  , identifier
  , integer
  , double
  , stringLiteral
  , basedLiteral
  , parens
  , semi
  , colon
  , comma
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type
-- Contract: spellcraft-adc-001 Section: Interface
type Parser = Parsec Void Text

-- | Space consumer (whitespace and comments)
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "/*" "*/")

-- | Parse a lexeme (token followed by whitespace)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a symbol (string followed by whitespace)
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a keyword (case-insensitive)
keyword :: Text -> Parser Text
keyword kw = lexeme $ do
  s <- takeWhile1P Nothing isAlphaNumOrUnderscore
  if T.toLower s == T.toLower kw
    then pure s
    else fail $ "Expected keyword " <> T.unpack kw

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore c = c == '_' || c == '.' || isAlphaNum c

-- | Parse an identifier
identifier :: Parser Text
identifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  -- Check it's not a reserved keyword
  if T.toLower ident `elem` reservedKeywords
    then fail $ "Keyword " <> T.unpack ident <> " cannot be used as identifier"
    else pure ident

reservedKeywords :: [Text]
reservedKeywords =
  [ "entity", "architecture", "component", "port", "generic"
  , "map", "is", "of", "begin", "end", "signal", "in", "out", "inout"
  , "to", "downto"
  , "if", "then", "elsif", "else"
  , "case", "when", "others"
  , "loop", "for", "while"
  , "process", "wait", "null"
  ]

-- | Parse an integer literal (supporting negative numbers)
integer :: Parser Integer
integer = lexeme (L.signed sc L.decimal)

-- | Parse a floating-point literal (supporting negative numbers)
double :: Parser Double
double = lexeme (L.signed sc L.float)

-- | Parse a string literal
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  _ <- char '"'
  s <- many (noneOf ['\"'])
  _ <- char '"'
  pure (T.pack s)

-- | Parse a VHDL based string literal (hex, binary, octal)
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- Examples: x"BEEF", X"CAFE", b"1010", o"777"
basedLiteral :: Parser Text
basedLiteral = lexeme $ do
  base <- oneOf ['x', 'X', 'b', 'B', 'o', 'O']
  _ <- char '"'
  digits <- many (noneOf ['\"'])
  _ <- char '"'
  pure $ T.pack (base : '"' : digits ++ "\"")

-- | Parse within parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse a semicolon
semi :: Parser Text
semi = symbol ";"

-- | Parse a colon
colon :: Parser Text
colon = symbol ":"

-- | Parse a comma
comma :: Parser Text
comma = symbol ","
