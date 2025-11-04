{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-001
module VHDL.Parser
  ( -- * Parsing
    parseVHDLFile
  , parseVHDLText
  , ParseError(..)
  ) where

import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Text.Megaparsec hiding (ParseError)
import VHDL.AST
import VHDL.Lexer
import VHDL.SourceLocation

-- | Parse error type
data ParseError = ParseError
  { parseErrorMessage :: Text
  , parseErrorLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

instance ToJSON ParseError

-- | Parse a VHDL file
parseVHDLFile :: FilePath -> IO (Either ParseError VHDLDesign)
parseVHDLFile path = do
  content <- TIO.readFile path
  pure $ parseVHDLText content path

-- | Parse VHDL text
parseVHDLText :: Text -> FilePath -> Either ParseError VHDLDesign
parseVHDLText content path =
  case runParser (vhdlDesign path) path content of
    Left err -> Left $ ParseError
      { parseErrorMessage = T.pack $ errorBundlePretty err
      , parseErrorLocation = convertSourcePos path (bundlePosState err)
      }
    Right design -> Right design

convertSourcePos :: FilePath -> PosState Text -> SourceLocation
convertSourcePos path posState =
  let pos = pstateSourcePos posState
  in SourceLocation
    { locFile = path
    , locLine = unPos (sourceLine pos)
    , locColumn = unPos (sourceColumn pos)
    }

-- | Parse complete VHDL design
vhdlDesign :: FilePath -> Parser VHDLDesign
vhdlDesign path = do
  sc  -- Consume initial whitespace/comments
  entities <- many (try entityDecl)
  architectures <- many (try architectureDecl)
  eof
  pure VHDLDesign
    { designEntities = entities
    , designArchitectures = architectures
    , designSourceFile = path
    }

-- | Parse entity declaration
entityDecl :: Parser Entity
entityDecl = do
  pos <- getSourcePos
  void $ keyword "entity"
  name <- identifier
  void $ keyword "is"
  generics <- option [] (try genericClause)
  ports <- option [] (try portClause)
  void $ keyword "end"
  void $ optional (keyword "entity")
  void $ optional identifier
  void semi
  pure Entity
    { entityName = name
    , entityGenerics = generics
    , entityPorts = ports
    , entityLocation = sourcePosToLocation pos
    }

-- | Parse generic clause
genericClause :: Parser [GenericDecl]
genericClause = do
  void $ keyword "generic"
  void $ symbol "("
  generics <- try genericDecl `sepBy` semi
  void $ symbol ")"
  void semi
  pure generics

-- | Parse single generic declaration
genericDecl :: Parser GenericDecl
genericDecl = do
  name <- identifier
  void colon
  typeName <- identifier
  defaultVal <- optional (symbol ":=" *> value)
  pure GenericDecl
    { genericName = name
    , genericType = typeName
    , genericDefault = defaultVal
    }

-- | Parse port clause
portClause :: Parser [PortDecl]
portClause = do
  void $ keyword "port"
  void $ symbol "("
  ports <- try portDecl `sepBy` semi
  void $ symbol ")"
  void semi
  pure ports

-- | Parse single port declaration
portDecl :: Parser PortDecl
portDecl = do
  name <- identifier
  void colon
  dir <- parseDirection
  typeName <- typeSpec
  pure PortDecl
    { portName = name
    , portDirection = dir
    , portType = typeName
    }

-- | Parse type specification (identifier optionally followed by parameters)
typeSpec :: Parser Text
typeSpec = do
  baseType <- identifier
  -- Optionally skip type parameters like (23 downto 0)
  _ <- optional (try $ parens (many (noneOf [')'])))
  pure baseType

-- | Parse port direction
parseDirection :: Parser PortDirection
parseDirection = choice
  [ try (keyword "inout") >> pure InOut
  , try (keyword "out") >> pure Output
  , keyword "in" >> pure Input
  ]

-- | Parse architecture declaration
architectureDecl :: Parser Architecture
architectureDecl = do
  pos <- getSourcePos
  void $ keyword "architecture"
  name <- identifier
  void $ keyword "of"
  entName <- identifier
  void $ keyword "is"
  -- Skip declarations section until "begin"
  _ <- skipManyTill anySingle (try $ lookAhead $ keyword "begin")
  void $ keyword "begin"
  components <- many (try componentInst)
  void $ keyword "end"
  void $ optional (keyword "architecture")
  void $ optional identifier
  void semi
  pure Architecture
    { archName = name
    , archEntityName = entName
    , archComponents = components
    , archLocation = sourcePosToLocation pos
    }

-- | Parse component instantiation
componentInst :: Parser ComponentInst
componentInst = do
  pos <- getSourcePos
  instName <- identifier
  void colon
  void $ optional (keyword "component")
  compName <- identifier
  gmap <- option [] (try genericMapClause)
  pmap <- option [] (try portMapClause)
  void semi
  pure ComponentInst
    { compInstName = instName
    , compComponentName = compName
    , compGenericMap = gmap
    , compPortMap = pmap
    , compLocation = sourcePosToLocation pos
    }

-- | Parse generic map
genericMapClause :: Parser [(Identifier, Value)]
genericMapClause = do
  void $ keyword "generic"
  void $ keyword "map"
  parens (association `sepBy` comma)

-- | Parse port map
portMapClause :: Parser [(Identifier, SignalName)]
portMapClause = do
  void $ keyword "port"
  void $ keyword "map"
  parens (signalAssociation `sepBy` comma)

-- | Parse generic association (name => value)
association :: Parser (Identifier, Value)
association = do
  name <- identifier
  void $ symbol "=>"
  val <- value
  pure (name, val)

-- | Parse port association (name => signal)
signalAssociation :: Parser (Identifier, SignalName)
signalAssociation = do
  name <- identifier
  void $ symbol "=>"
  signal <- identifier
  pure (name, signal)

-- | Parse value
value :: Parser Value
value = choice
  [ try (RealValue <$> double)
  , IntValue <$> integer
  , StringValue <$> stringLiteral
  , IdentifierValue <$> identifier
  ]

-- | Convert megaparsec SourcePos to our SourceLocation
sourcePosToLocation :: SourcePos -> SourceLocation
sourcePosToLocation pos = SourceLocation
  { locFile = sourceName pos
  , locLine = unPos (sourceLine pos)
  , locColumn = unPos (sourceColumn pos)
  }
