{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-001
-- ADC-IMPLEMENTS: spellcraft-adc-008
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
-- Contract: spellcraft-adc-008 Section: Implementation
-- Simplified: Parse library/use clauses, then all design units (entities + architectures)
-- | Helper type for parsing context items (libraries and uses interleaved)
data ContextItem = LibItem LibraryDeclaration | UseItem UseClause

vhdlDesign :: FilePath -> Parser VHDLDesign
vhdlDesign path = do
  sc  -- Consume initial whitespace/comments
  -- Parse library and use clauses (can be interleaved in VHDL-2008)
  contextItems <- many (try parseContextItem)
  let libraries = [lib | LibItem lib <- contextItems]
  let uses = [use | UseItem use <- contextItems]
  -- Parse all design units (entities and architectures intermixed)
  entities <- many (try entityDecl)
  sc  -- Ensure whitespace consumed before architectures
  architectures <- many (try architectureDecl)
  eof
  pure VHDLDesign
    { designLibraries = libraries
    , designUses = uses
    , designEntities = entities
    , designArchitectures = architectures
    , designSourceFile = path
    }

-- | Parse a single context item (library or use clause)
parseContextItem :: Parser ContextItem
parseContextItem = try (LibItem <$> parseLibraryDeclaration) <|> try (UseItem <$> parseUseClause)

-- | Parse library declaration (e.g., "library work;")
-- Contract: spellcraft-adc-008 Section: Implementation
parseLibraryDeclaration :: Parser LibraryDeclaration
parseLibraryDeclaration = do
  pos <- getSourcePos
  void $ keyword "library"
  name <- identifier
  void $ symbol ";"
  sc
  pure LibraryDeclaration
    { libName = name
    , libSourceLoc = sourcePosToLocation pos
    }

-- | Parse use clause (e.g., "use work.all;")
-- Contract: spellcraft-adc-008 Section: Implementation
parseUseClause :: Parser UseClause
parseUseClause = do
  pos <- getSourcePos
  void $ keyword "use"
  -- Use clause can have dots: work.all, ieee.std_logic_1164.all
  lib <- identifier
  void $ symbol "."
  pkgParts <- identifier `sepBy1` (symbol ".")
  void $ symbol ";"
  sc
  -- For multi-part package names like ieee.std_logic_1164.all,
  -- join them all together to preserve the full path
  let pkg = T.intercalate "." pkgParts
  pure UseClause
    { useLibrary = lib
    , usePackage = pkg
    , useSourceLoc = sourcePosToLocation pos
    }

-- | Parse entity declaration
entityDecl :: Parser Entity
entityDecl = do
  sc  -- Consume leading whitespace/comments
  pos <- getSourcePos
  void $ keyword "entity"
  name <- identifier
  void $ keyword "is"
  generics <- option [] (try genericClause)
  ports <- option [] (try portClause)
  void $ keyword "end"
  void $ optional (try $ keyword "entity")
  void $ optional (try identifier)
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
  sc  -- Explicitly consume whitespace after opening paren
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
  sc  -- Consume leading whitespace/comments
  pos <- getSourcePos
  void $ keyword "architecture"
  name <- identifier
  void $ keyword "of"
  entName <- identifier
  void $ keyword "is"
  -- Skip declarations section until "begin"
  _ <- skipManyTill anySingle (try $ lookAhead $ keyword "begin")
  void $ keyword "begin"
  -- Parse component instantiations (skip processes and other constructs)
  components <- many (try componentInst)
  -- Skip body until terminating "end architecture".
  -- NOTE: This currently only supports the explicit "end architecture" form,
  -- not the shorthand "end <name>;" form. This is a known limitation.
  _ <- skipManyTill anySingle
        (try $ lookAhead $ keyword "end" >> keyword "architecture")
  void $ keyword "end"
  void $ optional (try $ keyword "architecture")
  void $ optional (try identifier)
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
