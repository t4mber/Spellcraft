{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-001
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- ADC-IMPLEMENTS: spellcraft-adc-012
-- ADC-IMPLEMENTS: spellcraft-adc-028
module VHDL.Parser
  ( -- * Parsing
    parseVHDLFile
  , parseVHDLText
  , ParseError(..)
  ) where

import Control.Monad (void, when)
import Data.Aeson (ToJSON)
import Data.Char (isAlpha)
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char (char)
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

-- ADC-IMPLEMENTS: spellcraft-adc-025
-- | Helper type for parsing design units (entities and architectures in any order)
data DesignUnit = EntityUnit Entity | ArchUnit Architecture

vhdlDesign :: FilePath -> Parser VHDLDesign
vhdlDesign path = do
  sc  -- Consume initial whitespace/comments
  -- Parse library and use clauses (can be interleaved in VHDL-2008)
  contextItems <- many (try parseContextItem)
  let libraries = [lib | LibItem lib <- contextItems]
  let uses = [use | UseItem use <- contextItems]
  -- ADC-IMPLEMENTS: spellcraft-adc-025
  -- Parse design units in any order (entities and architectures can be intermixed)
  -- This allows standalone architecture files without entities
  designUnits <- many (try parseDesignUnit)
  let entities = [e | EntityUnit e <- designUnits]
  let architectures = [a | ArchUnit a <- designUnits]
  pure ()
  eof
  pure ()
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

-- ADC-IMPLEMENTS: spellcraft-adc-025
-- | Parse a single design unit (entity or architecture in any order)
-- This allows standalone architecture files without entities
parseDesignUnit :: Parser DesignUnit
parseDesignUnit = do
  result <- try (EntityUnit <$> entityDecl)
        <|> try (ArchUnit <$> architectureDecl)
  pure result

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

-- | Parse signal declaration with support for multiple signal names
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
-- ADC-IMPLEMENTS: spellcraft-adc-018
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- Supports: signal a, b, c : type := init;
signalDecl :: Parser [SignalDecl]
signalDecl = do
  pos <- getSourcePos
  void $ keyword "signal"
  -- Parse comma-separated list of signal names
  names <- identifier `sepBy1` comma
  void colon
  sigType <- typeSpec
  -- Check for optional initialization: := expression
  initValue <- optional $ try $ do
    void $ symbol ":="
    parseExpression
  void semi
  -- Return a SignalDecl for each name
  pure [ SignalDecl
           { sigDeclName = name
           , sigDeclType = sigType
           , sigDeclInitValue = initValue
           , sigDeclLocation = sourcePosToLocation pos
           }
       | name <- names
       ]

-- | Parse process statement
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
-- Enhanced: spellcraft-adc-013 Section: Parser Extensions
processStmt :: Parser ArchStatement
processStmt = do
  sc  -- ADC-015: Consume leading whitespace/comments
  pos <- getSourcePos
  -- Optional process label
  pName <- optional $ try (identifier <* colon)
  void $ keyword "process"
  -- Parse sensitivity list
  sensitivity <- option [] $ parens (identifier `sepBy` comma)
  
  -- Skip declarations for now
  _ <- optional $ try $ do
    pure ()
    _ <- skipManyTill anySingle (try $ lookAhead $ keyword "begin")
    pure ()
    pure ()
  pure ()
  void $ keyword "begin"
  pure ()
  -- Debug: try to peek at next token
  _ <- optional $ try $ do
    pos <- getSourcePos
    
    nextTok <- optional $ try $ lookAhead identifier
    
    -- Try to manually check if "end" matches
    endMatches <- optional $ try $ lookAhead (keyword "end")
    pure ()
  -- Parse sequential statements per ADC-013
  statements <- parseSequentialStatements
  pure ()
  void $ keyword "end"
  void $ keyword "process"
  void semi
  pure ProcessStmt
    { procName = pName
    , procSensitivity = sensitivity
    , procStatements = statements  -- Now parsing statements per ADC-013
    , procLocation = sourcePosToLocation pos
    }

-- | Parse concurrent signal assignment
-- Enhanced: spellcraft-adc-013 Section: Expression Parsing
-- ADC-IMPLEMENTS: spellcraft-adc-015
-- ADC-IMPLEMENTS: spellcraft-adc-022
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- Now supports conditional signal assignments: target <= value when cond else default;
concurrentAssignment :: Parser ArchStatement
concurrentAssignment = do
  sc  -- ADC-015: Consume leading whitespace/comments
  pure ()
  pos <- getSourcePos

  -- Use parsePrimaryExpr to avoid consuming <= as comparison operator
  target <- parsePrimaryExpr  -- Changed from 'identifier' to support indexed assignments (ADC-022)

  void $ symbol "<="
  pure ()
  -- Parse expression per ADC-013
  -- ADC-027: Support conditional expressions (when ... else)
  expr <- parseConditionalExpr

  void semi
  pure ()
  pure ConcurrentAssignment
    { concTarget = target  -- Now an Expression
    , concExpr = expr
    , concLocation = sourcePosToLocation pos
    }

-- | Parse conditional expression (value when condition else default)
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- Parses: expr1 when cond1 else expr2 [when cond2 else expr3]...
parseConditionalExpr :: Parser Expression
parseConditionalExpr = do
  firstExpr <- parseExpression
  -- Check for 'when' clause
  mWhen <- optional $ try $ do
    void $ keyword "when"
    cond <- parseExpression
    void $ keyword "else"
    -- Recursively parse the else part (can be another conditional)
    elseExpr <- parseConditionalExpr
    pure (cond, elseExpr)
  case mWhen of
    Nothing -> pure firstExpr  -- Simple assignment, no condition
    Just (cond, elseExpr) -> pure $ ConditionalExpr firstExpr cond elseExpr

-- | Parse architecture-level statement (process, concurrent, component, or generate)
-- ADC-IMPLEMENTS: spellcraft-adc-028
archStatement :: Parser ArchStatement
archStatement = choice
  [ try processStmt
  , try generateStmt  -- ADC-028: Generate statements
  , try (ComponentInstStmt <$> componentInst)
  , try concurrentAssignment
  ]

-- | Parse generate statement (for-generate or if-generate)
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- IEEE 1076-2008 Section 11.8
-- Patterns:
--   gen_label: for i in 0 to N-1 generate ... end generate [gen_label];
--   gen_label: if CONDITION generate ... end generate [gen_label];
generateStmt :: Parser ArchStatement
generateStmt = do
  sc  -- Consume leading whitespace/comments
  pos <- getSourcePos
  -- Generate statements require a label
  label <- identifier
  void colon
  -- Parse either for-generate or if-generate scheme
  scheme <- choice
    [ try parseForGenerateScheme
    , parseIfGenerateScheme
    ]
  void $ keyword "generate"
  -- Parse nested architecture statements (recursive)
  statements <- many (try archStatement)
  void $ keyword "end"
  void $ keyword "generate"
  -- Optional closing label
  void $ optional (try identifier)
  void semi
  pure $ GenerateStmt GenerateStatement
    { genLabel = label
    , genScheme = scheme
    , genStatements = statements
    , genLocation = sourcePosToLocation pos
    }

-- | Parse for-generate scheme: for i in start to/downto end
-- ADC-IMPLEMENTS: spellcraft-adc-028
parseForGenerateScheme :: Parser GenerateScheme
parseForGenerateScheme = do
  void $ keyword "for"
  loopVar <- identifier
  void $ keyword "in"
  startExpr <- parseExpression
  -- Parse direction (to or downto)
  dir <- try (keyword "downto" >> pure DownTo) <|> (keyword "to" >> pure To)
  endExpr <- parseExpression
  pure $ ForGenerate loopVar startExpr endExpr dir

-- | Parse if-generate scheme: if condition
-- ADC-IMPLEMENTS: spellcraft-adc-028
parseIfGenerateScheme :: Parser GenerateScheme
parseIfGenerateScheme = do
  void $ keyword "if"
  condition <- parseExpression
  pure $ IfGenerate condition

-- | Skip a declaration that we don't parse (constant, type, etc.)
-- ADC-IMPLEMENTS: spellcraft-adc-025
skipDeclaration :: Parser ()
skipDeclaration = do
  -- Try to match and skip: constant, type, subtype, component, function, procedure, etc.
  choice
    [ try (keyword "constant" >> skipTo (void semi))
    , try $ do
        -- Handle record types specially: type name is record ... end record;
        void $ keyword "type"
        void identifier  -- type name
        void $ keyword "is"
        -- Check if this is a record type
        choice
          [ try $ do
              void $ keyword "record"
              -- Skip until "end record;"
              skipTo (keyword "end" >> keyword "record" >> void semi)
          , skipTo (void semi)  -- Other type declarations end with semicolon
          ]
    , try (keyword "subtype" >> skipTo (void semi))
    , try (keyword "component" >> skipTo (keyword "end" >> keyword "component" >> void semi))
    , try (keyword "function" >> skipTo (keyword "end" >> optional (keyword "function") >> void semi))
    , try (keyword "procedure" >> skipTo (keyword "end" >> optional (keyword "procedure") >> void semi))
    ]
  where
    skipTo p = skipManyTill anySingle (try $ lookAhead p) >> p

-- | Parse declaration section (signals, constants, etc.)
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- Updated to handle multi-signal declarations returning lists
parseDeclarations :: Parser [SignalDecl]
parseDeclarations = do
  decls <- many $ choice
    [ try (Just <$> signalDecl)  -- Returns [SignalDecl]
    , try (skipDeclaration >> pure Nothing)
    ]
  pure $ concat $ catMaybes decls  -- Flatten the list of lists

-- | Parse architecture declaration
architectureDecl :: Parser Architecture
architectureDecl = do
  sc  -- Consume leading whitespace/comments
  pos <- getSourcePos
  pure ()
  void $ keyword "architecture"
  pure ()
  name <- identifier
  
  void $ keyword "of"
  pure ()
  entName <- identifier
  
  void $ keyword "is"
  pure ()
  -- ADC-IMPLEMENTS: spellcraft-adc-015
  -- Parse declarations section (signals, constants, types, etc.)
  signals <- parseDeclarations
  -- We should now be at "begin"
  void $ keyword "begin"
  pure ()
  posAfterBegin <- getSourcePos
  
  -- Parse architecture body statements
  -- This collects processes, concurrent assignments, and component instantiations
  statements <- many (try archStatement)
  posAfterStmts <- getSourcePos
  
  -- Now consume the "end architecture" terminator
  -- Supports both "end architecture;" and "end architecture <name>;"
  void $ keyword "end"
  pure ()
  void $ optional (try $ keyword "architecture")
  void $ optional (try identifier)
  void semi
  pure ()
  pure Architecture
    { archName = name
    , archEntityName = entName
    , archSignals = signals
    , archStatements = statements
    , archComponents = [c | ComponentInstStmt c <- statements]
    , archLocation = sourcePosToLocation pos
    }

-- | Parse component instantiation
-- ADC-IMPLEMENTS: spellcraft-adc-015
-- ADC-IMPLEMENTS: spellcraft-adc-025
-- Supports both component instantiation and direct entity instantiation:
--   inst : component comp_name ...
--   inst : comp_name ...
--   inst : entity work.entity_name ...
componentInst :: Parser ComponentInst
componentInst = do
  sc  -- ADC-015: Consume leading whitespace/comments
  pos <- getSourcePos
  
  instName <- identifier
  
  void colon
  pure ()
  -- Handle three forms:
  -- 1. "component comp_name"
  -- 2. "entity lib.entity_name"
  -- 3. "comp_name" (bare)
  -- IMPORTANT: Each alternative must use 'try' because 'keyword' consumes input before checking
  compName <- choice
    [ try (keyword "component" >> identifier)
    , try $ do
         void $ keyword "entity"
         pure ()
         lib <- identifier  -- library name (usually "work")
         
         void $ symbol "."
         pure ()
         name <- identifier  -- entity name
         
         pure (lib <> "." <> name)  -- Combine as "work.entity_name"
    , identifier  -- bare component name
    ]
  
  gmap <- option [] (try genericMapClause)
  pmap <- option [] (try portMapClause)
  void semi
  pure ()
  pure ComponentInst
    { compInstName = instName
    , compComponentName = compName
    , compGenericMap = gmap
    , compPortMap = pmap
    , compLocation = sourcePosToLocation pos
    }

-- | Parse generic map
-- ADC-IMPLEMENTS: spellcraft-adc-020
genericMapClause :: Parser [(Identifier, Expression)]
genericMapClause = do
  void $ keyword "generic"
  void $ keyword "map"
  parens (association `sepBy` comma)

-- | Parse port map
-- ADC-IMPLEMENTS: spellcraft-adc-021
-- ADC-IMPLEMENTS: spellcraft-adc-025
-- ADC-IMPLEMENTS: spellcraft-adc-027
portMapClause :: Parser [(Expression, Expression)]
portMapClause = do
  void $ keyword "port"
  void $ keyword "map"
  pure ()
  result <- parens (portAssociation `sepBy` comma)
  pure result

-- | Parse generic association (name => expression)
-- ADC-IMPLEMENTS: spellcraft-adc-020
-- Changed from Value to Expression to support complex generic parameters
association :: Parser (Identifier, Expression)
association = do
  name <- identifier
  void $ symbol "=>"
  expr <- parseExpression  -- Changed from 'value' to 'parseExpression'
  pure (name, expr)

-- | Parse port association (formal => actual)
-- ADC-IMPLEMENTS: spellcraft-adc-021
-- ADC-IMPLEMENTS: spellcraft-adc-025
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- Changed: Both formal and actual are now Expression to support:
--   - Indexed formals: a(0) => sig, result(3) => data
--   - Sliced formals: data(7 downto 0) => bus
--   - Complex actuals: clk => system_clk, data => unsigned(input)
portAssociation :: Parser (Expression, Expression)
portAssociation = do
  -- Parse formal (left side) - can be identifier, indexed, or sliced
  formal <- parsePrimaryExpr

  void $ symbol "=>"
  pure ()
  -- Parse actual (right side) - can be any expression
  actual <- parseExpression

  pure (formal, actual)

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

-- =============================================================================
-- ADC-013 Process Body Parsing Implementation
-- =============================================================================

-- | Parse a single sequential statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseSequentialStatement :: Parser Statement
parseSequentialStatement = do
  sc  -- Consume leading whitespace
  pure ()
  posAfterSc <- getSourcePos
  
  -- Try to parse different statement types
  choice
    [ try parseSignalAssignment
    , try parseVariableAssignment
    , try parseIfStatement
    , try parseCaseStatement
    , try parseLoopStatement
    , try parseWaitStatement
    , try parseNullStatement
    ]

-- | Parse sequential statements inside process
-- Contract: spellcraft-adc-013 Section: Parser Extensions
-- REFACTORED: Use manyTill for clearer termination logic
parseSequentialStatements :: Parser [Statement]
parseSequentialStatements = do
  -- Parse statements until we see "end process"
  -- Using manyTill with explicit terminator is clearer than recursive go
  stmts <- manyTill
    (parseSequentialStatement)
    (lookAhead $ try $ keyword "end" >> keyword "process")

  pure stmts

-- | Parse signal assignment statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
-- ADC-IMPLEMENTS: spellcraft-adc-022
parseSignalAssignment :: Parser Statement
parseSignalAssignment = do
  pos <- getSourcePos
  -- Use parsePrimaryExpr to avoid consuming <= as comparison operator
  target <- parsePrimaryExpr  -- Changed from 'identifier' to support indexed assignments (ADC-022)
  void $ symbol "<="
  expr <- parseExpression
  void semi
  pure SignalAssignment
    { stmtTarget = target  -- Now an Expression
    , stmtExpr = expr
    , stmtLocation = sourcePosToLocation pos
    }

-- | Parse variable assignment statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
-- ADC-IMPLEMENTS: spellcraft-adc-022
parseVariableAssignment :: Parser Statement
parseVariableAssignment = do
  pos <- getSourcePos
  -- Use parsePrimaryExpr to avoid consuming := as operator
  target <- parsePrimaryExpr  -- Changed from 'identifier' to support indexed assignments (ADC-022)
  void $ symbol ":="
  expr <- parseExpression
  void semi
  pure VariableAssignment
    { stmtVarTarget = target  -- Now an Expression
    , stmtVarExpr = expr
    , stmtLocation = sourcePosToLocation pos
    }

-- | Parse if statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseIfStatement :: Parser Statement
parseIfStatement = do
  pos <- getSourcePos
  void $ keyword "if"
  condition <- parseExpression
  void $ keyword "then"
  -- Parse statements until we see elsif/else/end
  thenStmts <- many (try (notFollowedBy endOrElse >> parseSequentialStatement))
  -- Parse elsif clauses
  elsifs <- many $ try $ do
    void $ keyword "elsif"
    cond <- parseExpression
    void $ keyword "then"
    stmts <- many (try (notFollowedBy endOrElse >> parseSequentialStatement))
    pure (cond, stmts)
  -- Parse optional else clause
  elseStmts <- option [] $ try $ do
    void $ keyword "else"
    many (try (notFollowedBy (keyword "end") >> parseSequentialStatement))
  void $ keyword "end"
  void $ keyword "if"
  void semi
  pure IfStatement
    { stmtCondition = condition
    , stmtThen = thenStmts
    , stmtElsifs = elsifs
    , stmtElse = elseStmts
    , stmtLocation = sourcePosToLocation pos
    }
  where
    endOrElse = choice [keyword "elsif", keyword "else", keyword "end"]

-- | Parse case statement
-- ADC-IMPLEMENTS: spellcraft-adc-024
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- Fixed: Use parseLiteral instead of parseExpression for when clause choices
-- Added: Support for OR choices (when "001" | "010" => ...)
parseCaseStatement :: Parser Statement
parseCaseStatement = do
  pos <- getSourcePos
  void $ keyword "case"
  expr <- parseExpression
  void $ keyword "is"
  whenClauses <- many (try parseWhenClause)
  void $ keyword "end"
  void $ keyword "case"
  void semi
  pure CaseStatement
    { stmtCaseExpr = expr
    , stmtWhenClauses = whenClauses
    , stmtLocation = sourcePosToLocation pos
    }
  where
    parseWhenClause = do
      void $ keyword "when"
      -- ADC-027: Parse choice list (supports | OR syntax)
      -- Examples: when "001" | "010" => ..., when others => ...
      choices <- parseCaseChoice `sepBy1` (symbol "|")
      -- Combine multiple choices into an Aggregate for now
      let choiceExpr = case choices of
            [single] -> single
            multiple -> Aggregate multiple  -- Use Aggregate to represent multiple choices
      void $ symbol "=>"
      stmts <- many (try (notFollowedBy nextWhenOrEnd >> parseSequentialStatement))
      pure (choiceExpr, stmts)
    -- Parse a single case choice
    parseCaseChoice = try parseLiteral
          <|> (try (keyword "others") >> pure (IdentifierExpr "others"))
          <|> (IdentifierExpr <$> identifier)
    nextWhenOrEnd = choice [keyword "when", keyword "end"]

-- | Parse loop statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseLoopStatement :: Parser Statement
parseLoopStatement = do
  pos <- getSourcePos
  -- Optional loop label
  loopVar <- optional $ try $ do
    void $ keyword "for"
    var <- identifier
    void $ keyword "in"
    pure var
  -- Parse range if it's a for loop
  range <- case loopVar of
    Just _ -> do
      start <- parseExpression
      -- ADC-027: Capture direction for downto loop ranges
      -- Use try on "downto" to allow backtracking when matching "to"
      dir <- try (keyword "downto" >> pure DownTo) <|> (keyword "to" >> pure To)
      end <- parseExpression
      pure $ Just (start, end, dir)
    Nothing -> do
      void $ keyword "loop"
      pure Nothing
  -- If we have a for loop, consume the "loop" keyword
  when (isJust loopVar) $ void $ keyword "loop"
  body <- many (try (notFollowedBy (keyword "end") >> parseSequentialStatement))
  void $ keyword "end"
  void $ keyword "loop"
  void semi
  pure LoopStatement
    { stmtLoopVar = loopVar
    , stmtLoopRange = range
    , stmtLoopBody = body
    , stmtLocation = sourcePosToLocation pos
    }

-- | Parse wait statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseWaitStatement :: Parser Statement
parseWaitStatement = do
  pos <- getSourcePos
  void $ keyword "wait"
  -- Optional wait condition
  waitExpr <- optional $ choice
    [ keyword "until" >> parseExpression
    , keyword "for" >> parseExpression
    , keyword "on" >> parseExpression
    ]
  void semi
  pure WaitStatement
    { stmtWaitExpr = waitExpr
    , stmtLocation = sourcePosToLocation pos
    }

-- | Parse null statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseNullStatement :: Parser Statement
parseNullStatement = do
  pos <- getSourcePos
  void $ keyword "null"
  void semi
  pure NullStatement
    { stmtLocation = sourcePosToLocation pos
    }

-- | Parse VHDL expression with proper precedence
-- Contract: spellcraft-adc-013 Section: Expression Parser
parseExpression :: Parser Expression
parseExpression = parseLogicalOrExpr

-- Precedence levels (lowest to highest):
-- 1. Logical OR
-- 2. Logical AND
-- 3. Relational (=, /=, <, >, <=, >=)
-- 4. Adding (+, -, &)
-- 5. Multiplying (*, /, mod, rem)
-- 6. Power (**) - ADC-019
-- 7. Unary (not, -)
-- 8. Primary (literals, identifiers, parentheses, function calls)

parseLogicalOrExpr :: Parser Expression
parseLogicalOrExpr = parseBinaryOp parseLogicalAndExpr [("or", Or), ("nor", Nor), ("xor", Xor)]

parseLogicalAndExpr :: Parser Expression
parseLogicalAndExpr = parseBinaryOp parseRelationalExpr [("and", And), ("nand", Nand)]

parseRelationalExpr :: Parser Expression
parseRelationalExpr = parseBinaryOp parseAddingExpr
  [ ("<=", LEq), (">=", GEq), ("/=", NEq)
  , ("=", Eq), ("<", Lt), (">", Gt)
  ]

parseAddingExpr :: Parser Expression
parseAddingExpr = parseBinaryOp parseMultiplyingExpr [("+", Add), ("-", Sub), ("&", Concat)]

parseMultiplyingExpr :: Parser Expression
parseMultiplyingExpr = parseBinaryOp parsePowerExpr
  [ ("*", Mul), ("/", Div), ("mod", Mod), ("rem", Rem)
  ]

-- ADC-IMPLEMENTS: spellcraft-adc-019
parsePowerExpr :: Parser Expression
parsePowerExpr = parseBinaryOp parseUnaryExpr [("**", Pow)]

parseUnaryExpr :: Parser Expression
parseUnaryExpr = choice
  [ try $ UnaryExpr Not <$> (keyword "not" >> parseUnaryExpr)
  , try $ UnaryExpr Negate <$> (symbol "-" >> parseUnaryExpr)
  , parsePrimaryExpr
  ]

parsePrimaryExpr :: Parser Expression
parsePrimaryExpr = do
  -- ADC-IMPLEMENTS: spellcraft-adc-016
  -- Parse base expression, then check for postfix attributes
  base <- choice
    [ try parseAggregate             -- ADC-015: Try aggregates first (others => ..., (x, y, z))
    , try (parens parseExpression)  -- Then parenthesized expressions
    , try parseLiteral               -- Then literals
    , try parseFunctionCallOrIndexed -- Function calls/indexed (has '(' after identifier)
    , IdentifierExpr <$> identifier  -- Finally bare identifiers
    ]
  -- Check for postfix operators: attributes ('attribute) and selected names (.field)
  parsePostfixOps base

-- ADC-IMPLEMENTS: spellcraft-adc-016, spellcraft-adc-026, spellcraft-adc-027
-- | Parse postfix operators (attributes, selected names, and cascaded index/slice)
-- Handles:
--   - Attributes: signal'event, arr'length, type'image(value)
--   - Selected names: signal.field, record.field1.field2
--   - Cascaded index/slice: arr(0)(7 downto 0), registers_in(12)(3 downto 0)
--   - Mixed: signal.field'length, arr(i).field
parsePostfixOps :: Expression -> Parser Expression
parsePostfixOps base = do
  ops <- many $ try $ choice
    [ do  -- Attribute: 'name or 'name(params)
        void $ char '\''
        lookAhead $ satisfy (\c -> isAlpha c || c == '_')
        attrName <- identifier
        params <- option [] $ try $ parens $ parseExpression `sepBy` comma
        pure $ \expr -> AttributeExpr expr attrName params
    , do  -- Selected name: .field
        void $ symbol "."
        -- Ensure next character is letter/underscore (not digit - that would be real literal)
        lookAhead $ satisfy (\c -> isAlpha c || c == '_')
        fieldName <- identifier
        pure $ \expr -> SelectedName expr fieldName
    , do  -- ADC-027: Cascaded index or slice: (index) or (high downto low)
        -- This handles: arr(0)(7 downto 0), data(i)(j)
        result <- parens $ do
          firstExpr <- parseExpression
          choice
            [ try $ do
                dir <- try (keyword "downto" >> pure DownTo) <|> (keyword "to" >> pure To)
                secondExpr <- parseExpression
                pure $ Right (dir, firstExpr, secondExpr)  -- It's a slice
            , pure $ Left firstExpr  -- It's an index
            ]
        case result of
          Right (dir, high, low) -> pure $ \expr -> SliceExpr expr high low dir
          Left idx -> pure $ \expr -> IndexedName expr idx
    ]
  -- Apply all postfix operators left-to-right
  pure $ foldl (\acc applyOp -> applyOp acc) base ops

-- | Parse binary operators with left associativity
parseBinaryOp :: Parser Expression -> [(Text, BinaryOp)] -> Parser Expression
parseBinaryOp parseHigher ops = do
  left <- parseHigher
  rest <- many $ do
    op <- choice [try (symbol sym >> pure oper) <|> try (keyword sym >> pure oper) | (sym, oper) <- ops]
    right <- parseHigher
    pure (op, right)
  pure $ foldl (\acc (op, right) -> BinaryExpr op acc right) left rest

-- | Parse function call, indexed name, or slice
-- Combined to avoid parsing identifier twice
-- Uses try to backtrack if there's no '(' after identifier
-- ADC-IMPLEMENTS: spellcraft-adc-017
parseFunctionCallOrIndexed :: Parser Expression
parseFunctionCallOrIndexed = try $ do
  name <- identifier
  -- Now parse as function call, slice, or indexed name
  result <- parens $ do
    -- Parse first expression
    firstExpr <- parseExpression
    -- Check what comes next to determine type
    choice
      [ try $ do
          -- Slice: downto or to keyword
          -- Use try on "downto" to allow backtracking to "to"
          dir <- try (keyword "downto" >> pure DownTo) <|> (keyword "to" >> pure To)
          secondExpr <- parseExpression
          pure $ Right (dir, firstExpr, secondExpr)
      , do
          -- Function call or indexed: comma-separated expressions
          rest <- many $ try $ (comma >> parseExpression)
          pure $ Left (firstExpr, rest)
      ]
  -- Process result
  case result of
    Right (dir, high, low) ->
      -- It's a slice: signal(high downto/to low)
      pure $ SliceExpr (IdentifierExpr name) high low dir
    Left (firstExpr, rest) ->
      if null rest
        then do
          -- Single expression - could be function call with one arg or indexed name
          -- Try to see if there are more indices
          moreIndices <- many $ try $ parens parseExpression
          if null moreIndices
            then pure $ FunctionCall name [firstExpr]  -- Function call with 1 arg
            else pure $ foldl IndexedName (IdentifierExpr name) (firstExpr : moreIndices)
        else
          -- Multiple comma-separated expressions - function call
          pure $ FunctionCall name (firstExpr : rest)

-- | Parse function call (kept for backward compat, delegates to combined parser)
parseFunctionCall :: Parser Expression
parseFunctionCall = parseFunctionCallOrIndexed

-- | Parse indexed name (kept for backward compat, delegates to combined parser)
parseIndexedName :: Parser Expression
parseIndexedName = parseFunctionCallOrIndexed

-- | Parse aggregate (array literal)
-- ADC-IMPLEMENTS: spellcraft-adc-015
-- Handles aggregates like (others => '0'), (1, 2, 3), (x => 1, y => 2)
-- Note: Must be tried before (parens parseExpression) to catch => syntax
parseAggregate :: Parser Expression
parseAggregate = do
  void $ symbol "("
  -- Try to parse aggregate elements
  -- Aggregates can be:
  --   (others => value)
  --   (index => value, ...)
  --   (value, value, ...)
  --
  -- We detect aggregates by looking for "=>" or multiple comma-separated values

  -- Try aggregate with =>
  firstElem <- try $ do
    key <- choice [keyword "others" >> pure (IdentifierExpr "others"), parseExpression]
    hasArrow <- optional (symbol "=>")
    case hasArrow of
      Just _ -> do
        val <- parseExpression
        pure $ BinaryExpr Eq key val  -- Reuse Eq operator to represent =>
      Nothing -> pure key  -- Just a value

  -- Parse remaining elements
  restElems <- many $ try $ do
    void comma
    key <- choice [keyword "others" >> pure (IdentifierExpr "others"), parseExpression]
    hasArrow <- optional (symbol "=>")
    case hasArrow of
      Just _ -> do
        val <- parseExpression
        pure $ BinaryExpr Eq key val
      Nothing -> pure key

  void $ symbol ")"

  let allElems = firstElem : restElems
  -- If single element without =>, treat as parenthesized expression
  case allElems of
    [single@(BinaryExpr Eq _ _)] -> pure $ Aggregate [single]  -- (others => x) is aggregate
    [single] -> pure single  -- (x) is just parentheses
    multiple -> pure $ Aggregate multiple  -- Multiple elements is aggregate

-- | Parse literal
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- Extended to support VHDL based literals (x"BEEF", b"1010", o"777")
parseLiteral :: Parser Expression
parseLiteral = LiteralExpr <$> choice
  [ try (RealLiteral <$> double)
  , try (IntLiteral <$> integer)
  , try (BasedLiteral <$> basedLiteral)  -- x"BEEF", b"1010", o"777"
  , try (StringLiteral <$> stringLiteral)
  , try parseBitLiteral
  , try parseCharLiteral
  ]

-- | Parse bit literal ('0' or '1')
-- ADC-IMPLEMENTS: spellcraft-adc-015
parseBitLiteral :: Parser Literal
parseBitLiteral = lexeme $ do
  void $ char '\''
  bit <- satisfy (\c -> c == '0' || c == '1')
  void $ char '\''
  pure $ BitLiteral (bit == '1')

-- | Parse character literal
-- ADC-IMPLEMENTS: spellcraft-adc-015
parseCharLiteral :: Parser Literal
parseCharLiteral = lexeme $ do
  void $ char '\''
  c <- anySingle
  void $ char '\''
  pure $ CharLiteral c
