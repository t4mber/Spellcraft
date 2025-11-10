{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-001
-- ADC-IMPLEMENTS: spellcraft-adc-008
-- ADC-IMPLEMENTS: spellcraft-adc-012
module VHDL.Parser
  ( -- * Parsing
    parseVHDLFile
  , parseVHDLText
  , ParseError(..)
  ) where

import Control.Monad (void, when)
import Data.Aeson (ToJSON)
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

-- | Parse signal declaration
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
signalDecl :: Parser SignalDecl
signalDecl = do
  pos <- getSourcePos
  void $ keyword "signal"
  name <- identifier
  void colon
  sigType <- typeSpec
  void semi
  pure SignalDecl
    { sigDeclName = name
    , sigDeclType = sigType
    , sigDeclLocation = sourcePosToLocation pos
    }

-- | Parse process statement
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
-- Enhanced: spellcraft-adc-013 Section: Parser Extensions
processStmt :: Parser ArchStatement
processStmt = do
  pos <- getSourcePos
  -- Optional process label
  pName <- optional $ try (identifier <* colon)
  void $ keyword "process"
  -- Parse sensitivity list
  sensitivity <- option [] $ parens (identifier `sepBy` comma)
  trace ("Parsed sensitivity list: " ++ show sensitivity) $ pure ()
  -- Skip declarations for now
  _ <- optional $ try $ do
    trace "Trying to skip declarations" $ pure ()
    _ <- skipManyTill anySingle (try $ lookAhead $ keyword "begin")
    trace "Skipped declarations" $ pure ()
    pure ()
  trace "About to consume 'begin'" $ pure ()
  void $ keyword "begin"
  trace "After 'begin' keyword" $ pure ()
  -- Debug: try to peek at next token
  _ <- optional $ try $ do
    pos <- getSourcePos
    trace ("Current position: " ++ show pos) $ pure ()
    nextTok <- optional $ try $ lookAhead identifier
    trace ("Next identifier: " ++ show nextTok) $ pure ()
    -- Try to manually check if "end" matches
    endMatches <- optional $ try $ lookAhead (keyword "end")
    trace ("Does 'end' match? " ++ show (case endMatches of Just _ -> True; Nothing -> False)) $ pure ()
    pure ()
  -- Parse sequential statements per ADC-013
  statements <- parseSequentialStatements
  trace "After parseSequentialStatements" $ pure ()
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
concurrentAssignment :: Parser ArchStatement
concurrentAssignment = do
  pos <- getSourcePos
  target <- identifier
  void $ symbol "<="
  -- Parse expression per ADC-013
  expr <- parseExpression
  void semi
  pure ConcurrentAssignment
    { concTarget = target
    , concExpr = expr
    , concLocation = sourcePosToLocation pos
    }

-- | Parse architecture-level statement (process, concurrent, or component)
archStatement :: Parser ArchStatement
archStatement = choice
  [ try processStmt
  , try (ComponentInstStmt <$> componentInst)
  , try concurrentAssignment
  ]

-- | Skip a declaration that we don't parse (constant, type, etc.)
skipDeclaration :: Parser ()
skipDeclaration = do
  -- Try to match and skip: constant, type, subtype, component, function, procedure, etc.
  choice
    [ try (keyword "constant" >> skipTo (void semi))
    , try (keyword "type" >> skipTo (void semi))
    , try (keyword "subtype" >> skipTo (void semi))
    , try (keyword "component" >> skipTo (keyword "end" >> keyword "component" >> void semi))
    , try (keyword "function" >> skipTo (keyword "end" >> optional (keyword "function") >> void semi))
    , try (keyword "procedure" >> skipTo (keyword "end" >> optional (keyword "procedure") >> void semi))
    ]
  where
    skipTo p = skipManyTill anySingle (try $ lookAhead p) >> p

-- | Parse declaration section (signals, constants, etc.)
parseDeclarations :: Parser [SignalDecl]
parseDeclarations = do
  decls <- many $ choice
    [ try (Just <$> signalDecl)
    , try (skipDeclaration >> pure Nothing)
    ]
  pure $ catMaybes decls

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
  -- Parse declarations section (signals, constants, types, etc.)
  signals <- parseDeclarations
  -- We should now be at "begin"
  void $ keyword "begin"
  -- Parse architecture body statements
  statements <- many (try archStatement)
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
    , archSignals = signals
    , archStatements = statements
    , archComponents = [c | ComponentInstStmt c <- statements]
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

-- =============================================================================
-- ADC-013 Process Body Parsing Implementation
-- =============================================================================

-- | Parse a single sequential statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseSequentialStatement :: Parser Statement
parseSequentialStatement = do
  sc  -- Consume leading whitespace
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
parseSequentialStatements :: Parser [Statement]
parseSequentialStatements = do
  -- Parse statements until we see "end process"
  -- We need to check for "end" followed by "process" to avoid stopping at "end if", "end case", etc.
  let go = do
        -- Try to match "end process" with lookAhead
        atEnd <- (lookAhead $ try $ do
          keyword "end"
          keyword "process"
          pure True) <|> pure False

        if atEnd
          then pure []
          else do
            stmtResult <- (Just <$> parseSequentialStatement) <|> pure Nothing
            case stmtResult of
              Nothing -> pure []
              Just stmt -> do
                rest <- go
                pure (stmt : rest)

  go

-- | Parse signal assignment statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseSignalAssignment :: Parser Statement
parseSignalAssignment = do
  pos <- getSourcePos
  target <- identifier
  void $ symbol "<="
  expr <- parseExpression
  void semi
  pure SignalAssignment
    { stmtTarget = target
    , stmtExpr = expr
    , stmtLocation = sourcePosToLocation pos
    }

-- | Parse variable assignment statement
-- Contract: spellcraft-adc-013 Section: Parser Extensions
parseVariableAssignment :: Parser Statement
parseVariableAssignment = do
  pos <- getSourcePos
  target <- identifier
  void $ symbol ":="
  expr <- parseExpression
  void semi
  pure VariableAssignment
    { stmtVarTarget = target
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
-- Contract: spellcraft-adc-013 Section: Parser Extensions
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
      choice <- parseExpression <|> (keyword "others" >> pure (IdentifierExpr "others"))
      void $ symbol "=>"
      stmts <- many (try (notFollowedBy nextWhenOrEnd >> parseSequentialStatement))
      pure (choice, stmts)
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
      void $ keyword "to" <|> keyword "downto"
      end <- parseExpression
      pure $ Just (start, end)
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
-- 6. Unary (not, -)
-- 7. Primary (literals, identifiers, parentheses, function calls)

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
parseMultiplyingExpr = parseBinaryOp parseUnaryExpr
  [ ("*", Mul), ("/", Div), ("mod", Mod), ("rem", Rem)
  ]

parseUnaryExpr :: Parser Expression
parseUnaryExpr = choice
  [ try $ UnaryExpr Not <$> (keyword "not" >> parseUnaryExpr)
  , try $ UnaryExpr Negate <$> (symbol "-" >> parseUnaryExpr)
  , parsePrimaryExpr
  ]

parsePrimaryExpr :: Parser Expression
parsePrimaryExpr = choice
  [ try (parens parseExpression)  -- Try parentheses first
  , try parseLiteral               -- Then literals
  , try parseFunctionCallOrIndexed -- Function calls/indexed (has '(' after identifier)
  , IdentifierExpr <$> identifier  -- Finally bare identifiers
  ]

-- | Parse binary operators with left associativity
parseBinaryOp :: Parser Expression -> [(Text, BinaryOp)] -> Parser Expression
parseBinaryOp parseHigher ops = do
  left <- parseHigher
  rest <- many $ do
    op <- choice [try (symbol sym >> pure oper) <|> try (keyword sym >> pure oper) | (sym, oper) <- ops]
    right <- parseHigher
    pure (op, right)
  pure $ foldl (\acc (op, right) -> BinaryExpr op acc right) left rest

-- | Parse function call or indexed name
-- Combined to avoid parsing identifier twice
-- Uses try to backtrack if there's no '(' after identifier
parseFunctionCallOrIndexed :: Parser Expression
parseFunctionCallOrIndexed = try $ do
  name <- identifier
  -- Now parse as function call or indexed name
  result <- parens $ do
    -- Parse first expression
    firstExpr <- parseExpression
    -- Check if there are more (comma-separated for function, or another paren for indexed)
    rest <- many $ try $ (comma >> parseExpression)
    pure (firstExpr, rest)
  -- Process result
  let (firstExpr, rest) = result
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
-- Note: This is for aggregates like (others => '0'), not simple parentheses
-- Simple parentheses are handled by (parens parseExpression) in parsePrimaryExpr
parseAggregate :: Parser Expression
parseAggregate = do
  void $ symbol "("
  -- An aggregate must have either "others =>" or multiple elements
  exprs <- parseExpression `sepBy` comma
  void $ symbol ")"
  -- If it's just one expression in parens, it's not an aggregate
  -- This should not be reached because parens parseExpression comes first
  case exprs of
    [single] -> pure single
    multiple -> pure $ Aggregate multiple

-- | Parse literal
parseLiteral :: Parser Expression
parseLiteral = LiteralExpr <$> choice
  [ try (RealLiteral <$> double)
  , try (IntLiteral <$> integer)
  , try (StringLiteral <$> stringLiteral)
  , try parseBitLiteral
  , try parseCharLiteral
  ]

-- | Parse bit literal ('0' or '1')
parseBitLiteral :: Parser Literal
parseBitLiteral = do
  void $ char '\''
  bit <- satisfy (\c -> c == '0' || c == '1')
  void $ char '\''
  pure $ BitLiteral (bit == '1')

-- | Parse character literal
parseCharLiteral :: Parser Literal
parseCharLiteral = do
  void $ char '\''
  c <- anySingle
  void $ char '\''
  pure $ CharLiteral c
