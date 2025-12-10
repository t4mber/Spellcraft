{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-012
-- ADC-IMPLEMENTS: spellcraft-adc-029
module VHDL.Analysis.SignalUsage
  ( -- * Signal Usage Analysis
    SignalInfo(..)
  , SignalViolation(..)
  , analyzeSignalUsage
  , analyzeSignalUsageWithContext
  , collectSignalDecls
  , collectSignalAssignments
  , collectSignalAssignmentsWithContext
  , collectSignalReads
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import VHDL.AST
import VHDL.SourceLocation (SourceLocation)

-- | Information about a signal's usage
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
data SignalInfo = SignalInfo
  { siName :: Identifier
  , siType :: TypeName
  , siDeclLocation :: SourceLocation
  , siAssignments :: [SourceLocation]
  , siReads :: [SourceLocation]
  } deriving (Show, Eq)

-- | Signal usage violations
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
data SignalViolation
  = UndrivenSignal
      { violationSignal :: Identifier
      , violationLocation :: SourceLocation
      , violationType :: Text
      }
  | UnusedSignal
      { violationSignal :: Identifier
      , violationLocation :: SourceLocation
      , violationType :: Text
      }
  deriving (Show, Eq)

-- | Analyze architecture for signal usage violations
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
analyzeSignalUsage :: Architecture -> [SignalViolation]
analyzeSignalUsage arch = analyzeSignalUsageWithContext emptyContext arch

-- | Analyze architecture for signal usage violations with multi-file context
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- When a context is provided, component instantiation output ports are
-- resolved using actual entity definitions instead of heuristics.
analyzeSignalUsageWithContext :: AnalysisContext -> Architecture -> [SignalViolation]
analyzeSignalUsageWithContext ctx arch =
  let signals = collectSignalDecls arch
      assignments = collectSignalAssignmentsWithContext ctx arch
      reads = collectSignalReads arch

      -- Check for undriven signals (declared but never assigned)
      undrivenViolations =
        [ UndrivenSignal
            { violationSignal = siName sig
            , violationLocation = siDeclLocation sig
            , violationType = "Signal declared but never assigned (undriven)"
            }
        | sig <- signals
        , not (siName sig `Map.member` assignments)
        ]

      -- Check for unused signals (assigned but never read)
      unusedViolations =
        [ UnusedSignal
            { violationSignal = siName sig
            , violationLocation = siDeclLocation sig
            , violationType = "Signal assigned but never read (unused)"
            }
        | sig <- signals
        , siName sig `Map.member` assignments
        , not (siName sig `Set.member` reads)
        ]
  in undrivenViolations ++ unusedViolations

-- | Collect all signal declarations from architecture
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
collectSignalDecls :: Architecture -> [SignalInfo]
collectSignalDecls arch =
  [ SignalInfo
      { siName = sigDeclName decl
      , siType = sigDeclType decl
      , siDeclLocation = sigDeclLocation decl
      , siAssignments = []
      , siReads = []
      }
  | decl <- archSignals arch
  ]

-- | Collect all signal assignments from architecture statements
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
collectSignalAssignments :: Architecture -> Map Identifier [SourceLocation]
collectSignalAssignments arch = collectSignalAssignmentsWithContext emptyContext arch

-- | Collect all signal assignments with multi-file context
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- When context is provided, component output ports are resolved using
-- actual entity definitions instead of heuristics.
collectSignalAssignmentsWithContext :: AnalysisContext -> Architecture -> Map Identifier [SourceLocation]
collectSignalAssignmentsWithContext ctx arch =
  let stmtAssignments = concatMap (collectFromArchStatementWithContext ctx) (archStatements arch)
  in Map.fromListWith (++) stmtAssignments

-- | Collect assignments from an architecture statement
collectFromArchStatement :: ArchStatement -> [(Identifier, [SourceLocation])]
collectFromArchStatement (ProcessStmt _ _ stmts _loc) =
  -- Collect assignments from process statements
  concatMap collectFromSeqStatement stmts
collectFromArchStatement (ConcurrentAssignment target _ loc) =
  -- ADC-IMPLEMENTS: spellcraft-adc-022
  -- Target is now an Expression, extract base signal name
  case targetToSignalName target of
    Just name -> [(name, [loc])]
    Nothing -> []  -- Complex target without clear signal name
collectFromArchStatement (ComponentInstStmt inst) =
  -- ADC-IMPLEMENTS: spellcraft-adc-012
  -- Component output ports assign to signals
  -- Heuristic: Assume ports with common output names are outputs
  -- Full solution requires parsing component entity declarations
  let portMaps = compPortMap inst
      outputPorts = filter isLikelyOutputPort portMaps
  in [ (targetSignal, [compLocation inst])
     | (_, expr) <- outputPorts
     , Just targetSignal <- [targetToSignalName expr]
     ]
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- Handle generate statements by recursively collecting from nested statements
collectFromArchStatement (GenerateStmt genStmt) =
  concatMap collectFromArchStatement (genStatements genStmt)

-- | Collect assignments from an architecture statement with multi-file context
-- ADC-IMPLEMENTS: spellcraft-adc-029
collectFromArchStatementWithContext :: AnalysisContext -> ArchStatement -> [(Identifier, [SourceLocation])]
collectFromArchStatementWithContext _ctx (ProcessStmt _ _ stmts _loc) =
  -- Collect assignments from process statements (context not needed)
  concatMap collectFromSeqStatement stmts
collectFromArchStatementWithContext _ctx (ConcurrentAssignment target _ loc) =
  -- ADC-IMPLEMENTS: spellcraft-adc-022
  -- Target is now an Expression, extract base signal name (context not needed)
  case targetToSignalName target of
    Just name -> [(name, [loc])]
    Nothing -> []  -- Complex target without clear signal name
collectFromArchStatementWithContext ctx (ComponentInstStmt inst) =
  -- ADC-IMPLEMENTS: spellcraft-adc-029
  -- Try to resolve component outputs using context first
  -- Fall back to heuristic if entity is not found
  let compName = compComponentName inst
      portMaps = compPortMap inst
      -- Try context-based resolution first
      contextOutputs = getOutputPortsFromContext ctx compName portMaps
  in if null contextOutputs
     then
       -- Fall back to heuristic for unknown components
       let outputPorts = filter isLikelyOutputPort portMaps
       in [ (targetSignal, [compLocation inst])
          | (_, expr) <- outputPorts
          , Just targetSignal <- [targetToSignalName expr]
          ]
     else
       contextOutputs
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- Handle generate statements by recursively collecting from nested statements
collectFromArchStatementWithContext ctx (GenerateStmt genStmt) =
  concatMap (collectFromArchStatementWithContext ctx) (genStatements genStmt)

-- | Get output ports from context using entity definitions
-- ADC-IMPLEMENTS: spellcraft-adc-029
getOutputPortsFromContext :: AnalysisContext -> Identifier -> [(Expression, Expression)] -> [(Identifier, [SourceLocation])]
getOutputPortsFromContext ctx compName portMaps =
  case lookupEntityInContext ctx compName of
    Nothing -> []  -- Entity not in context, return empty to trigger fallback
    Just entity ->
      let outputPortNames = [portName p | p <- entityPorts entity, isOutputDirection (portDirection p)]
          -- Match port map entries to output ports
          matchedOutputs = mapMaybe (matchOutputPort outputPortNames) portMaps
      in matchedOutputs
  where
    isOutputDirection Output = True
    isOutputDirection InOut = True
    isOutputDirection Input = False

-- | Match a port map entry to an output port
matchOutputPort :: [Identifier] -> (Expression, Expression) -> Maybe (Identifier, [SourceLocation])
matchOutputPort outputNames (formalExpr, actualExpr) = do
  formalName <- formalToIdentifier formalExpr
  if formalName `elem` outputNames
    then do
      targetSignal <- targetToSignalName actualExpr
      pure (targetSignal, [])  -- Location will be filled in by caller
    else Nothing

-- | Look up an entity in the context
-- ADC-IMPLEMENTS: spellcraft-adc-029
-- Handles both bare names ("contrast_u") and qualified names ("work.contrast_u")
lookupEntityInContext :: AnalysisContext -> Identifier -> Maybe Entity
lookupEntityInContext ctx name =
  let normalizedName = stripLibraryPrefix name
  in Map.lookup normalizedName (ctxEntities ctx)

-- | Strip library prefix from entity name
-- "work.contrast_u" -> "contrast_u"
stripLibraryPrefix :: Identifier -> Identifier
stripLibraryPrefix name =
  case Text.breakOn "." name of
    (_, rest) | Text.null rest -> name  -- No dot, return as-is
    (_, rest) -> Text.drop 1 rest       -- Drop the dot and return the rest

-- | Extract identifier from a formal expression
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- Handles indexed formals like a(0) by extracting the base identifier
formalToIdentifier :: Expression -> Maybe Identifier
formalToIdentifier (IdentifierExpr name) = Just name
formalToIdentifier (IndexedName baseExpr _) = formalToIdentifier baseExpr
formalToIdentifier (SliceExpr baseExpr _ _ _) = formalToIdentifier baseExpr
formalToIdentifier _ = Nothing

-- | Heuristic to determine if a port is likely an output port
-- ADC-IMPLEMENTS: spellcraft-adc-012
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- This is a temporary heuristic until full component entity parsing is implemented
isLikelyOutputPort :: (Expression, Expression) -> Bool
isLikelyOutputPort (formalExpr, _) =
  case formalToIdentifier formalExpr of
    Nothing -> False  -- Can't determine, assume input
    Just portName ->
      let lowerName = Text.toLower portName
      in -- Common VHDL output port naming conventions
         (any (`Text.isInfixOf` lowerName)
           [ "result", "output", "out", "valid", "ready", "done"
           , "data_out", "addr_out", "write_data"
           , "high_pass", "low_pass", "filtered"  -- Filter outputs
           , "q", "dout"  -- Memory/register outputs
           ]
         -- Exclude common input patterns
         && not (any (`Text.isInfixOf` lowerName)
           [ "input", "in_", "_in", "read", "enable", "reset", "clk", "clock"
           , "addr", "address", "sel", "select", "control", "mode"
           ])
         )
         -- Also check for _o suffix (common convention: signal_o = output)
         || Text.isSuffixOf "_o" lowerName
         || Text.isSuffixOf "_out" lowerName

-- | Extract base signal name from assignment target (ADC-022)
-- ADC-IMPLEMENTS: spellcraft-adc-012
-- NOTE: VHDL array indexing (signal(idx)) may be parsed as FunctionCall
-- because the syntax is ambiguous. We handle both cases here.
targetToSignalName :: Expression -> Maybe Identifier
targetToSignalName (IdentifierExpr name) = Just name
targetToSignalName (IndexedName base _) = targetToSignalName base  -- Recurse to get base signal
targetToSignalName (SliceExpr base _ _ _) = targetToSignalName base
-- FunctionCall with identifier name can be array indexing: sig(0) <= value
targetToSignalName (FunctionCall name _) = Just name
targetToSignalName _ = Nothing  -- Complex expressions

-- | Collect assignments from a sequential statement
-- Enhanced: spellcraft-adc-013 Section: SignalUsage Updates
-- Enhanced: spellcraft-adc-022 Section: Indexed Assignments
collectFromSeqStatement :: Statement -> [(Identifier, [SourceLocation])]
collectFromSeqStatement (SignalAssignment target _ loc) =
  -- ADC-IMPLEMENTS: spellcraft-adc-022
  -- Target is now an Expression, extract base signal name
  case targetToSignalName target of
    Just name -> [(name, [loc])]
    Nothing -> []  -- Complex target without clear signal name
collectFromSeqStatement (VariableAssignment _ _ _) =
  []  -- Variables don't count as signal assignments
collectFromSeqStatement (IfStatement _ thenStmts elsifs elseStmts _) =
  concatMap collectFromSeqStatement thenStmts ++
  concatMap (concatMap collectFromSeqStatement . snd) elsifs ++
  concatMap collectFromSeqStatement elseStmts
collectFromSeqStatement (CaseStatement _ whenClauses _) =
  concatMap (concatMap collectFromSeqStatement . snd) whenClauses
collectFromSeqStatement (LoopStatement _ _ body _) =
  concatMap collectFromSeqStatement body
collectFromSeqStatement (WaitStatement _ _) = []
collectFromSeqStatement (NullStatement _) = []

-- | Collect all signal reads from architecture statements
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
collectSignalReads :: Architecture -> Set Identifier
collectSignalReads arch =
  let stmtReads = concatMap collectReadsFromArchStatement (archStatements arch)
  in Set.fromList stmtReads

-- | Collect reads from an architecture statement
-- Enhanced: spellcraft-adc-013 Section: SignalUsage Updates
collectReadsFromArchStatement :: ArchStatement -> [Identifier]
collectReadsFromArchStatement (ProcessStmt _ sensitivity stmts _) =
  -- Sensitivity list contains signals that are read
  sensitivity ++ concatMap collectReadsFromSeqStatement stmts
collectReadsFromArchStatement (ConcurrentAssignment _ expr _) =
  extractSignalsFromExpr expr
collectReadsFromArchStatement (ComponentInstStmt comp) =
  -- ADC-IMPLEMENTS: spellcraft-adc-021
  -- Port map connections are reads (expressions, not just identifiers)
  concatMap (extractSignalsFromExpr . snd) (compPortMap comp)
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- Handle generate statements by recursively collecting reads from nested statements
collectReadsFromArchStatement (GenerateStmt genStmt) =
  concatMap collectReadsFromArchStatement (genStatements genStmt)

-- | Collect reads from a sequential statement
-- Enhanced: spellcraft-adc-013 Section: SignalUsage Updates
collectReadsFromSeqStatement :: Statement -> [Identifier]
collectReadsFromSeqStatement (SignalAssignment _ expr _) =
  extractSignalsFromExpr expr
collectReadsFromSeqStatement (VariableAssignment _ expr _) =
  extractSignalsFromExpr expr
collectReadsFromSeqStatement (IfStatement cond thenStmts elsifs elseStmts _) =
  extractSignalsFromExpr cond ++
  concatMap collectReadsFromSeqStatement thenStmts ++
  concatMap (concatMap collectReadsFromSeqStatement . snd) elsifs ++
  concatMap collectReadsFromSeqStatement elseStmts
collectReadsFromSeqStatement (CaseStatement expr whenClauses _) =
  extractSignalsFromExpr expr ++
  concatMap (\(whenExpr, stmts) ->
    extractSignalsFromExpr whenExpr ++
    concatMap collectReadsFromSeqStatement stmts
  ) whenClauses
collectReadsFromSeqStatement (LoopStatement _ range body _) =
  (case range of
     Just (start, end, _dir) -> extractSignalsFromExpr start ++ extractSignalsFromExpr end
     Nothing -> []) ++
  concatMap collectReadsFromSeqStatement body
collectReadsFromSeqStatement (WaitStatement mexpr _) =
  maybe [] extractSignalsFromExpr mexpr
collectReadsFromSeqStatement (NullStatement _) = []

-- | Extract signal references from expressions
-- Contract: spellcraft-adc-013 Section: SignalUsage Updates
-- Contract: spellcraft-adc-016 Section: SignalUsage Updates
-- Contract: spellcraft-adc-017 Section: SignalUsage Updates
extractSignalsFromExpr :: Expression -> [Identifier]
extractSignalsFromExpr (IdentifierExpr name) = [name]
extractSignalsFromExpr (LiteralExpr _) = []
extractSignalsFromExpr (BinaryExpr _ left right) =
  extractSignalsFromExpr left ++ extractSignalsFromExpr right
extractSignalsFromExpr (UnaryExpr _ expr) =
  extractSignalsFromExpr expr
-- ADC-IMPLEMENTS: spellcraft-adc-012
-- FunctionCall can be array indexing: arr(0) parses as FunctionCall "arr" [IntLiteral 0]
-- Include the "function" name as a potential signal read
extractSignalsFromExpr (FunctionCall name args) =
  [name] ++ concatMap extractSignalsFromExpr args
extractSignalsFromExpr (IndexedName base idx) =
  extractSignalsFromExpr base ++ extractSignalsFromExpr idx
extractSignalsFromExpr (Aggregate exprs) =
  concatMap extractSignalsFromExpr exprs
-- ADC-IMPLEMENTS: spellcraft-adc-016
-- Extract signals from attribute expressions: signal'event, arr'length
extractSignalsFromExpr (AttributeExpr base _ params) =
  extractSignalsFromExpr base ++ concatMap extractSignalsFromExpr params
-- ADC-IMPLEMENTS: spellcraft-adc-017
-- Extract signals from slice expressions: signal(7 downto 0)
extractSignalsFromExpr (SliceExpr base high low _) =
  extractSignalsFromExpr base ++ extractSignalsFromExpr high ++ extractSignalsFromExpr low
-- ADC-IMPLEMENTS: spellcraft-adc-026
-- Extract signals from selected names: record.field
extractSignalsFromExpr (SelectedName base _) =
  extractSignalsFromExpr base
-- ADC-IMPLEMENTS: spellcraft-adc-027
-- Extract signals from conditional expressions: value when cond else default
extractSignalsFromExpr (ConditionalExpr value cond elseExpr) =
  extractSignalsFromExpr value ++ extractSignalsFromExpr cond ++ extractSignalsFromExpr elseExpr
