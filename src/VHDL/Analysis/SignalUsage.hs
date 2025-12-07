{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-012
module VHDL.Analysis.SignalUsage
  ( -- * Signal Usage Analysis
    SignalInfo(..)
  , SignalViolation(..)
  , analyzeSignalUsage
  , collectSignalDecls
  , collectSignalAssignments
  , collectSignalReads
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (trace)
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
analyzeSignalUsage arch =
  let signals = collectSignalDecls arch
      assignments = collectSignalAssignments arch
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
collectSignalAssignments arch =
  trace ("collectSignalAssignments: architecture has " ++ show (length $ archStatements arch) ++ " statements") $
  let stmtAssignments = concatMap collectFromArchStatement (archStatements arch)
      result = Map.fromListWith (++) stmtAssignments
  in trace ("collectSignalAssignments: found " ++ show (Map.size result) ++ " assigned signals: " ++ show (Map.keys result)) result

-- | Collect assignments from an architecture statement
collectFromArchStatement :: ArchStatement -> [(Identifier, [SourceLocation])]
collectFromArchStatement (ProcessStmt _ _ stmts loc) =
  -- Collect assignments from process statements
  trace ("collectFromArchStatement (ProcessStmt): found " ++ show (length stmts) ++ " statements") $
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
      assignments = [ (targetSignal, [compLocation inst])
                    | (_, expr) <- outputPorts
                    , Just targetSignal <- [targetToSignalName expr]
                    ]
  in trace ("collectFromArchStatement (ComponentInstStmt): found " ++ show (length assignments) ++ " output port assignments") assignments
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- Handle generate statements by recursively collecting from nested statements
collectFromArchStatement (GenerateStmt genStmt) =
  let nestedStmts = genStatements genStmt
      assignments = concatMap collectFromArchStatement nestedStmts
  in trace ("collectFromArchStatement (GenerateStmt): found " ++ show (length assignments) ++ " nested assignments") assignments

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
