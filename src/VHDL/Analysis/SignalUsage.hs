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
  [(target, [loc])]
collectFromArchStatement (ComponentInstStmt _) =
  []

-- | Collect assignments from a sequential statement
-- Enhanced: spellcraft-adc-013 Section: SignalUsage Updates
collectFromSeqStatement :: Statement -> [(Identifier, [SourceLocation])]
collectFromSeqStatement (SignalAssignment target _ loc) =
  [(target, [loc])]
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
     Just (start, end) -> extractSignalsFromExpr start ++ extractSignalsFromExpr end
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
extractSignalsFromExpr (FunctionCall _ args) =
  concatMap extractSignalsFromExpr args
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
