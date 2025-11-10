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
  let stmtAssignments = concatMap collectFromArchStatement (archStatements arch)
  in Map.fromListWith (++) stmtAssignments

-- | Collect assignments from an architecture statement
collectFromArchStatement :: ArchStatement -> [(Identifier, [SourceLocation])]
collectFromArchStatement (ProcessStmt _ _ stmts loc) =
  -- Collect assignments from process statements
  concatMap collectFromSeqStatement stmts
collectFromArchStatement (ConcurrentAssignment target _ loc) =
  [(target, [loc])]
collectFromArchStatement (ComponentInstStmt _) =
  []

-- | Collect assignments from a sequential statement
collectFromSeqStatement :: Statement -> [(Identifier, [SourceLocation])]
collectFromSeqStatement (SignalAssignment target _ loc) =
  [(target, [loc])]
collectFromSeqStatement (IfStatement _ thenStmts elseStmts _) =
  concatMap collectFromSeqStatement thenStmts ++
  concatMap collectFromSeqStatement elseStmts

-- | Collect all signal reads from architecture statements
-- Contract: spellcraft-adc-012 Section: Signal Usage Tracker
collectSignalReads :: Architecture -> Set Identifier
collectSignalReads arch =
  let stmtReads = concatMap collectReadsFromArchStatement (archStatements arch)
  in Set.fromList stmtReads

-- | Collect reads from an architecture statement
collectReadsFromArchStatement :: ArchStatement -> [Identifier]
collectReadsFromArchStatement (ProcessStmt _ sensitivity stmts _) =
  -- Sensitivity list contains signals that are read
  sensitivity ++ concatMap collectReadsFromSeqStatement stmts
collectReadsFromArchStatement (ConcurrentAssignment _ source _) =
  [source]
collectReadsFromArchStatement (ComponentInstStmt comp) =
  -- Port map connections are reads
  map snd (compPortMap comp)

-- | Collect reads from a sequential statement
collectReadsFromSeqStatement :: Statement -> [Identifier]
collectReadsFromSeqStatement (SignalAssignment _ source _) =
  [source]
collectReadsFromSeqStatement (IfStatement cond thenStmts elseStmts _) =
  cond : (concatMap collectReadsFromSeqStatement thenStmts ++
          concatMap collectReadsFromSeqStatement elseStmts)
