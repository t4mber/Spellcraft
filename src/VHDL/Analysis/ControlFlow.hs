{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-012
-- Priority 2: Control Flow Analysis for latch inference detection
module VHDL.Analysis.ControlFlow
  ( -- * Control Flow Graph Types
    ControlFlowGraph(..)
  , CFGNode(..)
  , NodeId
  , Condition(..)
  , PathCoverage(..)
    -- * Analysis Functions
  , buildCFG
  , analyzePathCoverage
  , detectLatchInference
  , analyzeControlFlow
    -- * Violation Types
  , ControlFlowViolation(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (mapMaybe, catMaybes)
import VHDL.AST
import VHDL.SourceLocation (SourceLocation(..))

-- | Unique identifier for CFG nodes
type NodeId = Int

-- | Condition for CFG edges (true branch, false branch, or unconditional)
data Condition
  = TrueBranch Expression
  | FalseBranch Expression
  | Unconditional
  deriving (Show, Eq)

-- | Control Flow Graph node
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
data CFGNode
  = AssignmentNode
      { anTarget :: Identifier
      , anExpr :: Expression
      , anLocation :: SourceLocation
      , anNodeId :: NodeId
      }
  | ConditionalNode
      { cnCondition :: Expression
      , cnLocation :: SourceLocation
      , cnNodeId :: NodeId
      }
  | MergeNode
      { mnLocation :: SourceLocation
      , mnNodeId :: NodeId
      }
  | EntryNode
      { enNodeId :: NodeId
      }
  | ExitNode
      { exNodeId :: NodeId
      }
  deriving (Show, Eq)

-- | Get node ID from any CFG node
getNodeId :: CFGNode -> NodeId
getNodeId (AssignmentNode _ _ _ nid) = nid
getNodeId (ConditionalNode _ _ nid) = nid
getNodeId (MergeNode _ nid) = nid
getNodeId (EntryNode nid) = nid
getNodeId (ExitNode nid) = nid

-- | Control Flow Graph
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
data ControlFlowGraph = CFG
  { cfgNodes :: Map NodeId CFGNode
  , cfgEdges :: [(NodeId, NodeId, Condition)]
  , cfgEntry :: NodeId
  , cfgExit :: NodeId
  } deriving (Show, Eq)

-- | Path coverage analysis for a signal
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
data PathCoverage = PathCoverage
  { pcSignal :: Identifier
  , pcTotalPaths :: Int
  , pcCoveredPaths :: Int
  , pcUncoveredPaths :: Int
  , pcIsLatch :: Bool  -- True if signal assigned in some but not all paths
  } deriving (Show, Eq)

-- | Control flow violations
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
data ControlFlowViolation
  = LatchInference
      { latchSignal :: Identifier
      , latchLocation :: SourceLocation
      , latchDescription :: Text
      }
  deriving (Show, Eq)

-- | State for CFG building
data BuildState = BuildState
  { bsNextId :: NodeId
  , bsNodes :: Map NodeId CFGNode
  , bsEdges :: [(NodeId, NodeId, Condition)]
  }

-- | Initial build state
initBuildState :: BuildState
initBuildState = BuildState
  { bsNextId = 0
  , bsNodes = Map.empty
  , bsEdges = []
  }

-- | Allocate a new node ID
allocNodeId :: BuildState -> (NodeId, BuildState)
allocNodeId st = (bsNextId st, st { bsNextId = bsNextId st + 1 })

-- | Add a node to the state
addNode :: CFGNode -> BuildState -> BuildState
addNode node st = st { bsNodes = Map.insert (getNodeId node) node (bsNodes st) }

-- | Add an edge to the state
addEdge :: NodeId -> NodeId -> Condition -> BuildState -> BuildState
addEdge from to cond st = st { bsEdges = (from, to, cond) : bsEdges st }

-- | Build CFG from process statement body
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
buildCFG :: [Statement] -> SourceLocation -> ControlFlowGraph
buildCFG stmts procLoc =
  let -- Create entry node
      (entryId, st1) = allocNodeId initBuildState
      entryNode = EntryNode entryId
      st2 = addNode entryNode st1

      -- Create exit node
      (exitId, st3) = allocNodeId st2
      exitNode = ExitNode exitId
      st4 = addNode exitNode st3

      -- Build CFG for statements
      (lastIds, finalSt) = buildStmtsCFG stmts [entryId] st4

      -- Connect all last nodes to exit
      finalSt' = foldr (\nid s -> addEdge nid exitId Unconditional s) finalSt lastIds

  in CFG
       { cfgNodes = bsNodes finalSt'
       , cfgEdges = bsEdges finalSt'
       , cfgEntry = entryId
       , cfgExit = exitId
       }

-- | Build CFG for a list of statements, returns list of exit node IDs
buildStmtsCFG :: [Statement] -> [NodeId] -> BuildState -> ([NodeId], BuildState)
buildStmtsCFG [] prevIds st = (prevIds, st)
buildStmtsCFG (stmt:rest) prevIds st =
  let (nextIds, st') = buildStmtCFG stmt prevIds st
  in buildStmtsCFG rest nextIds st'

-- | Build CFG for a single statement
buildStmtCFG :: Statement -> [NodeId] -> BuildState -> ([NodeId], BuildState)

-- Signal assignment: simple node
buildStmtCFG (SignalAssignment target expr loc) prevIds st =
  let (nodeId, st1) = allocNodeId st
      targetName = extractTargetName target
      node = AssignmentNode targetName expr loc nodeId
      st2 = addNode node st1
      -- Connect all previous nodes to this node
      st3 = foldr (\pid s -> addEdge pid nodeId Unconditional s) st2 prevIds
  in ([nodeId], st3)

-- Variable assignment: treat similarly
buildStmtCFG (VariableAssignment target expr loc) prevIds st =
  let (nodeId, st1) = allocNodeId st
      targetName = extractTargetName target
      node = AssignmentNode targetName expr loc nodeId
      st2 = addNode node st1
      st3 = foldr (\pid s -> addEdge pid nodeId Unconditional s) st2 prevIds
  in ([nodeId], st3)

-- If statement: creates conditional node with branches
buildStmtCFG (IfStatement cond thenStmts elsifs elseStmts loc) prevIds st =
  let -- Create conditional node
      (condId, st1) = allocNodeId st
      condNode = ConditionalNode cond loc condId
      st2 = addNode condNode st1
      -- Connect previous to conditional
      st3 = foldr (\pid s -> addEdge pid condId Unconditional s) st2 prevIds

      -- Build then branch
      (thenExitIds, st4) = buildStmtsCFG thenStmts [condId] st3
      st5 = foldr (\_ s -> s) st4 thenExitIds  -- Edge already added by buildStmtsCFG
      -- Actually add true branch edge from cond to first then statement
      st5' = if null thenStmts
             then st4
             else let firstThenId = head $ getFirstNodes thenStmts st4
                  in addEdge condId firstThenId (TrueBranch cond) st4

      -- Build elsif branches (chain of conditionals)
      (elsifExitIds, st6) = buildElsifsCFG elsifs condId st5'

      -- Build else branch
      (elseExitIds, st7) = if null elseStmts
                           then ([condId], st6)  -- No else: condition can fall through
                           else buildStmtsCFG elseStmts [condId] st6

      -- Create merge node
      (mergeId, st8) = allocNodeId st7
      mergeNode = MergeNode loc mergeId
      st9 = addNode mergeNode st8

      -- Connect all exit nodes to merge
      allExitIds = thenExitIds ++ elsifExitIds ++ elseExitIds
      st10 = foldr (\eid s -> addEdge eid mergeId Unconditional s) st9 allExitIds

  in ([mergeId], st10)

-- Case statement: similar to if with multiple branches
buildStmtCFG (CaseStatement expr whenClauses loc) prevIds st =
  let -- Create conditional node for case
      (condId, st1) = allocNodeId st
      condNode = ConditionalNode expr loc condId
      st2 = addNode condNode st1
      st3 = foldr (\pid s -> addEdge pid condId Unconditional s) st2 prevIds

      -- Build each when clause (inline to capture condId)
      buildWhenClause (_, stmts) (exitIds, s) =
        let (clauseExits, s') = buildStmtsCFG stmts [condId] s
        in (clauseExits ++ exitIds, s')

      -- Build each when clause
      (allExitIds, st4) = foldr buildWhenClause ([], st3) whenClauses

      -- Create merge node
      (mergeId, st5) = allocNodeId st4
      mergeNode = MergeNode loc mergeId
      st6 = addNode mergeNode st5

      -- Connect all exit nodes to merge
      st7 = foldr (\eid s -> addEdge eid mergeId Unconditional s) st6 allExitIds

  in ([mergeId], st7)

-- Loop statement: simplified - treat body as sequential
buildStmtCFG (LoopStatement _ _ body loc) prevIds st =
  buildStmtsCFG body prevIds st

-- Wait/Null: no effect on control flow
buildStmtCFG (WaitStatement _ _) prevIds st = (prevIds, st)
buildStmtCFG (NullStatement _) prevIds st = (prevIds, st)

-- | Build CFG for elsif branches
buildElsifsCFG :: [(Expression, [Statement])] -> NodeId -> BuildState -> ([NodeId], BuildState)
buildElsifsCFG [] _ st = ([], st)
buildElsifsCFG ((cond, stmts):rest) prevCondId st =
  let -- Create conditional node for this elsif
      (condId, st1) = allocNodeId st
      condNode = ConditionalNode cond (getStmtLocation stmts) condId
      st2 = addNode condNode st1
      -- Connect previous condition's false branch to this
      st3 = addEdge prevCondId condId (FalseBranch cond) st2

      -- Build this elsif's body
      (bodyExits, st4) = buildStmtsCFG stmts [condId] st3

      -- Continue with rest of elsifs
      (restExits, st5) = buildElsifsCFG rest condId st4

  in (bodyExits ++ restExits, st5)

-- | Get location from first statement in list, or a default
getStmtLocation :: [Statement] -> SourceLocation
getStmtLocation [] = SourceLocation "" 0 0
getStmtLocation (s:_) = getStmtLoc s

-- | Get statement location
getStmtLoc :: Statement -> SourceLocation
getStmtLoc (SignalAssignment _ _ loc) = loc
getStmtLoc (VariableAssignment _ _ loc) = loc
getStmtLoc (IfStatement _ _ _ _ loc) = loc
getStmtLoc (CaseStatement _ _ loc) = loc
getStmtLoc (LoopStatement _ _ _ loc) = loc
getStmtLoc (WaitStatement _ loc) = loc
getStmtLoc (NullStatement loc) = loc

-- | Extract target name from expression
extractTargetName :: Expression -> Identifier
extractTargetName (IdentifierExpr name) = name
extractTargetName (IndexedName base _) = extractTargetName base
extractTargetName (SliceExpr base _ _ _) = extractTargetName base
extractTargetName _ = "<complex>"

-- | Get first node IDs that would be created for statements
getFirstNodes :: [Statement] -> BuildState -> [NodeId]
getFirstNodes _ st = [bsNextId st]  -- Next ID to be allocated

-- | Analyze path coverage for a signal in the CFG
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
analyzePathCoverage :: ControlFlowGraph -> Identifier -> PathCoverage
analyzePathCoverage cfg signal =
  let paths = enumeratePaths cfg
      totalPaths = length paths
      -- A path covers the signal if it contains an assignment to it
      coveredPaths = length $ filter (pathAssignsSignal signal cfg) paths
      uncoveredPaths = totalPaths - coveredPaths
      isLatch = coveredPaths > 0 && uncoveredPaths > 0
  in PathCoverage
       { pcSignal = signal
       , pcTotalPaths = totalPaths
       , pcCoveredPaths = coveredPaths
       , pcUncoveredPaths = uncoveredPaths
       , pcIsLatch = isLatch
       }

-- | Enumerate all paths through CFG (with depth limit to prevent explosion)
enumeratePaths :: ControlFlowGraph -> [[NodeId]]
enumeratePaths cfg = enumerateFrom (cfgEntry cfg) Set.empty 0
  where
    maxDepth = 100  -- Limit path depth
    edges = cfgEdges cfg
    exitId = cfgExit cfg

    successors :: NodeId -> [NodeId]
    successors nid = [to | (from, to, _) <- edges, from == nid]

    enumerateFrom :: NodeId -> Set NodeId -> Int -> [[NodeId]]
    enumerateFrom nid visited depth
      | depth > maxDepth = [[nid]]  -- Truncate path
      | nid == exitId = [[nid]]  -- Reached exit
      | nid `Set.member` visited = [[nid]]  -- Cycle detected
      | otherwise =
          let succs = successors nid
              visited' = Set.insert nid visited
          in if null succs
             then [[nid]]
             else concatMap (\s -> map (nid:) $ enumerateFrom s visited' (depth + 1)) succs

-- | Check if a path assigns a signal
pathAssignsSignal :: Identifier -> ControlFlowGraph -> [NodeId] -> Bool
pathAssignsSignal signal cfg path =
  any isAssignment path
  where
    nodes = cfgNodes cfg
    isAssignment nid =
      case Map.lookup nid nodes of
        Just (AssignmentNode target _ _ _) -> target == signal
        _ -> False

-- | Detect latch inference in a process
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
detectLatchInference :: [Statement] -> SourceLocation -> [ControlFlowViolation]
detectLatchInference stmts procLoc =
  let cfg = buildCFG stmts procLoc
      -- Find all signals assigned anywhere in the process
      assignedSignals = collectAssignedSignals stmts
      -- Check each signal for incomplete coverage
      coverages = map (analyzePathCoverage cfg) (Set.toList assignedSignals)
      -- Filter for latches
      latches = filter pcIsLatch coverages
  in map toLatchViolation latches
  where
    toLatchViolation pc =
      LatchInference
        { latchSignal = pcSignal pc
        , latchLocation = findFirstAssignmentLoc (pcSignal pc) stmts procLoc
        , latchDescription = Text.pack $
            "Signal '" ++ Text.unpack (pcSignal pc) ++
            "' assigned in " ++ show (pcCoveredPaths pc) ++
            " of " ++ show (pcTotalPaths pc) ++
            " code paths (potential latch inference)"
        }

-- | Collect all signals assigned in statements
collectAssignedSignals :: [Statement] -> Set Identifier
collectAssignedSignals = foldr collectFromStmt Set.empty
  where
    collectFromStmt (SignalAssignment target _ _) acc =
      Set.insert (extractTargetName target) acc
    collectFromStmt (VariableAssignment _ _ _) acc = acc  -- Skip variables
    collectFromStmt (IfStatement _ thenStmts elsifs elseStmts _) acc =
      let acc1 = foldr collectFromStmt acc thenStmts
          acc2 = foldr (\(_, stmts) a -> foldr collectFromStmt a stmts) acc1 elsifs
          acc3 = foldr collectFromStmt acc2 elseStmts
      in acc3
    collectFromStmt (CaseStatement _ whenClauses _) acc =
      foldr (\(_, stmts) a -> foldr collectFromStmt a stmts) acc whenClauses
    collectFromStmt (LoopStatement _ _ body _) acc =
      foldr collectFromStmt acc body
    collectFromStmt _ acc = acc

-- | Find the first assignment location for a signal
findFirstAssignmentLoc :: Identifier -> [Statement] -> SourceLocation -> SourceLocation
findFirstAssignmentLoc signal stmts defaultLoc =
  case findInStmts stmts of
    Just loc -> loc
    Nothing -> defaultLoc
  where
    findInStmts [] = Nothing
    findInStmts (s:rest) =
      case findInStmt s of
        Just loc -> Just loc
        Nothing -> findInStmts rest

    findInStmt (SignalAssignment target _ loc)
      | extractTargetName target == signal = Just loc
      | otherwise = Nothing
    findInStmt (IfStatement _ thenStmts elsifs elseStmts _) =
      findInStmts thenStmts `orElse`
      findInElsifs elsifs `orElse`
      findInStmts elseStmts
    findInStmt (CaseStatement _ whenClauses _) =
      findInWhenClauses whenClauses
    findInStmt (LoopStatement _ _ body _) =
      findInStmts body
    findInStmt _ = Nothing

    findInElsifs [] = Nothing
    findInElsifs ((_, stmts):rest) =
      findInStmts stmts `orElse` findInElsifs rest

    findInWhenClauses [] = Nothing
    findInWhenClauses ((_, stmts):rest) =
      findInStmts stmts `orElse` findInWhenClauses rest

    orElse Nothing b = b
    orElse a _ = a

-- | Analyze control flow for an architecture
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
analyzeControlFlow :: Architecture -> [ControlFlowViolation]
analyzeControlFlow arch =
  concatMap analyzeArchStmt (archStatements arch)
  where
    analyzeArchStmt (ProcessStmt _ _ stmts loc) =
      -- For clocked processes, analyze the inner statements after rising_edge
      let innerStmts = extractClockedBody stmts
      in detectLatchInference innerStmts loc
    analyzeArchStmt _ = []  -- Only analyze process statements

-- | Extract the body of a clocked process (statements inside rising_edge check)
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Control Flow Analysis
-- This helps reduce false positives by analyzing only the synchronous logic
extractClockedBody :: [Statement] -> [Statement]
extractClockedBody stmts =
  case stmts of
    -- Pattern: if rising_edge(clk) then ... end if;
    [IfStatement cond thenStmts [] [] _]
      | isRisingEdgeCheck cond -> thenStmts
    -- Pattern: if (clk'event and clk = '1') then ... end if;
    [IfStatement cond thenStmts [] [] _]
      | isClockEventCheck cond -> thenStmts
    -- Not a simple clocked pattern, return original statements
    _ -> stmts

-- | Check if expression is rising_edge(clk) pattern
isRisingEdgeCheck :: Expression -> Bool
isRisingEdgeCheck (FunctionCall name _) = name == "rising_edge"
isRisingEdgeCheck _ = False

-- | Check if expression is (clk'event and clk = '1') pattern
isClockEventCheck :: Expression -> Bool
isClockEventCheck (BinaryExpr And left right) =
  isEventAttribute left || isEventAttribute right
isClockEventCheck _ = False

-- | Check if expression is a 'event attribute
isEventAttribute :: Expression -> Bool
isEventAttribute (AttributeExpr _ attr _) = attr == "event"
isEventAttribute _ = False
