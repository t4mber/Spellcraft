-- ADC-IMPLEMENTS: spellcraft-adc-003
module VHDL.Analysis.Propagation
  ( propagateFrequencies
  , traceClockPath
  , ClockPath
  , PathStep(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import VHDL.AST (Identifier, SignalName, ComponentInst(..))
import VHDL.Analysis.ClockGraph
  ( AnalysisError
  , ClockEdge(..)
  , ClockGraph(..)
  , ClockNode(..)
  , ClockSource(..)
  )
import VHDL.Analysis.FrequencyCalc (calculateOutputFrequency)
import VHDL.Constraint.Library (ComponentLibrary, lookupComponent)
import qualified Data.Text as T
import Debug.Trace (trace)

-- | Clock path for tracing
-- Contract: spellcraft-adc-003 Section: Interface
type ClockPath = [PathStep]

data PathStep = PathStep
  { psSignal :: SignalName
  , psComponent :: Maybe Identifier
  , psFrequency :: Maybe Double
  } deriving (Show, Eq)

-- | Propagate frequencies through the clock graph
-- Contract: spellcraft-adc-003 Section: Interface
-- Enhanced with: spellcraft-adc-007 iterative propagation
propagateFrequencies
  :: ClockGraph
  -> ComponentLibrary
  -> Either AnalysisError ClockGraph
propagateFrequencies graph lib = do
  -- Iteratively propagate frequencies through the graph
  -- until no more changes occur (fixed point)
  pure $ iterateUntilStable graph lib 10  -- Max 10 iterations

-- | Iterate propagation until stable or max iterations
iterateUntilStable :: ClockGraph -> ComponentLibrary -> Int -> ClockGraph
iterateUntilStable graph _ 0 = trace "Max iterations reached" graph
iterateUntilStable graph lib n =
  let _ = trace ("\n=== Propagation iteration " ++ show (11 - n) ++ " ===") ()
      updatedNodes = Map.mapWithKey (updateNodeFreqWithEdges graph lib) (cgNodes graph)
      newGraph = graph { cgNodes = updatedNodes }
      changed = updatedNodes /= cgNodes graph
      _ = trace ("  Nodes changed: " ++ show changed) ()
  in if not changed
     then trace ("Converged after " ++ show (11 - n) ++ " iterations") graph
     else iterateUntilStable newGraph lib (n - 1)

-- | Update node frequency based on incoming edges
-- Enhanced with: spellcraft-adc-007 proper generic map passing
updateNodeFreqWithEdges :: ClockGraph -> ComponentLibrary -> SignalName -> ClockNode -> ClockNode
updateNodeFreqWithEdges graph lib signalName node =
  -- Find edges that target this signal
  let incomingEdges = filter (\e -> ceTo e == signalName) (cgEdges graph)
  in trace ("Updating " ++ show signalName ++ ", incoming edges: " ++ show (length incomingEdges)) $
     case incomingEdges of
       [] -> node  -- No incoming edges, keep as is (might be a source)
       (edge:_) ->
         let _ = trace ("  Edge from: " ++ show (ceFrom edge) ++ " via " ++ show (ceComponent edge)) ()
         in -- Get component instance first to get the component type
         case findComponentInst (ceComponent edge) graph of
           Nothing -> trace ("  ERROR: Component instance not found: " ++ show (ceComponent edge)) node
           Just compInst ->
             let componentType = compComponentName compInst
                 _ = trace ("  Component type: " ++ show componentType) ()
             in -- Now get frequency from source and component spec
             case (Map.lookup (ceFrom edge) (cgNodes graph), lookupComponent componentType lib) of
               (Nothing, _) -> trace ("  ERROR: Source node not found: " ++ show (ceFrom edge)) node
               (_, Nothing) -> trace ("  ERROR: Component type not in library: " ++ show componentType) node
               (Just sourceNode, Just spec) ->
                 let _ = trace ("  Source freq: " ++ show (cnFrequency sourceNode)) ()
                 in case cnFrequency sourceNode of
                   Nothing -> trace ("  Source has no frequency yet") node
                   Just srcFreq ->
                     -- We already have compInst from above
                     let generics = compGenericMap compInst
                         _ = trace ("  Generics: " ++ show generics) ()
                     in -- Calculate output frequency using component generics
                     case calculateOutputFrequency spec generics srcFreq of
                       Left err -> trace ("  Calc error: " ++ show err) node
                       Right outFreq -> trace ("  SUCCESS: Output freq = " ++ show outFreq) $
                         node
                         { cnFrequency = Just outFreq
                         , cnComponent = Just (ceComponent edge)
                         , cnPort = Just (ceInputPort edge)
                         }

-- | Find component instantiation by name from graph
findComponentInst :: Identifier -> ClockGraph -> Maybe ComponentInst
findComponentInst instName graph =
  let comps = cgComponents graph
  in listToMaybe [c | c <- comps, compInstName c == instName]

-- | Trace clock path from source to sink
-- Contract: spellcraft-adc-003 Section: Interface
traceClockPath
  :: ClockGraph
  -> SignalName  -- Start signal
  -> SignalName  -- End signal
  -> Maybe ClockPath
traceClockPath graph start end =
  case (Map.lookup start (cgNodes graph), Map.lookup end (cgNodes graph)) of
    (Just startNode, Just endNode) ->
      Just [nodeToStep startNode, nodeToStep endNode]
    _ -> Nothing

nodeToStep :: ClockNode -> PathStep
nodeToStep node = PathStep
  { psSignal = cnSignal node
  , psComponent = cnComponent node
  , psFrequency = cnFrequency node
  }
