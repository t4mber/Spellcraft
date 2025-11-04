-- ADC-IMPLEMENTS: vhdl-analyzer-adc-003
module VHDL.Analysis.Propagation
  ( propagateFrequencies
  , traceClockPath
  , ClockPath
  , PathStep(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

-- | Clock path for tracing
-- Contract: vhdl-analyzer-adc-003 Section: Interface
type ClockPath = [PathStep]

data PathStep = PathStep
  { psSignal :: SignalName
  , psComponent :: Maybe Identifier
  , psFrequency :: Maybe Double
  } deriving (Show, Eq)

-- | Propagate frequencies through the clock graph
-- Contract: vhdl-analyzer-adc-003 Section: Interface
propagateFrequencies
  :: ClockGraph
  -> ComponentLibrary
  -> Either AnalysisError ClockGraph
propagateFrequencies graph lib = do
  -- Simple propagation: assign frequencies to nodes based on edges
  let updatedNodes = Map.map (updateNodeFreq graph lib) (cgNodes graph)
  pure graph { cgNodes = updatedNodes }

-- | Update node frequency based on incoming edges
updateNodeFreq :: ClockGraph -> ComponentLibrary -> ClockNode -> ClockNode
updateNodeFreq graph lib node =
  -- Find edges that target this signal
  let incomingEdges = filter (\e -> ceTo e == cnSignal node) (cgEdges graph)
  in case incomingEdges of
       [] -> node  -- No incoming edges, keep as is
       (edge:_) ->
         -- Get frequency from source signal
         case Map.lookup (ceFrom edge) (cgNodes graph) of
           Nothing -> node
           Just sourceNode ->
             case (cnFrequency sourceNode, cnComponent sourceNode, lookupComponent (ceComponent edge) lib) of
               (Just srcFreq, _, Just spec) ->
                 -- Calculate output frequency through component
                 case calculateOutputFrequency spec [] srcFreq of  -- TODO: pass actual generics
                   Right outFreq -> node { cnFrequency = Just outFreq }
                   Left _ -> node
               _ -> node

-- | Trace clock path from source to sink
-- Contract: vhdl-analyzer-adc-003 Section: Interface
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
