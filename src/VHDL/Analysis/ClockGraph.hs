{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: vhdl-analyzer-adc-003
module VHDL.Analysis.ClockGraph
  ( -- * Clock Graph
    ClockGraph(..)
  , ClockNode(..)
  , ClockEdge(..)
  , ClockSource(..)
    -- * Analysis
  , buildClockGraph
  , AnalysisError(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import VHDL.AST
  ( Architecture(..)
  , ComponentInst(..)
  , Identifier
  , SignalName
  , VHDLDesign(..)
  )
import VHDL.Constraint.Library (ComponentLibrary, lookupComponent)
import VHDL.Constraint.Types (ComponentSpec, PortConstraint(..))
import VHDL.SourceLocation (SourceLocation)

-- | Clock connectivity graph
-- Contract: vhdl-analyzer-adc-003 Section: Interface
data ClockGraph = ClockGraph
  { cgNodes :: Map SignalName ClockNode
  , cgEdges :: [ClockEdge]
  , cgSources :: [ClockSource]
  } deriving (Show, Eq, Generic)

-- | Node in clock graph representing a signal
data ClockNode = ClockNode
  { cnSignal :: SignalName
  , cnFrequency :: Maybe Double  -- MHz, if known
  , cnComponent :: Maybe Identifier  -- Component producing this signal
  , cnPort :: Maybe Identifier  -- Output port name
  } deriving (Show, Eq, Generic)

-- | Edge in clock graph representing a connection
data ClockEdge = ClockEdge
  { ceFrom :: SignalName
  , ceTo :: SignalName
  , ceComponent :: Identifier  -- Component making the connection
  , ceInputPort :: Identifier
  } deriving (Show, Eq, Generic)

-- | Clock source with known frequency
data ClockSource = ClockSource
  { csSignal :: SignalName
  , csFrequency :: Double  -- MHz
  , csComponent :: Identifier
  , csPort :: Identifier
  , csLocation :: SourceLocation
  } deriving (Show, Eq, Generic)

-- | Analysis errors
-- Contract: vhdl-analyzer-adc-003 Section: Interface
data AnalysisError
  = UnknownComponent Identifier SourceLocation
  | MissingGeneric Identifier Identifier SourceLocation
  | InvalidGenericValue Identifier Text SourceLocation
  | CircularClockPath [SignalName]
  | AmbiguousClockSource SignalName [ClockSource]
  deriving (Show, Eq)

-- | Build clock graph from VHDL design
-- Contract: vhdl-analyzer-adc-003 Section: Interface
buildClockGraph
  :: VHDLDesign
  -> ComponentLibrary
  -> Either AnalysisError ClockGraph
buildClockGraph design lib = do
  let archs = designArchitectures design
  -- Build nodes and edges from all architectures
  let allComponents = concatMap archComponents archs

  -- Create nodes for all signals in port maps
  let signals = concatMap extractSignals allComponents
  let nodes = Map.fromList [(s, mkNode s) | s <- signals]

  -- Create edges from port map connections
  edges <- concat <$> mapM (extractEdges lib) allComponents

  -- Identify clock sources (for prototype, we'll need external specification)
  let sources = []  -- Will be enhanced in propagation phase

  pure ClockGraph
    { cgNodes = nodes
    , cgEdges = edges
    , cgSources = sources
    }

-- | Extract all signal names from a component instantiation
extractSignals :: ComponentInst -> [SignalName]
extractSignals comp = map snd (compPortMap comp)

-- | Create a basic clock node
mkNode :: SignalName -> ClockNode
mkNode signal = ClockNode
  { cnSignal = signal
  , cnFrequency = Nothing
  , cnComponent = Nothing
  , cnPort = Nothing
  }

-- | Extract edges from component instantiation
extractEdges :: ComponentLibrary -> ComponentInst -> Either AnalysisError [ClockEdge]
extractEdges lib comp = do
  case lookupComponent (compComponentName comp) lib of
    Nothing -> Left $ UnknownComponent (compComponentName comp) (compLocation comp)
    Just spec -> do
      -- Create edges for each port connection
      pure $ map (mkEdge comp) (compPortMap comp)

-- | Create a clock edge from port connection
mkEdge :: ComponentInst -> (Identifier, SignalName) -> ClockEdge
mkEdge comp (port, signal) = ClockEdge
  { ceFrom = signal
  , ceTo = signal  -- Will be refined in propagation
  , ceComponent = compInstName comp
  , ceInputPort = port
  }
