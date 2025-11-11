{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: spellcraft-adc-003
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
import Data.Maybe (mapMaybe, listToMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import VHDL.AST
  ( Architecture(..)
  , ComponentInst(..)
  , Entity(..)
  , Expression(..)
  , Identifier
  , PortDecl(..)
  , PortDirection(..)
  , SignalName
  , VHDLDesign(..)
  )
import VHDL.Constraint.Library (ComponentLibrary, lookupComponent)
import VHDL.Constraint.Types (ComponentSpec(..), PortConstraint(..))
import VHDL.SourceLocation (SourceLocation)

-- | Clock connectivity graph
-- Contract: spellcraft-adc-003 Section: Interface
-- Enhanced with: spellcraft-adc-007 component instance storage
data ClockGraph = ClockGraph
  { cgNodes :: Map SignalName ClockNode
  , cgEdges :: [ClockEdge]
  , cgSources :: [ClockSource]
  , cgComponents :: [ComponentInst]  -- Store for generic map lookup
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
-- Contract: spellcraft-adc-003 Section: Interface
data AnalysisError
  = UnknownComponent Identifier SourceLocation
  | MissingGeneric Identifier Identifier SourceLocation
  | InvalidGenericValue Identifier Text SourceLocation
  | CircularClockPath [SignalName]
  | AmbiguousClockSource SignalName [ClockSource]
  deriving (Show, Eq)

-- | Build clock graph from VHDL design
-- Contract: spellcraft-adc-003 Section: Interface
-- Enhanced with: spellcraft-adc-007 Section: Clock Source Detection
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
  let baseNodes = Map.fromList [(s, mkNode s) | s <- signals]

  -- Create edges from port map connections (with proper signal flow)
  edges <- concat <$> mapM (extractEdges lib) allComponents

  -- Detect clock sources from entity ports (ADC-007)
  let sources = detectClockSourcesFromDesign design

  -- Assign initial frequencies to source nodes
  let nodesWithFreqs = assignInitialFreqs baseNodes sources

  pure ClockGraph
    { cgNodes = nodesWithFreqs
    , cgEdges = edges
    , cgSources = sources
    , cgComponents = allComponents  -- Store for frequency propagation
    }

-- | Detect clock sources from design (ADC-007)
detectClockSourcesFromDesign :: VHDLDesign -> [ClockSource]
detectClockSourcesFromDesign design =
  let entities = designEntities design
  in concatMap detectEntitySources entities

-- | Detect clock sources from entity input ports
detectEntitySources :: Entity -> [ClockSource]
detectEntitySources entity =
  let ports = entityPorts entity
      clockPorts = filter isClockLikePort ports
  in map (makeClockSource entity) clockPorts

-- | Check if port is clock-like (input with clock-like name)
isClockLikePort :: PortDecl -> Bool
isClockLikePort port =
  case portDirection port of
    Input -> isClockLikeName (portName port)
    _ -> False

-- | Check if a name looks like a clock signal
isClockLikeName :: Text -> Bool
isClockLikeName name =
  let lowerName = T.toLower name
  in any (`T.isInfixOf` lowerName) ["clk", "clock", "osc"]

-- | Create clock source from entity port
makeClockSource :: Entity -> PortDecl -> ClockSource
makeClockSource entity port = ClockSource
  { csSignal = portName port
  , csFrequency = 50.0  -- Default; TODO: parse from comments
  , csComponent = entityName entity
  , csPort = portName port
  , csLocation = entityLocation entity
  }

-- | Assign initial frequencies from sources
assignInitialFreqs :: Map SignalName ClockNode -> [ClockSource] -> Map SignalName ClockNode
assignInitialFreqs nodes sources =
  foldl assignOne nodes sources
  where
    assignOne ns source =
      Map.adjust (\n -> n { cnFrequency = Just (csFrequency source)
                          , cnComponent = Just (csComponent source)
                          , cnPort = Just (csPort source)
                          }) (csSignal source) ns

-- | Extract all signal names from a component instantiation
-- ADC-IMPLEMENTS: spellcraft-adc-021
-- Port map now contains expressions, extract signal identifiers from them
extractSignals :: ComponentInst -> [SignalName]
extractSignals comp = catMaybes $ map (exprToSignalName . snd) (compPortMap comp)

-- | Extract signal name from simple expression (best effort)
-- ADC-IMPLEMENTS: spellcraft-adc-021
exprToSignalName :: Expression -> Maybe SignalName
exprToSignalName (IdentifierExpr name) = Just name
exprToSignalName _ = Nothing  -- Complex expressions don't represent single signals

-- | Create a basic clock node
mkNode :: SignalName -> ClockNode
mkNode signal = ClockNode
  { cnSignal = signal
  , cnFrequency = Nothing
  , cnComponent = Nothing
  , cnPort = Nothing
  }

-- | Extract edges from component instantiation
-- Enhanced with proper signal flow (ADC-007)
extractEdges :: ComponentLibrary -> ComponentInst -> Either AnalysisError [ClockEdge]
extractEdges lib comp = do
  case lookupComponent (compComponentName comp) lib of
    Nothing -> Left $ UnknownComponent (compComponentName comp) (compLocation comp)
    Just spec -> do
      -- Create edges connecting input signals to output signals through component
      pure $ buildComponentEdges comp spec

-- | Build edges showing signal flow through component
-- Fixed: Use port direction instead of name heuristics
-- ADC-IMPLEMENTS: spellcraft-adc-021
-- Port map now contains expressions, extract signal identifiers
buildComponentEdges :: ComponentInst -> ComponentSpec -> [ClockEdge]
buildComponentEdges comp spec =
  let portMap = Map.fromList (compPortMap comp)
      -- Find clock input ports (Input direction + clock-like name)
      clockInputs = [ p | p <- compSpecPorts spec
                        , portConstraintDirection p == Input
                        , isClockLikeName (portConstraintName p) ]
      -- Find clock output ports (Output direction + clock-like name)
      clockOutputs = [ p | p <- compSpecPorts spec
                         , portConstraintDirection p == Output
                         , isClockLikeName (portConstraintName p) ]
  in [ ClockEdge
       { ceFrom = inputSig
       , ceTo = outputSig
       , ceComponent = compInstName comp
       , ceInputPort = portConstraintName inp
       }
     | inp <- clockInputs
     , out <- clockOutputs
     , Just inputExpr <- [Map.lookup (portConstraintName inp) portMap]
     , Just outputExpr <- [Map.lookup (portConstraintName out) portMap]
     , Just inputSig <- [exprToSignalName inputExpr]
     , Just outputSig <- [exprToSignalName outputExpr]
     ]
