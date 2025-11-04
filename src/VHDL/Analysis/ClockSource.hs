{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-007
module VHDL.Analysis.ClockSource
  ( -- * Clock Source Detection
    detectClockSources
  , detectClockSourcesWithComments
  , assignInitialFrequencies
  , isClockLikeName
  ) where

import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import VHDL.AST
  ( Entity(..)
  , PortDecl(..)
  , PortDirection(..)
  , VHDLDesign(..)
  )
import VHDL.Analysis.ClockGraph
  ( ClockGraph(..)
  , ClockNode(..)
  , ClockSource(..)
  )
import VHDL.Analysis.FrequencyParser (parseFrequencyFromText)
import VHDL.SourceLocation (SourceLocation(..), mkSourceLocation)

-- | Detect clock sources from entity input ports
-- Contract: spellcraft-adc-007 Section: Interface
detectClockSources
  :: VHDLDesign
  -> [ClockSource]
detectClockSources design =
  let entities = designEntities design
  in concatMap detectEntityClockSources entities

-- | Detect clock sources from a single entity
detectEntityClockSources :: Entity -> [ClockSource]
detectEntityClockSources entity =
  let ports = entityPorts entity
      clockPorts = filter isClockPort ports
  in mapMaybe (portToClockSource entity) clockPorts

-- | Check if a port is a clock port (input with clock-like name)
isClockPort :: PortDecl -> Bool
isClockPort port =
  case portDirection port of
    Input -> isClockLikeName (portName port)
    _ -> False

-- | Check if a name looks like a clock signal
-- Contract: spellcraft-adc-007 Section: Clock Source Detection
isClockLikeName :: Text -> Bool
isClockLikeName name =
  let lowerName = T.toLower name
  in any (`T.isInfixOf` lowerName) ["clk", "clock", "osc"]

-- | Convert a port to a clock source
-- Attempts to parse frequency from VHDL source comments
-- Contract: spellcraft-adc-007 Section: Clock Source Detection
portToClockSource :: Entity -> PortDecl -> Maybe ClockSource
portToClockSource entity port =
  Just ClockSource
    { csSignal = portName port
    , csFrequency = 50.0  -- Default fallback (comment parsing requires source text)
    , csComponent = entityName entity
    , csPort = portName port
    , csLocation = entityLocation entity
    }

-- | Detect clock sources with frequency parsing from source text
-- Contract: spellcraft-adc-007 Section: Interface
detectClockSourcesWithComments
  :: VHDLDesign
  -> Text  -- ^ Original VHDL source text
  -> [ClockSource]
detectClockSourcesWithComments design sourceText =
  let entities = designEntities design
      sources = concatMap detectEntityClockSources entities
  in map (updateFreqFromComments sourceText) sources

-- | Update clock source frequency by parsing comments from source
updateFreqFromComments :: Text -> ClockSource -> ClockSource
updateFreqFromComments sourceText source =
  let portLine = extractPortLine sourceText (csSignal source)
      parsedFreq = portLine >>= parseFrequencyFromText
  in case parsedFreq of
       Just freq -> source { csFrequency = freq }
       Nothing -> source  -- Keep default frequency

-- | Extract the line containing a port declaration (with potential comment)
extractPortLine :: Text -> Text -> Maybe Text
extractPortLine sourceText portName =
  let allLines = T.lines sourceText
      matchingLines = filter (T.isInfixOf portName) allLines
  in listToMaybe matchingLines

-- | Assign initial frequencies to clock graph nodes from sources
-- Contract: spellcraft-adc-007 Section: Interface
assignInitialFrequencies
  :: ClockGraph
  -> [ClockSource]
  -> ClockGraph
assignInitialFrequencies graph sources =
  let updatedNodes = foldl assignSourceFreq (cgNodes graph) sources
  in graph { cgNodes = updatedNodes, cgSources = sources }

-- | Assign frequency from a single clock source
assignSourceFreq :: Map Text ClockNode -> ClockSource -> Map Text ClockNode
assignSourceFreq nodes source =
  Map.adjust updateNode (csSignal source) nodes
  where
    updateNode node = node
      { cnFrequency = Just (csFrequency source)
      , cnComponent = Just (csComponent source)
      , cnPort = Just (csPort source)
      }
