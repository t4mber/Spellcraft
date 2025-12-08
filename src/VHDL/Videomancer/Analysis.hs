{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: <videomancer-api-01>
-- Purpose: Orchestrate video hardware analysis combining VHDL parsing, frequency analysis, and parameter validation
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Analysis
  ( -- * Analysis Functions
    analyzeVideoHardware
  , AnalysisResult(..)
    -- * Error Types
  , VideomancerError(..)
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import VHDL.AST (VHDLDesign(..))
import VHDL.Parser (ParseError(..))
import VHDL.Analysis.ClockSource (detectClockSources)
import VHDL.Videomancer.Config (ProgramConfig)

-- | Analysis result
data AnalysisResult = AnalysisResult
  { arViolations :: [Text]          -- ^ Frequency/constraint violations
  , arClockSources :: Int            -- ^ Number of clock sources found
  , arDesigns :: [VHDLDesign]        -- ^ Successfully parsed designs
  } deriving (Show, Eq, Generic)

instance ToJSON AnalysisResult

-- | Videomancer errors
data VideomancerError
  = ParseError FilePath ParseError
  | ConfigLoadError FilePath Text
  | ValidationError Text
  | AnalysisError Text
  deriving (Show, Eq, Generic)

instance Exception VideomancerError
instance ToJSON VideomancerError

-- | Analyze video hardware designs
-- Contract: spellcraft-adc-010 Section: APIEndpoint
analyzeVideoHardware :: [VHDLDesign]
                     -> Maybe ProgramConfig
                     -> IO AnalysisResult
analyzeVideoHardware designs _maybeConfig = do
  -- Extract clock sources from all designs
  let clockSources = concatMap detectClockSources designs

  -- For now, return basic analysis
  -- Future: integrate frequency analysis, combinatorial analysis, clock graph
  pure $ AnalysisResult
    { arViolations = []  -- No violations detected yet
    , arClockSources = length clockSources
    , arDesigns = designs
    }
