{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: <parameter-validation-algo-01>
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Timing Validation
-- Purpose: Validate that JSON parameter configurations are sound with respect to VHDL constraints
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Validation
  ( -- * Validation Functions
    validateParameterSoundness
  , ValidationMode(..)
  , ValidationReport(..)
  , ValidationStatus(..)
    -- * Video/Audio Timing Validation (v0.8.0)
  , validateVideoTiming
  , validateAudioTiming
  , validatePipelineConstraints
  , TimingViolation(..)
  , TimingValidationResult(..)
    -- * Helper Functions
  , buildParameterMapping
  , validateAgainstConstraint
  , checkCompleteness
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import VHDL.AST (VHDLDesign)
import VHDL.Videomancer.Config
  ( ProgramConfig(..), ParameterConfig(..), ParameterType(..)
  , VideoTiming(..), AudioTiming(..), PipelineConstraint(..)
  )
import VHDL.Videomancer.Constraint

-- | Validation mode
data ValidationMode
  = Strict        -- ^ Fail on any violation (including warnings)
  | Permissive    -- ^ Report warnings but don't fail
  deriving (Show, Eq)

-- | Validation status
data ValidationStatus
  = Valid         -- ^ All validations passed
  | Invalid       -- ^ Has errors
  | ValidWithWarnings  -- ^ Valid but has warnings
  | Skipped       -- ^ Validation was skipped
  deriving (Show, Eq, Generic)

instance ToJSON ValidationStatus

-- | Validation report
data ValidationReport = ValidationReport
  { vrStatus :: ValidationStatus
  , vrParametersChecked :: Int
  , vrConstraintsFound :: Int
  , vrViolations :: [ConstraintViolation]
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationReport

-- | Validate parameter soundness
-- Contract: spellcraft-adc-010 Section: Algorithm
validateParameterSoundness :: ProgramConfig
                            -> [VHDLDesign]
                            -> ValidationMode
                            -> Either [ConstraintViolation] ValidationReport
validateParameterSoundness config designs mode = do
  -- Step 1: Extract parameter constraints from VHDL generics
  let constraints = extractConstraintsFromVHDL designs

  -- Step 2: Build parameter mapping (JSON params -> VHDL constraints)
  let paramMap = buildParameterMapping (progParameters config) constraints

  -- Step 3: Validate each parameter
  let violations = concatMap (validateParameter mode) paramMap

  -- Step 4: Check for missing/extra parameters
  let completeness = checkCompleteness (progParameters config) constraints

  -- Combine all violations
  let allViolations = violations <> completeness

  -- Step 5: Generate report or return violations
  let (errors, warnings) = partitionBySeverity allViolations

  if null errors
    then if null warnings
           then Right $ ValidationReport
             { vrStatus = Valid
             , vrParametersChecked = length (progParameters config)
             , vrConstraintsFound = length constraints
             , vrViolations = []
             }
           else Right $ ValidationReport
             { vrStatus = ValidWithWarnings
             , vrParametersChecked = length (progParameters config)
             , vrConstraintsFound = length constraints
             , vrViolations = warnings
             }
    else Left allViolations

-- | Build mapping from JSON parameters to VHDL constraints
buildParameterMapping :: [ParameterConfig]
                      -> [ParameterConstraint]
                      -> [(ParameterConfig, Maybe ParameterConstraint)]
buildParameterMapping params constraints =
  let constraintMap = Map.fromList [(pcName c, c) | c <- constraints]
  in map (\p -> (p, Map.lookup (paramName p) constraintMap)) params

-- | Validate a single parameter against its constraint
validateParameter :: ValidationMode
                  -> (ParameterConfig, Maybe ParameterConstraint)
                  -> [ConstraintViolation]
validateParameter mode (param, maybeConstraint) =
  case maybeConstraint of
    Nothing ->
      if mode == Strict
        then [ConstraintViolation
               { cvParameter = paramName param
               , cvExpected = Nothing
               , cvActual = param
               , cvViolationType = MissingParameter
               , cvSeverity = Warning
               , cvMessage = "Parameter '" <> paramName param <> "' not found in VHDL generics"
               }]
        else []
    Just constraint -> validateAgainstConstraint param constraint

-- | Validate parameter against VHDL constraint
validateAgainstConstraint :: ParameterConfig
                          -> ParameterConstraint
                          -> [ConstraintViolation]
validateAgainstConstraint param constraint =
  catMaybes
    [ checkMinValue param constraint
    , checkMaxValue param constraint
    , checkTypeCompatibility param constraint
    ]

-- | Check minimum value constraint
checkMinValue :: ParameterConfig -> ParameterConstraint -> Maybe ConstraintViolation
checkMinValue param constraint =
  case (paramType param, paramMin param, pcMinValue constraint) of
    (RangeParameter, Just jsonMin, Just vhdlMin) ->
      if jsonMin < vhdlMin
        then Just $ ConstraintViolation
          { cvParameter = paramName param
          , cvExpected = Just constraint
          , cvActual = param
          , cvViolationType = MinValueMismatch
          , cvSeverity = Error
          , cvMessage = "JSON min (" <> T.pack (show jsonMin) <>
                       ") < VHDL min (" <> T.pack (show vhdlMin) <> ")"
          }
        else Nothing
    _ -> Nothing

-- | Check maximum value constraint
checkMaxValue :: ParameterConfig -> ParameterConstraint -> Maybe ConstraintViolation
checkMaxValue param constraint =
  case (paramType param, paramMax param, pcMaxValue constraint) of
    (RangeParameter, Just jsonMax, Just vhdlMax) ->
      if jsonMax > vhdlMax
        then Just $ ConstraintViolation
          { cvParameter = paramName param
          , cvExpected = Just constraint
          , cvActual = param
          , cvViolationType = MaxValueMismatch
          , cvSeverity = Error
          , cvMessage = "JSON max (" <> T.pack (show jsonMax) <>
                       ") > VHDL max (" <> T.pack (show vhdlMax) <> ")"
          }
        else Nothing
    _ -> Nothing

-- | Check type compatibility
checkTypeCompatibility :: ParameterConfig -> ParameterConstraint -> Maybe ConstraintViolation
checkTypeCompatibility param constraint =
  -- For now, just check that RANGE params have numeric constraints
  case (paramType param, pcDataType constraint) of
    (RangeParameter, Nothing) ->
      Just $ ConstraintViolation
        { cvParameter = paramName param
        , cvExpected = Just constraint
        , cvActual = param
        , cvViolationType = TypeMismatch
        , cvSeverity = Warning
        , cvMessage = "RANGE parameter has no numeric type constraint in VHDL"
        }
    _ -> Nothing

-- | Check for missing parameters and extra parameters
checkCompleteness :: [ParameterConfig] -> [ParameterConstraint] -> [ConstraintViolation]
checkCompleteness params constraints =
  let paramNames = Map.fromList [(paramName p, p) | p <- params]

      -- Find constraints without matching params
      missingParams = mapMaybe (checkMissingParam paramNames) constraints

      -- Find params without matching constraints (already handled in validateParameter)
      -- We don't double-report these

  in missingParams

-- | Check if a constraint has a matching parameter
checkMissingParam :: Map.Map Text ParameterConfig
                  -> ParameterConstraint
                  -> Maybe ConstraintViolation
checkMissingParam paramMap constraint =
  if Map.member (pcName constraint) paramMap
    then Nothing
    else Just $ ConstraintViolation
      { cvParameter = pcName constraint
      , cvExpected = Just constraint
      , cvActual = undefined  -- No corresponding param
      , cvViolationType = ExtraParameter
      , cvSeverity = Info
      , cvMessage = "VHDL generic '" <> pcName constraint <> "' has no corresponding JSON parameter"
      }

-- | Partition violations by severity
partitionBySeverity :: [ConstraintViolation]
                    -> ([ConstraintViolation], [ConstraintViolation])
partitionBySeverity violations =
  let errors = filter (\v -> cvSeverity v == Error) violations
      warnings = filter (\v -> cvSeverity v /= Error) violations
  in (errors, warnings)

--------------------------------------------------------------------------------
-- Video/Audio Timing Validation (v0.8.0)
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Timing Validation
--------------------------------------------------------------------------------

-- | Timing violation type
data TimingViolation = TimingViolation
  { tvCategory :: Text              -- ^ "Video", "Audio", or "Pipeline"
  , tvParameter :: Text             -- ^ Parameter that violated constraint
  , tvExpected :: Text              -- ^ Expected value/range
  , tvActual :: Text                -- ^ Actual value found
  , tvSeverity :: Severity          -- ^ Violation severity
  , tvMessage :: Text               -- ^ Human-readable message
  } deriving (Show, Eq, Generic)

instance ToJSON TimingViolation

-- | Result of timing validation
data TimingValidationResult = TimingValidationResult
  { tvrVideoValid :: Bool           -- ^ Video timing is valid
  , tvrAudioValid :: Bool           -- ^ Audio timing is valid
  , tvrPipelineValid :: Bool        -- ^ Pipeline constraints met
  , tvrViolations :: [TimingViolation]  -- ^ List of violations found
  } deriving (Show, Eq, Generic)

instance ToJSON TimingValidationResult

-- | Validate video timing against a standard
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Video Timing Validation
validateVideoTiming :: VideoTiming    -- ^ Expected timing standard
                    -> Double         -- ^ Actual pixel clock in MHz
                    -> Int            -- ^ Actual horizontal resolution
                    -> Int            -- ^ Actual vertical resolution
                    -> Double         -- ^ Actual frame rate in Hz
                    -> [TimingViolation]
validateVideoTiming expected actualPixClk actualHRes actualVRes actualFps =
  catMaybes
    [ checkPixelClock expected actualPixClk
    , checkHorizontalRes expected actualHRes
    , checkVerticalRes expected actualVRes
    , checkFrameRate expected actualFps
    ]

-- | Check pixel clock constraint
checkPixelClock :: VideoTiming -> Double -> Maybe TimingViolation
checkPixelClock timing actualMHz =
  let expectedMHz = vtPixelClockMHz timing
      tolerance = 0.01  -- 1% tolerance
      minMHz = expectedMHz * (1 - tolerance)
      maxMHz = expectedMHz * (1 + tolerance)
  in if actualMHz >= minMHz && actualMHz <= maxMHz
       then Nothing
       else Just TimingViolation
         { tvCategory = "Video"
         , tvParameter = "pixel_clock"
         , tvExpected = T.pack (show expectedMHz) <> " MHz"
         , tvActual = T.pack (show actualMHz) <> " MHz"
         , tvSeverity = Error
         , tvMessage = "Pixel clock " <> T.pack (show actualMHz) <>
                       " MHz does not match " <> T.pack (show (vtStandard timing)) <>
                       " standard (" <> T.pack (show expectedMHz) <> " MHz expected)"
         }

-- | Check horizontal resolution
checkHorizontalRes :: VideoTiming -> Int -> Maybe TimingViolation
checkHorizontalRes timing actualH =
  let expectedH = vtHActive timing
  in if actualH == expectedH
       then Nothing
       else Just TimingViolation
         { tvCategory = "Video"
         , tvParameter = "h_active"
         , tvExpected = T.pack (show expectedH) <> " pixels"
         , tvActual = T.pack (show actualH) <> " pixels"
         , tvSeverity = Error
         , tvMessage = "Horizontal resolution " <> T.pack (show actualH) <>
                       " does not match " <> T.pack (show (vtStandard timing)) <>
                       " standard (" <> T.pack (show expectedH) <> " expected)"
         }

-- | Check vertical resolution
checkVerticalRes :: VideoTiming -> Int -> Maybe TimingViolation
checkVerticalRes timing actualV =
  let expectedV = vtVActive timing
  in if actualV == expectedV
       then Nothing
       else Just TimingViolation
         { tvCategory = "Video"
         , tvParameter = "v_active"
         , tvExpected = T.pack (show expectedV) <> " lines"
         , tvActual = T.pack (show actualV) <> " lines"
         , tvSeverity = Error
         , tvMessage = "Vertical resolution " <> T.pack (show actualV) <>
                       " does not match " <> T.pack (show (vtStandard timing)) <>
                       " standard (" <> T.pack (show expectedV) <> " expected)"
         }

-- | Check frame rate
checkFrameRate :: VideoTiming -> Double -> Maybe TimingViolation
checkFrameRate timing actualFps =
  let expectedFps = vtFrameRateHz timing
      tolerance = 0.005  -- 0.5% tolerance for frame rate
      minFps = expectedFps * (1 - tolerance)
      maxFps = expectedFps * (1 + tolerance)
  in if actualFps >= minFps && actualFps <= maxFps
       then Nothing
       else Just TimingViolation
         { tvCategory = "Video"
         , tvParameter = "frame_rate"
         , tvExpected = T.pack (show expectedFps) <> " Hz"
         , tvActual = T.pack (show actualFps) <> " Hz"
         , tvSeverity = Warning
         , tvMessage = "Frame rate " <> T.pack (show actualFps) <>
                       " Hz differs from " <> T.pack (show (vtStandard timing)) <>
                       " standard (" <> T.pack (show expectedFps) <> " Hz expected)"
         }

-- | Validate audio timing against a standard
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Audio Timing Validation
validateAudioTiming :: AudioTiming    -- ^ Expected timing standard
                    -> Int            -- ^ Actual sample rate in Hz
                    -> Int            -- ^ Actual bit depth
                    -> Double         -- ^ Actual master clock in MHz
                    -> [TimingViolation]
validateAudioTiming expected actualSampleRate actualBitDepth actualMclk =
  catMaybes
    [ checkSampleRate expected actualSampleRate
    , checkBitDepth expected actualBitDepth
    , checkMasterClock expected actualMclk
    ]

-- | Check sample rate constraint
checkSampleRate :: AudioTiming -> Int -> Maybe TimingViolation
checkSampleRate timing actualHz =
  let expectedHz = atSampleRateHz timing
  in if actualHz == expectedHz
       then Nothing
       else Just TimingViolation
         { tvCategory = "Audio"
         , tvParameter = "sample_rate"
         , tvExpected = T.pack (show expectedHz) <> " Hz"
         , tvActual = T.pack (show actualHz) <> " Hz"
         , tvSeverity = Error
         , tvMessage = "Sample rate " <> T.pack (show actualHz) <>
                       " Hz does not match " <> T.pack (show (atStandard timing)) <>
                       " standard (" <> T.pack (show expectedHz) <> " Hz expected)"
         }

-- | Check bit depth
checkBitDepth :: AudioTiming -> Int -> Maybe TimingViolation
checkBitDepth timing actualBits =
  let expectedBits = atBitDepth timing
  in if actualBits >= expectedBits
       then Nothing  -- Allow higher bit depth
       else Just TimingViolation
         { tvCategory = "Audio"
         , tvParameter = "bit_depth"
         , tvExpected = T.pack (show expectedBits) <> " bits (minimum)"
         , tvActual = T.pack (show actualBits) <> " bits"
         , tvSeverity = Error
         , tvMessage = "Bit depth " <> T.pack (show actualBits) <>
                       " is below minimum for " <> T.pack (show (atStandard timing)) <>
                       " standard (" <> T.pack (show expectedBits) <> " bits required)"
         }

-- | Check master clock
checkMasterClock :: AudioTiming -> Double -> Maybe TimingViolation
checkMasterClock timing actualMHz =
  let expectedMHz = atMasterClockMHz timing
      tolerance = 0.001  -- 0.1% tolerance for audio clocks
      minMHz = expectedMHz * (1 - tolerance)
      maxMHz = expectedMHz * (1 + tolerance)
  in if actualMHz >= minMHz && actualMHz <= maxMHz
       then Nothing
       else Just TimingViolation
         { tvCategory = "Audio"
         , tvParameter = "master_clock"
         , tvExpected = T.pack (show expectedMHz) <> " MHz"
         , tvActual = T.pack (show actualMHz) <> " MHz"
         , tvSeverity = Error
         , tvMessage = "Master clock " <> T.pack (show actualMHz) <>
                       " MHz does not match " <> T.pack (show (atStandard timing)) <>
                       " standard (" <> T.pack (show expectedMHz) <> " MHz expected)"
         }

-- | Validate pipeline constraints
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Pipeline Validation
validatePipelineConstraints :: [PipelineConstraint]  -- ^ Pipeline stage constraints
                            -> [(Text, Int, Double)] -- ^ Actual (name, latency, throughput)
                            -> [TimingViolation]
validatePipelineConstraints constraints actuals =
  concatMap validateStage (zip constraints actuals)
  where
    validateStage (constraint, (name, latency, throughput)) =
      catMaybes
        [ checkLatency constraint name latency
        , checkThroughput constraint name throughput
        ]

-- | Check pipeline latency constraint
checkLatency :: PipelineConstraint -> Text -> Int -> Maybe TimingViolation
checkLatency constraint stageName actualLatency =
  let maxLatency = pcMaxLatencyCycles constraint
  in if actualLatency <= maxLatency
       then Nothing
       else Just TimingViolation
         { tvCategory = "Pipeline"
         , tvParameter = stageName <> ".latency"
         , tvExpected = "<= " <> T.pack (show maxLatency) <> " cycles"
         , tvActual = T.pack (show actualLatency) <> " cycles"
         , tvSeverity = Error
         , tvMessage = "Pipeline stage '" <> stageName <>
                       "' latency (" <> T.pack (show actualLatency) <>
                       " cycles) exceeds maximum (" <> T.pack (show maxLatency) <> " cycles)"
         }

-- | Check pipeline throughput constraint
checkThroughput :: PipelineConstraint -> Text -> Double -> Maybe TimingViolation
checkThroughput constraint stageName actualThroughput =
  let minThroughput = pcMinThroughputMpps constraint
  in if actualThroughput >= minThroughput
       then Nothing
       else Just TimingViolation
         { tvCategory = "Pipeline"
         , tvParameter = stageName <> ".throughput"
         , tvExpected = ">= " <> T.pack (show minThroughput) <> " Mpps"
         , tvActual = T.pack (show actualThroughput) <> " Mpps"
         , tvSeverity = Error
         , tvMessage = "Pipeline stage '" <> stageName <>
                       "' throughput (" <> T.pack (show actualThroughput) <>
                       " Mpps) below minimum (" <> T.pack (show minThroughput) <> " Mpps)"
         }
