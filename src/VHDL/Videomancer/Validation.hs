{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: <parameter-validation-algo-01>
-- Purpose: Validate that JSON parameter configurations are sound with respect to VHDL constraints
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Validation
  ( -- * Validation Functions
    validateParameterSoundness
  , ValidationMode(..)
  , ValidationReport(..)
  , ValidationStatus(..)
    -- * Helper Functions
  , buildParameterMapping
  , validateAgainstConstraint
  , checkCompleteness
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes, mapMaybe, isNothing)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import VHDL.AST (VHDLDesign)
import VHDL.Videomancer.Config (ProgramConfig(..), ParameterConfig(..), ParameterType(..))
import qualified VHDL.Videomancer.Config as Config
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
      constraintNames = Map.fromList [(pcName c, c) | c <- constraints]

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
