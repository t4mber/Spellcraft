{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- ADC-IMPLEMENTS: <videomancer-config-01>
-- Purpose: Parse and represent JSON parameter configuration files for video synthesis hardware
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Config
  ( -- * Parameter Configuration Types
    ParameterType(..)
  , ParameterConfig(..)
  , ProgramConfig(..)
  , VideomancerConfig(..)
  , AnalysisMode(..)
    -- * Loading Functions
  , loadProgramConfig
  , validateConfig
  , validateParameter
    -- * Validation Errors
  , ConfigError(..)
  ) where

import Control.Monad (when, unless)
import Data.Aeson (FromJSON(..), ToJSON, (.:), (.:?), withObject, withText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set

-- | Type of parameter (RANGE or BOOLEAN)
data ParameterType
  = RangeParameter
  | BooleanParameter
  deriving (Show, Eq, Generic)

instance FromJSON ParameterType where
  parseJSON = withText "ParameterType" $ \case
    "RANGE" -> pure RangeParameter
    "BOOLEAN" -> pure BooleanParameter
    other -> fail $ "Unknown parameter type: " <> T.unpack other

instance ToJSON ParameterType

-- | Configuration for a single parameter
data ParameterConfig = ParameterConfig
  { paramRef :: Text              -- ^ Parameter reference (e.g., "P1", "P2")
  , paramName :: Text             -- ^ Human-readable name
  , paramType :: ParameterType    -- ^ Type of parameter
  , paramFloatPrecision :: Maybe Int    -- ^ Decimal precision for RANGE
  , paramMin :: Maybe Double      -- ^ Minimum value for RANGE
  , paramMax :: Maybe Double      -- ^ Maximum value for RANGE
  , paramSuffix :: Maybe Text     -- ^ Unit suffix (e.g., "%", "")
  , paramLabels :: Maybe [Text]   -- ^ Labels for BOOLEAN (should be exactly 2)
  } deriving (Show, Eq, Generic)

instance FromJSON ParameterConfig where
  parseJSON = withObject "ParameterConfig" $ \v -> ParameterConfig
    <$> v .: "ref"
    <*> v .: "name"
    <*> v .: "type"
    <*> v .:? "float_precision"
    <*> v .:? "min"
    <*> v .:? "max"
    <*> v .:? "suffix"
    <*> v .:? "labels"

instance ToJSON ParameterConfig

-- | Top-level program configuration
data ProgramConfig = ProgramConfig
  { progName :: Text              -- ^ Program name
  , progVersion :: Text           -- ^ Version string
  , progParameters :: [ParameterConfig]  -- ^ List of parameter configs
  } deriving (Show, Eq, Generic)

instance FromJSON ProgramConfig where
  parseJSON = withObject "ProgramConfig" $ \v -> do
    config <- v .: "program_config"
    ProgramConfig
      <$> config .: "name"
      <*> config .: "version"
      <*> config .: "parameter_configs"

instance ToJSON ProgramConfig

-- | Analysis mode for Videomancer
data AnalysisMode
  = StandardAnalysis       -- ^ Standard VHDL analysis
  | StrictValidation       -- ^ Strict parameter validation (fail on warnings)
  | PermissiveValidation   -- ^ Permissive validation (warnings only)
  deriving (Show, Eq, Generic)

instance ToJSON AnalysisMode

-- | Complete Videomancer configuration
data VideomancerConfig = VideomancerConfig
  { vcProgramConfig :: ProgramConfig     -- ^ Parameter configuration
  , vcVhdlFiles :: [FilePath]            -- ^ VHDL files to analyze
  , vcAnalysisMode :: AnalysisMode       -- ^ Analysis mode
  } deriving (Show, Eq, Generic)

instance ToJSON VideomancerConfig

-- | Configuration validation errors
data ConfigError
  = FileNotFound FilePath
  | JSONParseError FilePath Text
  | ValidationError Text
  | RangeMissingMinMax Text  -- ^ Parameter ref
  | BooleanInvalidLabels Text Int  -- ^ Parameter ref, label count
  | DuplicateParameterRef Text
  | InvalidFloatPrecision Text Int
  | InvalidRange Text Double Double  -- ^ Parameter ref, min, max
  deriving (Show, Eq)

-- | Load program configuration from JSON file
loadProgramConfig :: FilePath -> IO (Either ConfigError ProgramConfig)
loadProgramConfig path = do
  -- Check file exists
  content <- BS.readFile path
  case Aeson.eitherDecodeStrict' content of
    Left err -> pure $ Left $ JSONParseError path (T.pack err)
    Right config -> do
      -- Validate the loaded config
      case validateConfig config of
        Left validationErr -> pure $ Left validationErr
        Right () -> pure $ Right config

-- | Validate a program configuration
-- Contract: spellcraft-adc-010 Section: DataModel: VideomancerConfig
validateConfig :: ProgramConfig -> Either ConfigError ()
validateConfig config = do
  -- Validate each parameter
  mapM_ validateParameter (progParameters config)

  -- Check for duplicate refs
  checkDuplicateRefs (progParameters config)

  pure ()

-- | Validate a single parameter configuration
validateParameter :: ParameterConfig -> Either ConfigError ()
validateParameter param =
  case paramType param of
    RangeParameter -> validateRangeParameter param
    BooleanParameter -> validateBooleanParameter param

-- | Validate RANGE parameter
validateRangeParameter :: ParameterConfig -> Either ConfigError ()
validateRangeParameter param = do
  -- RANGE parameters MUST have min and max
  when (isNothing (paramMin param) || isNothing (paramMax param)) $
    Left $ RangeMissingMinMax (paramRef param)

  -- float_precision MUST be >= 0 if present
  case paramFloatPrecision param of
    Just precision | precision < 0 ->
      Left $ InvalidFloatPrecision (paramRef param) precision
    _ -> pure ()

  -- min MUST be < max
  case (paramMin param, paramMax param) of
    (Just minVal, Just maxVal) | minVal >= maxVal ->
      Left $ InvalidRange (paramRef param) minVal maxVal
    _ -> pure ()

  pure ()

-- | Validate BOOLEAN parameter
validateBooleanParameter :: ParameterConfig -> Either ConfigError ()
validateBooleanParameter param = do
  -- BOOLEAN parameters MUST have exactly 2 labels
  case paramLabels param of
    Just labels | length labels /= 2 ->
      Left $ BooleanInvalidLabels (paramRef param) (length labels)
    Nothing ->
      Left $ BooleanInvalidLabels (paramRef param) 0
    _ -> pure ()

  pure ()

-- | Check for duplicate parameter references
checkDuplicateRefs :: [ParameterConfig] -> Either ConfigError ()
checkDuplicateRefs params =
  let refs = map paramRef params
      refSet = Set.fromList refs
  in if Set.size refSet == length refs
       then pure ()
       else
         -- Find the duplicate
         let findDup [] _ = error "Impossible: duplicate not found"
             findDup (r:rs) seen =
               if Set.member r seen
                 then r
                 else findDup rs (Set.insert r seen)
             dup = findDup refs Set.empty
         in Left $ DuplicateParameterRef dup
