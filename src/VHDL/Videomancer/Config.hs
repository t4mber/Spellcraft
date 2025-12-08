{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- ADC-IMPLEMENTS: <videomancer-config-01>
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Video Timing Standards
-- Purpose: Parse and represent JSON parameter configuration files for video synthesis hardware
-- Contract: spellcraft-adc-010
module VHDL.Videomancer.Config
  ( -- * Parameter Configuration Types
    ParameterType(..)
  , ParameterConfig(..)
  , ProgramConfig(..)
  , VideomancerConfig(..)
  , AnalysisMode(..)
    -- * Video Timing Standards (v0.8.0)
  , VideoStandard(..)
  , VideoTiming(..)
  , AudioStandard(..)
  , AudioTiming(..)
  , HDRMetadata(..)
  , PipelineConstraint(..)
    -- * Standard Timing Definitions
  , videoTimingFor
  , audioTimingFor
  , standardVideoTimings
  , standardAudioTimings
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

--------------------------------------------------------------------------------
-- Video Timing Standards (v0.8.0)
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Video Timing Standards
--------------------------------------------------------------------------------

-- | Video standard definitions
data VideoStandard
  = SD480i           -- ^ 480i NTSC (720x480 @ 29.97Hz interlaced)
  | SD576i           -- ^ 576i PAL (720x576 @ 25Hz interlaced)
  | HD720p           -- ^ 720p HD (1280x720 @ 60Hz progressive)
  | HD1080i          -- ^ 1080i HD (1920x1080 @ 30Hz interlaced)
  | HD1080p          -- ^ 1080p Full HD (1920x1080 @ 60Hz progressive)
  | UHD4K            -- ^ 4K UHD (3840x2160 @ 60Hz)
  | UHD4K120         -- ^ 4K UHD (3840x2160 @ 120Hz)
  | DCI4K            -- ^ DCI 4K Cinema (4096x2160 @ 24Hz)
  | UHD8K            -- ^ 8K UHD (7680x4320 @ 60Hz)
  | UHD8K120         -- ^ 8K UHD (7680x4320 @ 120Hz)
  | Custom Text      -- ^ Custom video standard
  deriving (Show, Eq, Generic)

instance FromJSON VideoStandard where
  parseJSON = withText "VideoStandard" $ \case
    "480i"      -> pure SD480i
    "576i"      -> pure SD576i
    "720p"      -> pure HD720p
    "1080i"     -> pure HD1080i
    "1080p"     -> pure HD1080p
    "4K"        -> pure UHD4K
    "4K60"      -> pure UHD4K
    "4K120"     -> pure UHD4K120
    "DCI4K"     -> pure DCI4K
    "8K"        -> pure UHD8K
    "8K60"      -> pure UHD8K
    "8K120"     -> pure UHD8K120
    other       -> pure (Custom other)

instance ToJSON VideoStandard

-- | Video timing parameters for a given standard
data VideoTiming = VideoTiming
  { vtStandard :: VideoStandard     -- ^ Video standard name
  , vtHActive :: Int                -- ^ Horizontal active pixels
  , vtVActive :: Int                -- ^ Vertical active lines
  , vtHTotal :: Int                 -- ^ Horizontal total (including blanking)
  , vtVTotal :: Int                 -- ^ Vertical total (including blanking)
  , vtPixelClockMHz :: Double       -- ^ Pixel clock frequency in MHz
  , vtFrameRateHz :: Double         -- ^ Frame rate in Hz
  , vtInterlaced :: Bool            -- ^ True if interlaced
  , vtHdrSupport :: Bool            -- ^ True if HDR capable
  , vtColorDepth :: Int             -- ^ Bits per color (8, 10, 12)
  } deriving (Show, Eq, Generic)

instance FromJSON VideoTiming
instance ToJSON VideoTiming

-- | HDR metadata timing constraints
data HDRMetadata = HDRMetadata
  { hdrStandard :: Text              -- ^ HDR standard (HDR10, HDR10+, Dolby Vision, HLG)
  , hdrMaxLuminance :: Double        -- ^ Maximum luminance in nits
  , hdrMinLuminance :: Double        -- ^ Minimum luminance in nits
  , hdrMetadataRateHz :: Double      -- ^ Metadata update rate (0 = static, >0 = dynamic)
  , hdrInfoframePeriodMs :: Double   -- ^ HDR infoframe period in ms
  } deriving (Show, Eq, Generic)

instance FromJSON HDRMetadata
instance ToJSON HDRMetadata

-- | Audio standard definitions
data AudioStandard
  = PCM48kHz         -- ^ Standard PCM 48 kHz (CD quality consumer)
  | PCM96kHz         -- ^ High-resolution PCM 96 kHz
  | PCM192kHz        -- ^ Ultra high-resolution PCM 192 kHz
  | I2S              -- ^ I2S serial interface
  | SPDIF            -- ^ S/PDIF optical/coaxial
  | AES3             -- ^ AES3/EBU professional audio
  | HDMI_Audio       -- ^ HDMI embedded audio
  | CustomAudio Text -- ^ Custom audio standard
  deriving (Show, Eq, Generic)

instance FromJSON AudioStandard where
  parseJSON = withText "AudioStandard" $ \case
    "PCM48"     -> pure PCM48kHz
    "PCM96"     -> pure PCM96kHz
    "PCM192"    -> pure PCM192kHz
    "I2S"       -> pure I2S
    "SPDIF"     -> pure SPDIF
    "AES3"      -> pure AES3
    "HDMI"      -> pure HDMI_Audio
    other       -> pure (CustomAudio other)

instance ToJSON AudioStandard

-- | Audio timing parameters
data AudioTiming = AudioTiming
  { atStandard :: AudioStandard     -- ^ Audio standard
  , atSampleRateHz :: Int           -- ^ Sample rate in Hz
  , atBitDepth :: Int               -- ^ Bits per sample (16, 24, 32)
  , atChannels :: Int               -- ^ Number of channels
  , atBitClockMHz :: Double         -- ^ Bit clock frequency in MHz (for I2S)
  , atWordSelectHz :: Double        -- ^ Word select frequency in Hz (for I2S)
  , atMasterClockMHz :: Double      -- ^ Master clock frequency in MHz
  , atLatencyUs :: Double           -- ^ Maximum allowed latency in microseconds
  } deriving (Show, Eq, Generic)

instance FromJSON AudioTiming
instance ToJSON AudioTiming

-- | Pipeline constraint for video processing
data PipelineConstraint = PipelineConstraint
  { pcStageName :: Text             -- ^ Pipeline stage name
  , pcMaxLatencyCycles :: Int       -- ^ Maximum latency in clock cycles
  , pcMinThroughputMpps :: Double   -- ^ Minimum throughput in Mega-pixels per second
  , pcBufferDepthLines :: Int       -- ^ Required line buffer depth
  , pcFifoDepthSamples :: Int       -- ^ Required FIFO depth in samples
  } deriving (Show, Eq, Generic)

instance FromJSON PipelineConstraint
instance ToJSON PipelineConstraint

--------------------------------------------------------------------------------
-- Standard Timing Definitions
-- ADC-IMPLEMENTS: spellcraft-adc-030 Section: Timing Lookup
--------------------------------------------------------------------------------

-- | Get video timing for a standard
videoTimingFor :: VideoStandard -> VideoTiming
videoTimingFor standard = case standard of
  SD480i -> VideoTiming
    { vtStandard = SD480i
    , vtHActive = 720, vtVActive = 480
    , vtHTotal = 858, vtVTotal = 525
    , vtPixelClockMHz = 13.5
    , vtFrameRateHz = 29.97
    , vtInterlaced = True
    , vtHdrSupport = False
    , vtColorDepth = 8
    }

  SD576i -> VideoTiming
    { vtStandard = SD576i
    , vtHActive = 720, vtVActive = 576
    , vtHTotal = 864, vtVTotal = 625
    , vtPixelClockMHz = 13.5
    , vtFrameRateHz = 25.0
    , vtInterlaced = True
    , vtHdrSupport = False
    , vtColorDepth = 8
    }

  HD720p -> VideoTiming
    { vtStandard = HD720p
    , vtHActive = 1280, vtVActive = 720
    , vtHTotal = 1650, vtVTotal = 750
    , vtPixelClockMHz = 74.25
    , vtFrameRateHz = 60.0
    , vtInterlaced = False
    , vtHdrSupport = False
    , vtColorDepth = 8
    }

  HD1080i -> VideoTiming
    { vtStandard = HD1080i
    , vtHActive = 1920, vtVActive = 1080
    , vtHTotal = 2200, vtVTotal = 1125
    , vtPixelClockMHz = 74.25
    , vtFrameRateHz = 30.0
    , vtInterlaced = True
    , vtHdrSupport = False
    , vtColorDepth = 8
    }

  HD1080p -> VideoTiming
    { vtStandard = HD1080p
    , vtHActive = 1920, vtVActive = 1080
    , vtHTotal = 2200, vtVTotal = 1125
    , vtPixelClockMHz = 148.5
    , vtFrameRateHz = 60.0
    , vtInterlaced = False
    , vtHdrSupport = False
    , vtColorDepth = 8
    }

  UHD4K -> VideoTiming
    { vtStandard = UHD4K
    , vtHActive = 3840, vtVActive = 2160
    , vtHTotal = 4400, vtVTotal = 2250
    , vtPixelClockMHz = 594.0
    , vtFrameRateHz = 60.0
    , vtInterlaced = False
    , vtHdrSupport = True
    , vtColorDepth = 10
    }

  UHD4K120 -> VideoTiming
    { vtStandard = UHD4K120
    , vtHActive = 3840, vtVActive = 2160
    , vtHTotal = 4400, vtVTotal = 2250
    , vtPixelClockMHz = 1188.0
    , vtFrameRateHz = 120.0
    , vtInterlaced = False
    , vtHdrSupport = True
    , vtColorDepth = 10
    }

  DCI4K -> VideoTiming
    { vtStandard = DCI4K
    , vtHActive = 4096, vtVActive = 2160
    , vtHTotal = 4400, vtVTotal = 2250
    , vtPixelClockMHz = 237.6
    , vtFrameRateHz = 24.0
    , vtInterlaced = False
    , vtHdrSupport = True
    , vtColorDepth = 12
    }

  UHD8K -> VideoTiming
    { vtStandard = UHD8K
    , vtHActive = 7680, vtVActive = 4320
    , vtHTotal = 8800, vtVTotal = 4500
    , vtPixelClockMHz = 2376.0
    , vtFrameRateHz = 60.0
    , vtInterlaced = False
    , vtHdrSupport = True
    , vtColorDepth = 10
    }

  UHD8K120 -> VideoTiming
    { vtStandard = UHD8K120
    , vtHActive = 7680, vtVActive = 4320
    , vtHTotal = 8800, vtVTotal = 4500
    , vtPixelClockMHz = 4752.0
    , vtFrameRateHz = 120.0
    , vtInterlaced = False
    , vtHdrSupport = True
    , vtColorDepth = 10
    }

  Custom _ -> VideoTiming
    { vtStandard = standard
    , vtHActive = 0, vtVActive = 0
    , vtHTotal = 0, vtVTotal = 0
    , vtPixelClockMHz = 0
    , vtFrameRateHz = 0
    , vtInterlaced = False
    , vtHdrSupport = False
    , vtColorDepth = 8
    }

-- | Get audio timing for a standard
audioTimingFor :: AudioStandard -> AudioTiming
audioTimingFor standard = case standard of
  PCM48kHz -> AudioTiming
    { atStandard = PCM48kHz
    , atSampleRateHz = 48000
    , atBitDepth = 24
    , atChannels = 2
    , atBitClockMHz = 3.072        -- 48kHz * 32 bits * 2 channels
    , atWordSelectHz = 48000
    , atMasterClockMHz = 12.288    -- 256 * Fs
    , atLatencyUs = 1000
    }

  PCM96kHz -> AudioTiming
    { atStandard = PCM96kHz
    , atSampleRateHz = 96000
    , atBitDepth = 24
    , atChannels = 2
    , atBitClockMHz = 6.144
    , atWordSelectHz = 96000
    , atMasterClockMHz = 24.576
    , atLatencyUs = 500
    }

  PCM192kHz -> AudioTiming
    { atStandard = PCM192kHz
    , atSampleRateHz = 192000
    , atBitDepth = 24
    , atChannels = 2
    , atBitClockMHz = 12.288
    , atWordSelectHz = 192000
    , atMasterClockMHz = 49.152
    , atLatencyUs = 250
    }

  I2S -> AudioTiming
    { atStandard = I2S
    , atSampleRateHz = 48000
    , atBitDepth = 24
    , atChannels = 2
    , atBitClockMHz = 3.072        -- BCLK = Fs * bits * channels
    , atWordSelectHz = 48000       -- LRCLK = Fs
    , atMasterClockMHz = 12.288    -- MCLK = 256 * Fs typical
    , atLatencyUs = 1000
    }

  SPDIF -> AudioTiming
    { atStandard = SPDIF
    , atSampleRateHz = 48000
    , atBitDepth = 24
    , atChannels = 2
    , atBitClockMHz = 3.072
    , atWordSelectHz = 48000
    , atMasterClockMHz = 12.288
    , atLatencyUs = 2000
    }

  AES3 -> AudioTiming
    { atStandard = AES3
    , atSampleRateHz = 48000
    , atBitDepth = 24
    , atChannels = 2
    , atBitClockMHz = 3.072
    , atWordSelectHz = 48000
    , atMasterClockMHz = 12.288
    , atLatencyUs = 500
    }

  HDMI_Audio -> AudioTiming
    { atStandard = HDMI_Audio
    , atSampleRateHz = 48000
    , atBitDepth = 24
    , atChannels = 8              -- Up to 8 channels for HDMI
    , atBitClockMHz = 12.288      -- 48kHz * 32 bits * 8 channels
    , atWordSelectHz = 48000
    , atMasterClockMHz = 12.288
    , atLatencyUs = 5000          -- Video sync dependent
    }

  CustomAudio _ -> AudioTiming
    { atStandard = standard
    , atSampleRateHz = 0
    , atBitDepth = 0
    , atChannels = 0
    , atBitClockMHz = 0
    , atWordSelectHz = 0
    , atMasterClockMHz = 0
    , atLatencyUs = 0
    }

-- | List of all standard video timings
standardVideoTimings :: [VideoTiming]
standardVideoTimings =
  [ videoTimingFor SD480i
  , videoTimingFor SD576i
  , videoTimingFor HD720p
  , videoTimingFor HD1080i
  , videoTimingFor HD1080p
  , videoTimingFor UHD4K
  , videoTimingFor UHD4K120
  , videoTimingFor DCI4K
  , videoTimingFor UHD8K
  , videoTimingFor UHD8K120
  ]

-- | List of all standard audio timings
standardAudioTimings :: [AudioTiming]
standardAudioTimings =
  [ audioTimingFor PCM48kHz
  , audioTimingFor PCM96kHz
  , audioTimingFor PCM192kHz
  , audioTimingFor I2S
  , audioTimingFor SPDIF
  , audioTimingFor AES3
  , audioTimingFor HDMI_Audio
  ]
