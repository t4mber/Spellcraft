{-# LANGUAGE DeriveGeneric #-}

-- ADC-IMPLEMENTS: spellcraft-adc-001
module VHDL.SourceLocation
  ( SourceLocation(..)
  , mkSourceLocation
  , formatLocation
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Source code location for error reporting
-- Contract: spellcraft-adc-001 Section: Interface
data SourceLocation = SourceLocation
  { locFile :: FilePath
  , locLine :: Int
  , locColumn :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SourceLocation

-- | Create a source location with validation
mkSourceLocation :: FilePath -> Int -> Int -> SourceLocation
mkSourceLocation file line col = SourceLocation
  { locFile = file
  , locLine = max 1 line
  , locColumn = max 1 col
  }

-- | Format location as "file:line:column"
formatLocation :: SourceLocation -> Text
formatLocation loc = T.pack $ concat
  [ locFile loc
  , ":"
  , show (locLine loc)
  , ":"
  , show (locColumn loc)
  ]
