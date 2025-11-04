-- ADC-IMPLEMENTS: spellcraft-adc-005
module VHDL.CLI.Color
  ( -- * ANSI Colors
    colored
  , red
  , yellow
  , green
  , bold
  , detectColor
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , setSGRCode
  )
import System.IO (hIsTerminalDevice, stdout)

-- | Wrap text in ANSI color codes
-- Contract: spellcraft-adc-005 Section: Constraints
colored :: [SGR] -> Text -> IO Text
colored sgr text = do
  useColor <- detectColor
  if useColor
    then pure $ T.pack (setSGRCode sgr) <> text <> T.pack (setSGRCode [Reset])
    else pure text

-- | Red text (for errors)
red :: Text -> IO Text
red = colored [SetColor Foreground Vivid Red]

-- | Yellow text (for warnings)
yellow :: Text -> IO Text
yellow = colored [SetColor Foreground Vivid Yellow]

-- | Green text (for success)
green :: Text -> IO Text
green = colored [SetColor Foreground Vivid Green]

-- | Bold text
bold :: Text -> IO Text
bold = colored [SetConsoleIntensity BoldIntensity]

-- | Detect if we should use color output
-- Contract: spellcraft-adc-005 Section: Tests
detectColor :: IO Bool
detectColor = hIsTerminalDevice stdout
