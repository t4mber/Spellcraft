{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-007
module VHDL.Analysis.FrequencyParser
  ( parseFrequencyFromText
  , parseFrequencyFromComment
  ) where

import Data.Char (isDigit, isSpace)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | Parse frequency from comment text
-- Contract: spellcraft-adc-007 Section: Interface
parseFrequencyFromComment :: Text -> Maybe Double
parseFrequencyFromComment = parseFrequencyFromText

-- | Parse frequency from any text containing frequency specification
-- Supports formats: "25 MHz", "50MHz", "100 MHZ", "25.0 MHz", "1.5GHz"
-- Contract: spellcraft-adc-007 Section: Frequency Parsing
parseFrequencyFromText :: Text -> Maybe Double
parseFrequencyFromText text =
  let words' = T.words text
      -- Find patterns like "50MHz" or "50 MHz"
      freqCandidates = concatMap extractFreqFromWord words'
  in listToMaybe freqCandidates

-- | Extract frequency from a single word or adjacent words
extractFreqFromWord :: Text -> [Double]
extractFreqFromWord word
  | T.null word = []
  | otherwise =
      -- Try to split into number and unit
      let (numPart, unitPart) = T.span (\c -> isDigit c || c == '.') word
      in case (parseNumber numPart, parseUnit unitPart) of
           (Just num, Just multiplier) -> [num * multiplier]
           _ -> []

-- | Parse number from text
parseNumber :: Text -> Maybe Double
parseNumber text
  | T.null text = Nothing
  | otherwise = readMaybe (T.unpack text)

-- | Parse unit and return multiplier for MHz
parseUnit :: Text -> Maybe Double
parseUnit unit =
  let lowerUnit = T.toLower $ T.filter (not . isSpace) unit
  in case lowerUnit of
       u | "mhz" `T.isPrefixOf` u -> Just 1.0
       u | "ghz" `T.isPrefixOf` u -> Just 1000.0
       u | "khz" `T.isPrefixOf` u -> Just 0.001
       _ -> Nothing
