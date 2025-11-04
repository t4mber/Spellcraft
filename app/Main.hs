-- ADC-IMPLEMENTS: vhdl-analyzer-adc-005
module Main (main) where

import System.Exit (exitWith)
import VHDL.CLI.Options (parseOptions)
import VHDL.CLI.Report (runAnalysis)

-- | Main entry point
-- Contract: vhdl-analyzer-adc-005 Section: Interface
main :: IO ()
main = do
  opts <- parseOptions
  exitCode <- runAnalysis opts
  exitWith exitCode
