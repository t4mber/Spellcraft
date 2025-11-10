-- ADC-IMPLEMENTS: vhdl-analyzer-adc-005
-- ADC-IMPLEMENTS: <videomancer-integration-01>
module Main (main) where

import System.Exit (exitWith)
import VHDL.CLI.Options (parseOptions, CliOptions(..), AnalysisMode(..))
import VHDL.CLI.Report (runAnalysis)
import VHDL.CLI.Videomancer (runVideomancer, VideomancerOptions(..))

-- | Main entry point
-- Contract: vhdl-analyzer-adc-005 Section: Interface
-- Enhanced: spellcraft-adc-010 Section: Tool (Videomancer routing)
main :: IO ()
main = do
  opts <- parseOptions

  -- Route to appropriate analysis mode
  exitCode <- case optAnalysisMode opts of
    StandardMode -> runAnalysis opts

    VideomancerMode configFile reportPath _ -> do
      -- Convert CLI options to VideomancerOptions
      let vmOpts = VideomancerOptions
            { voInputFiles = optInputFiles opts
            , voConfigFile = configFile
            , voReportPath = reportPath
            , voStrictMode = optStrictMode opts
            , voVerbose = optVerbose opts
            }
      runVideomancer vmOpts

  exitWith exitCode
