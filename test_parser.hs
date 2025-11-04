#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec, text, vdhl-analyzer
-}

import qualified Data.Text.IO as TIO
import Text.Megaparsec
import VDHL.Lexer
import VDHL.Parser

main :: IO ()
main = do
  let testInput = "entity test_ports is\n  port (\n    clk : in std_logic\n  );\nend entity test_ports;\n"
  putStrLn "Testing parser with:"
  putStrLn testInput
  putStrLn "---"
  case parseVDHLText testInput "test.vhd" of
    Left err -> do
      putStrLn "Parse failed:"
      print err
    Right design -> do
      putStrLn "Parse succeeded:"
      print design
