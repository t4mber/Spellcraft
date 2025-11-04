{-# LANGUAGE OverloadedStrings #-}

-- | Runtime analysis of Clash source files
-- Compiles Clash files with GHC to catch type-level constraint violations
module VHDL.Analysis.ClashFile
  ( analyzeClashFile
  , ClashAnalysisResult(..)
  , ClashViolation(..)
  , clashViolationToConstraint
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)
import Debug.Trace (trace)
import VHDL.Constraint.Types (ConstraintViolation(..))
import VHDL.SourceLocation (SourceLocation, mkSourceLocation)

-- | Result of analyzing a Clash file
data ClashAnalysisResult = ClashAnalysisResult
  { carFilePath :: FilePath
  , carViolations :: [ClashViolation]
  , carConnections :: [(Text, Integer, Integer)]  -- (component, input freq, max freq)
  } deriving (Show, Eq)

-- | Clash-specific violation
data ClashViolation = ClashViolation
  { cvComponent :: Text
  , cvActualFreq :: Integer
  , cvMaxFreq :: Integer
  , cvLocation :: SourceLocation
  } deriving (Show, Eq)

-- | Analyze a Clash source file by compiling it with Stack
analyzeClashFile :: FilePath -> IO (Either String ClashAnalysisResult)
analyzeClashFile filePath = do
  -- Try to compile the file with stack ghc
  result <- try $ readProcessWithExitCode "stack" ["ghc", "--", "-fno-code", "-fforce-recomp", filePath] ""
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "Failed to run stack ghc: " ++ show e
    Right (exitCode, stdout, stderr) -> do
      let output = stdout ++ stderr
          violations = parseGHCErrors filePath output
      pure $ Right $ ClashAnalysisResult
        { carFilePath = filePath
        , carViolations = violations
        , carConnections = []  -- Not needed for compiler-based analysis
        }

-- | Parse GHC error messages for type-level constraint violations
parseGHCErrors :: FilePath -> String -> [ClashViolation]
parseGHCErrors filePath output =
  let textOutput = T.pack output
      errorBlocks = T.splitOn (T.pack "\n\n") textOutput
  in concatMap (\block ->
       let result = parseErrorBlock filePath block
       in if not (null result)
          then result
          else result
     ) errorBlocks

-- | Parse a single error block
parseErrorBlock :: FilePath -> Text -> [ClashViolation]
parseErrorBlock filePath block
  | "Couldn't match type" `T.isInfixOf` block &&
    "False" `T.isInfixOf` block &&
    "True" `T.isInfixOf` block &&
    "connectEncoder" `T.isInfixOf` block = do
      -- This is a frequency constraint violation from connectEncoder
      case extractLocationFromError block of
        Just (line, col) ->
          [ClashViolation
            { cvComponent = "Type-level frequency constraint violation"
            , cvActualFreq = 0  -- Frequencies are in type-level constraint
            , cvMaxFreq = 0     -- Check source code for @Freq type applications
            , cvLocation = mkSourceLocation filePath line col
            }]
        Nothing -> []
  | "Couldn't match type" `T.isInfixOf` block &&
    ("CheckMaxFreq" `T.isInfixOf` block || "FrequencyCheck" `T.isInfixOf` block) = do
      -- This is a frequency constraint violation with frequencies in error
      case extractLocationAndFrequencies block of
        Just (line, col, actual, maxFreq) ->
          [ClashViolation
            { cvComponent = "Type-level constraint"
            , cvActualFreq = actual
            , cvMaxFreq = maxFreq
            , cvLocation = mkSourceLocation filePath line col
            }]
        Nothing -> []
  | otherwise = []

-- | Extract just location from GHC error
extractLocationFromError :: Text -> Maybe (Int, Int)
extractLocationFromError block =
  case T.breakOn (T.pack ":") block of
    (_, rest1) | not (T.null rest1) ->
      let afterFirst = T.drop 1 rest1
      in case T.breakOn (T.pack ":") afterFirst of
        (lineText, rest2) | not (T.null rest2) ->
          let afterSecond = T.drop 1 rest2
              colText = T.takeWhile (/= ':') afterSecond
          in case (readMaybe (T.unpack lineText), readMaybe (T.unpack colText)) of
            (Just l, Just c) -> Just (l, c)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- | Extract location and frequencies from GHC error
extractLocationAndFrequencies :: Text -> Maybe (Int, Int, Integer, Integer)
extractLocationAndFrequencies block = do
  -- Extract line:col from pattern like "file.hs:58:3:"
  lineCol <- extractLineCol block
  -- Extract frequencies from "CheckMaxFreq 208 165" pattern
  freqs <- extractCheckMaxFreq block
  pure (fst lineCol, snd lineCol, fst freqs, snd freqs)
  where
    extractLineCol txt =
      case T.breakOn (T.pack ":") txt of
        (_, rest1) | not (T.null rest1) ->
          let afterFirst = T.drop 1 rest1
          in case T.breakOn (T.pack ":") afterFirst of
            (lineText, rest2) | not (T.null rest2) ->
              let afterSecond = T.drop 1 rest2
                  colText = T.takeWhile (/= ':') afterSecond
              in case (readMaybe (T.unpack lineText), readMaybe (T.unpack colText)) of
                (Just l, Just c) -> Just (l, c)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

    extractCheckMaxFreq txt =
      case T.breakOn (T.pack "CheckMaxFreq") txt of
        (_, rest) | not (T.null rest) ->
          let afterCheck = T.drop (T.length "CheckMaxFreq") rest
              nums = T.words afterCheck
          in case nums of
            (n1:n2:_) ->
              case (readMaybe (T.unpack n1), readMaybe (T.unpack n2)) of
                (Just a, Just m) -> Just (a, m)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

-- | Convert Clash violations to constraint violations
clashViolationToConstraint :: ClashViolation -> ConstraintViolation
clashViolationToConstraint cv = FrequencyViolation
  { violationLocation = cvLocation cv
  , violationComponent = cvComponent cv
  , violationPort = "type_check"
  , violationActual = fromInteger (max 1 (cvActualFreq cv))
  , violationMax = fromInteger (max 1 (cvMaxFreq cv))
  }
