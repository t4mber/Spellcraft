-- ADC-IMPLEMENTS: vhdl-analyzer-adc-003
module VHDL.Analysis.Violation
  ( detectFrequencyViolations
  ) where

import Data.Maybe (maybeToList, mapMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import VHDL.AST (Identifier)
import VHDL.Analysis.ClockGraph (ClockGraph(..), ClockNode(..), ClockEdge(..))
import VHDL.Constraint.Library (ComponentLibrary, lookupComponent)
import VHDL.Constraint.Types
  ( ConstraintViolation(..)
  , ComponentSpec(..)
  , PortConstraint(..)
  )
import VHDL.SourceLocation (SourceLocation(..))

-- | Detect frequency constraint violations in clock graph
-- Contract: vhdl-analyzer-adc-003 Section: Interface
detectFrequencyViolations
  :: ClockGraph
  -> ComponentLibrary
  -> [ConstraintViolation]
detectFrequencyViolations graph lib =
  -- Check each edge for frequency violations
  concatMap checkEdge (cgEdges graph)
  where
    checkEdge edge =
      let signal = ceFrom edge
          component = ceComponent edge
          port = ceInputPort edge
      in case (Map.lookup signal (cgNodes graph), lookupComponent component lib) of
           (Just node, Just spec) -> checkNodeAgainstSpec node component port spec
           _ -> []

-- | Check if a node violates component spec constraints
checkNodeAgainstSpec :: ClockNode -> Text -> Identifier -> ComponentSpec -> [ConstraintViolation]
checkNodeAgainstSpec node compName portName spec =
  case cnFrequency node of
    Nothing -> []  -- No frequency known, can't check
    Just freq ->
      let portConstraints = filter (\p -> portConstraintName p == portName) (compSpecPorts spec)
      in concatMap (checkFreqConstraint freq compName portName) portConstraints

-- | Check frequency against port constraint
checkFreqConstraint :: Double -> Text -> Identifier -> PortConstraint -> [ConstraintViolation]
checkFreqConstraint actualFreq compName portName constraint =
  case portConstraintMaxFreq constraint of
    Nothing -> []  -- No max frequency constraint
    Just maxFreq ->
      if actualFreq > maxFreq
        then [FrequencyViolation
               { violationComponent = compName
               , violationPort = portName
               , violationActual = actualFreq
               , violationMax = maxFreq
               , violationLocation = SourceLocation "" 0 0  -- TODO: proper location tracking
               }]
        else []
