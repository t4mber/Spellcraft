-- ADC-IMPLEMENTS: spellcraft-adc-003
module VHDL.Analysis.Violation
  ( detectFrequencyViolations
  ) where

import Data.Maybe (maybeToList, mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import VHDL.AST (Identifier, ComponentInst(..), SignalName, PortDirection(..), Expression(..))
import VHDL.Analysis.ClockGraph (ClockGraph(..), ClockNode(..), ClockEdge(..))
import VHDL.Constraint.Library (ComponentLibrary, lookupComponent)
import VHDL.Constraint.Types
  ( ConstraintViolation(..)
  , ComponentSpec(..)
  , PortConstraint(..)
  )
import VHDL.SourceLocation (SourceLocation(..))

-- | Detect frequency constraint violations in clock graph
-- Contract: spellcraft-adc-003 Section: Interface
detectFrequencyViolations
  :: ClockGraph
  -> ComponentLibrary
  -> [ConstraintViolation]
detectFrequencyViolations graph lib =
  -- Check all component instances, not just edges
  -- This catches endpoints like encoders that don't produce clock outputs
  concatMap checkComponentInstance (cgComponents graph)
  where
    checkComponentInstance comp =
      let compName = compInstName comp
          compType = compComponentName comp
          compLoc = compLocation comp  -- Get source location from component
      in case lookupComponent compType lib of
           Nothing -> []
           Just spec ->
             -- ADC-IMPLEMENTS: spellcraft-adc-021
             -- Check all input ports with clock constraints
             -- Port map now contains expressions, extract signal identifiers
             let inputPorts = [(port, sig) | (port, expr) <- compPortMap comp
                                            , Just sig <- [exprToSignalName expr]
                                            , hasClockConstraint port spec]
             in concatMap (checkPort compName compLoc spec) inputPorts

    checkPort compName compLoc spec (portName, signalName) =
      case Map.lookup signalName (cgNodes graph) of
        Nothing -> []
        Just node -> checkNodeAgainstSpec node compName portName spec compLoc

    -- ADC-IMPLEMENTS: spellcraft-adc-021
    -- Extract signal name from expression (for violation detection)
    exprToSignalName (IdentifierExpr name) = Just name
    exprToSignalName _ = Nothing  -- Complex expressions don't represent single signals

    -- Check if a port has frequency constraints
    hasClockConstraint portName spec =
      any (\p -> portConstraintName p == portName &&
                 portConstraintDirection p == Input &&
                 maybe False (const True) (portConstraintMaxFreq p))
          (compSpecPorts spec)

-- | Check if a node violates component spec constraints
checkNodeAgainstSpec :: ClockNode -> Text -> Identifier -> ComponentSpec -> SourceLocation -> [ConstraintViolation]
checkNodeAgainstSpec node compName portName spec compLoc =
  case cnFrequency node of
    Nothing -> []  -- No frequency known, can't check
    Just freq ->
      let portConstraints = filter (\p -> portConstraintName p == portName) (compSpecPorts spec)
      in concatMap (checkFreqConstraint freq compName portName compLoc) portConstraints

-- | Check frequency against port constraint
checkFreqConstraint :: Double -> Text -> Identifier -> SourceLocation -> PortConstraint -> [ConstraintViolation]
checkFreqConstraint actualFreq compName portName compLoc constraint =
  case portConstraintMaxFreq constraint of
    Nothing -> []  -- No max frequency constraint
    Just maxFreq ->
      if actualFreq > maxFreq
        then [FrequencyViolation
               { violationComponent = compName
               , violationPort = portName
               , violationActual = actualFreq
               , violationMax = maxFreq
               , violationLocation = compLoc  -- Use component instantiation location
               }]
        else []
