-- ADC-IMPLEMENTS: vhdl-analyzer-adc-002
module VHDL.Constraint.Library
  ( -- * Component Library
    ComponentLibrary
  , defaultComponentLibrary
  , lookupComponent
  , addComponent
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import VHDL.Constraint.Types (ComponentSpec(..))

-- | Component library (map from name to spec)
-- Contract: vhdl-analyzer-adc-002 Section: Interface
type ComponentLibrary = Map Text ComponentSpec

-- | Default component library (empty for now, populated by TestComponents)
-- Contract: vhdl-analyzer-adc-002 Section: Interface
defaultComponentLibrary :: ComponentLibrary
defaultComponentLibrary = Map.empty

-- | Look up a component by name
-- Contract: vhdl-analyzer-adc-002 Section: Interface
lookupComponent :: Text -> ComponentLibrary -> Maybe ComponentSpec
lookupComponent = Map.lookup

-- | Add a component to the library
-- Contract: vhdl-analyzer-adc-002 Section: Interface
addComponent :: ComponentSpec -> ComponentLibrary -> ComponentLibrary
addComponent spec lib = Map.insert (compSpecName spec) spec lib
