-- ADC-IMPLEMENTS: vhdl-analyzer-adc-002
module ComponentLibs.TestComponents
  ( testComponentLibrary
  , pll1Spec
  , ypbprEncoderSpec
  ) where

import VHDL.AST (PortDirection(..))
import VHDL.Constraint.Library (ComponentLibrary, addComponent, defaultComponentLibrary)
import VHDL.Constraint.Types
  ( ComponentSpec(..)
  , GenericConstraint(..)
  , GenericType(..)
  , PortConstraint(..)
  , Range(..)
  )

-- | Test component library for PRD examples
-- Contract: vhdl-analyzer-adc-002 Section: Examples
testComponentLibrary :: ComponentLibrary
testComponentLibrary =
  addComponent ypbprEncoderSpec $
  addComponent pll1Spec $
  defaultComponentLibrary

-- | PLL component with multiplication factor
-- Contract: vhdl-analyzer-adc-002 Section: Examples (Example 1)
pll1Spec :: ComponentSpec
pll1Spec = ComponentSpec
  { compSpecName = "PLL_1"
  , compSpecGenerics =
      [ GenericConstraint
          { genConstraintName = "MULT_FACTOR"
          , genConstraintType = RealType
          , genConstraintRange = Just (RealRange 1.0 10.0)
          }
      ]
  , compSpecPorts =
      [ PortConstraint
          { portConstraintName = "clk_in"
          , portConstraintDirection = Input
          , portConstraintMaxFreq = Just 100.0  -- 100 MHz max input
          , portConstraintFanOut = Nothing
          }
      , PortConstraint
          { portConstraintName = "clk_out"
          , portConstraintDirection = Output
          , portConstraintMaxFreq = Nothing  -- Output freq determined by calculation
          , portConstraintFanOut = Just 10
          }
      ]
  }

-- | YPbPr Encoder with max input frequency constraint
-- Contract: vhdl-analyzer-adc-002 Section: Examples (Example 2)
ypbprEncoderSpec :: ComponentSpec
ypbprEncoderSpec = ComponentSpec
  { compSpecName = "YPbPr_Encoder_A"
  , compSpecGenerics = []
  , compSpecPorts =
      [ PortConstraint
          { portConstraintName = "pixel_clk"
          , portConstraintDirection = Input
          , portConstraintMaxFreq = Just 165.0  -- Max 165 MHz - key constraint!
          , portConstraintFanOut = Nothing
          }
      ]
  }
