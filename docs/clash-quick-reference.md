# VDHL Clash Integration - Quick Reference

**ADC Contract:** vdhl-analyzer-adc-006
**Status:** ✅ COMPLETE

## Quick Start

```haskell
import VDHL.Clash.Types
import VDHL.Clash.FrequencyCheck
import VDHL.Clash.Domains
import VDHL.Clash.Constraints
```

## Common Patterns

### 1. Create a Clock Domain and Signal

```haskell
-- Create 100 MHz domain
let domain = mkClockDomain @100 "my_clock"

-- Create a signal in that domain
let signal = mkHWSignal @100 "data_bus" domain
```

### 2. Type-Safe PLL Connection

```haskell
-- Create PLL: 50 MHz * 4 = 200 MHz
let pll = mkPLL @50 @4 @200 "main_pll"

-- Connect input signal (50 MHz)
let inputSignal = mkHWSignal @50 "clk_in" inputDomain

-- Output is guaranteed to be 200 MHz
let outputSignal = connectPLL pll inputSignal :: HWSignal 200 a
```

### 3. Encoder with Frequency Constraint

```haskell
-- Create encoder with max 150 MHz
let encoder = mkEncoder @150 "encoder" 8

-- Try to connect a signal
case connectEncoder encoder signal of
  Right connectedSignal -> -- OK, constraint satisfied
  Left violation -> -- Error, frequency too high
```

### 4. Clock Domain Crossing

```haskell
-- Create crossing
let crossing = createCrossing
      "slow_to_fast"
      slowDomain
      fastDomain
      (FIFOCrossing 16)

-- Validate crossing safety
case validateCrossing crossing of
  Right validated -> -- Safe to use
  Left error -> -- Invalid crossing
```

### 5. Constraint Checking

```haskell
-- Type-level check (compile-time)
checkFrequencyConstraint @100 @50 @150 signal

-- Runtime validation
let checks = [FrequencyCheck "check" 100 (Just 50) (Just 150)]
let results = validateHardwareConstraints signal checks
if satisfiesAll results then ... else ...
```

## Type-Level Constraints

### Frequency Constraints

- `CheckMaxFreq actual max` - Enforces `actual ≤ max`
- `CheckMinFreq actual min` - Enforces `actual ≥ min`
- `FrequencyConstraint freq min max` - Enforces `min ≤ freq ≤ max`

### Usage in Type Signatures

```haskell
myFunction :: (CheckMaxFreq freq 150) => HWSignal freq a -> ...
```

## Predefined Domains

```haskell
mkSystemDomain  -- 100 MHz
mkFastDomain    -- 400 MHz
mkSlowDomain    -- 25 MHz
```

## Type Families

```haskell
-- Frequency multiplication
type Result = FreqMult 50 4  -- Result = 200

-- Frequency division
type Result = FreqDiv 200 2  -- Result = 100
```

## Integration with VDHL.Constraint.*

```haskell
-- Convert Clash result to VDHL violation
toViolation :: ConstraintResult -> Maybe ConstraintViolation

-- Import constraints from ComponentSpec
fromComponentSpec :: ComponentSpec -> [ConstraintCheck]
```

## File Locations

```
/Users/tad/t4mber/vdhl-analyzer/
├── src/VDHL/Clash/
│   ├── Types.hs           - Core types
│   ├── FrequencyCheck.hs  - Frequency validation
│   ├── Domains.hs         - Clock domains
│   └── Constraints.hs     - Constraint checking
├── examples/
│   └── ClashExample.hs    - Usage examples
└── docs/
    └── ADC-006-CLASH-INTEGRATION.md  - Full docs
```

## Example Program

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import VDHL.Clash.Types
import VDHL.Clash.FrequencyCheck

main :: IO ()
main = do
  -- Create 50 MHz input
  let inputDomain = mkClockDomain @50 "input"
  let inputSignal = mkHWSignal @50 "clk_in" inputDomain

  -- Create PLL (50 * 4 = 200)
  let pll = mkPLL @50 @4 @200 "pll"

  -- Connect - type system guarantees 200 MHz output
  let outputSignal = connectPLL pll inputSignal

  print $ domainFreqMHz $ hwSignalDomain outputSignal  -- 200
```

## Compile-Time vs Runtime

### Compile-Time (Type-Level)

```haskell
-- These constraints are checked by GHC during compilation
connectPLL :: (outFreq ~ FreqMult inFreq factor) => ...
connectEncoder :: (CheckMaxFreq freq maxFreq) => ...
checkFrequencyConstraint :: (FrequencyConstraint freq min max) => ...
```

### Runtime (Value-Level)

```haskell
-- These return Either for runtime checking
connectEncoder encoder signal :: Either ConstraintViolation (HWSignal freq a)
validateHardwareConstraints signal checks :: [ConstraintResult]
validateCrossing crossing :: Either Text (DomainCrossing srcFreq dstFreq)
```

## Constraint Combinators

```haskell
-- AND: both must be satisfied
result1 &&& result2

-- OR: at least one must be satisfied
result1 ||| result2

-- Check all
satisfiesAll [result1, result2, result3]

-- Check any
satisfiesAny [result1, result2, result3]
```

## Common Type Errors

### Error: "Couldn't match type '200' with '150'"

```haskell
-- Problem: PLL output doesn't match type
let pll = mkPLL @50 @4 @150 "pll"  -- Wrong! 50 * 4 = 200, not 150

-- Fix: Use correct output frequency
let pll = mkPLL @50 @4 @200 "pll"  -- Correct
```

### Error: "Couldn't match type ''True' with ''False'"

```haskell
-- Problem: Frequency constraint violated
connectEncoder encoder signal  -- where signal is 200 MHz and encoder max is 150

-- Fix: Use appropriate encoder or lower frequency
let encoder = mkEncoder @250 "encoder" 8  -- Increase max frequency
```

## Testing

```bash
# Build project
cabal build

# Run examples
cabal run ClashExample
```

## Dependencies Required

```cabal
build-depends:
  , clash-prelude >= 1.8
  , clash-ghc >= 1.8
```

## Extensions Required

Already enabled in project:
- DataKinds
- TypeFamilies
- TypeOperators
- KindSignatures

## See Also

- Full documentation: `docs/ADC-006-CLASH-INTEGRATION.md`
- Examples: `examples/ClashExample.hs`
- Implementation summary: `IMPLEMENTATION-SUMMARY-ADC-006.md`
