# ADC-006: Clash Type-Level Hardware Constraint Modeling

**Contract ID:** vdhl-analyzer-adc-006
**Status:** Implemented
**Implementation Date:** 2025-11-03

## Overview

This document describes the implementation of ADC contract vdhl-analyzer-adc-006, which provides Clash integration for type-level hardware constraint modeling in the VDHL analyzer.

## Architecture

The Clash integration consists of four main modules:

```
src/VDHL/Clash/
├── Types.hs             - Core type-level hardware types
├── FrequencyCheck.hs    - Type-level frequency validation
├── Domains.hs           - Clock domain management
└── Constraints.hs       - Type-level constraint checking
```

## Module Descriptions

### 1. VDHL.Clash.Types

**File:** `/Users/tad/t4mber/vdhl-analyzer/src/VDHL/Clash/Types.hs`

Core type-level hardware types using GHC's type-level naturals:

- **`FreqMHz`**: Type-level frequency representation (type alias for `Nat`)
- **`FreqMult`**: Type family for frequency multiplication
- **`FreqDiv`**: Type family for frequency division
- **`ClockDomain freq`**: Clock domain tagged with frequency
- **`HWSignal freq a`**: Hardware signal with type-level frequency
- **`PLL inFreq factor`**: Phase-locked loop component
- **`Encoder maxFreq`**: Encoder with maximum frequency constraint

**Key Features:**
- Frequencies are encoded at the type level for compile-time validation
- Smart constructors automatically extract type-level information
- Type families enable frequency arithmetic at compile time

**Example:**
```haskell
-- Create a 100 MHz clock domain
let domain = mkClockDomain @100 "system_clk"

-- Create a signal in that domain
let signal = mkHWSignal @100 "data_bus" domain
```

### 2. VDHL.Clash.FrequencyCheck

**File:** `/Users/tad/t4mber/vdhl-analyzer/src/VDHL/Clash/FrequencyCheck.hs`

Type-level frequency validation and safe connection functions:

- **`CheckMaxFreq`**: Type-level constraint ensuring frequency ≤ max
- **`CheckMinFreq`**: Type-level constraint ensuring frequency ≥ min
- **`CheckFreqRange`**: Type-level constraint for frequency ranges
- **`connectPLL`**: Type-safe PLL connection (enforces frequency arithmetic)
- **`connectEncoder`**: Safe encoder connection with constraint checking
- **`connectClockDivider`**: Type-safe clock division

**Key Features:**
- Compile-time frequency constraint checking
- Safe connection functions that enforce correct frequency relationships
- Runtime validation with detailed error messages

**Example:**
```haskell
-- Create PLL that multiplies 50 MHz by 4 to get 200 MHz
let pll = mkPLL @50 @4 @200 "main_pll"

-- Connect input signal - output type guarantees 200 MHz
let output = connectPLL pll inputSignal :: HWSignal 200 a
```

### 3. VDHL.Clash.Domains

**File:** `/Users/tad/t4mber/vdhl-analyzer/src/VDHL/Clash/Domains.hs`

Clock domain management and crossing validation:

- **`DomainRegistry`**: Registry for tracking clock domains
- **`DomainRelation`**: Describes relationship between domains (Synchronous, Rational, Asynchronous)
- **`DomainCrossing`**: Specification for clock domain crossings
- **`CrossingStrategy`**: Strategies for crossing (Direct, FIFO, Handshake)
- Predefined domains: `System` (100 MHz), `Fast` (400 MHz), `Slow` (25 MHz)

**Key Features:**
- Automatic detection of domain relationships
- Safe domain crossing validation
- Registry-based domain tracking

**Example:**
```haskell
-- Create domains
let slowDomain = mkSlowDomain    -- 25 MHz
let fastDomain = mkFastDomain    -- 400 MHz

-- Check relationship
let relation = relateDomains slowDomain fastDomain

-- Create and validate crossing
let crossing = createCrossing "slow_to_fast" slowDomain fastDomain (FIFOCrossing 16)
case validateCrossing crossing of
  Right _ -> putStrLn "Safe crossing"
  Left err -> putStrLn $ "Invalid: " ++ show err
```

### 4. VDHL.Clash.Constraints

**File:** `/Users/tad/t4mber/vdhl-analyzer/src/VDHL/Clash/Constraints.hs`

Type-level constraint checking and integration with existing VDHL types:

- **`FrequencyConstraint`**: Type-level frequency range constraint
- **`PowerConstraint`**: Type-level power-based frequency limit
- **`validateHardwareConstraints`**: Runtime constraint validation
- **`checkFrequencyConstraint`**: Type-level frequency checking
- **`checkPowerConstraint`**: Type-level power checking
- Constraint combinators: `&&&` (AND), `|||` (OR)
- Integration functions: `toViolation`, `fromComponentSpec`

**Key Features:**
- Seamless integration with `VDHL.Constraint.Types`
- Converts type-level checks to `ConstraintViolation` types
- Supports constraint composition with combinators

**Example:**
```haskell
-- Type-level frequency constraint (50 ≤ freq ≤ 150)
checkFrequencyConstraint @100 @50 @150 signal :: Either ConstraintViolation ()

-- Runtime constraint validation
let checks = [FrequencyCheck "check1" 100 (Just 50) (Just 150)]
let results = validateHardwareConstraints signal checks
if satisfiesAll results then ... else ...
```

## Integration with Existing VDHL Modules

### Integration Points

1. **VDHL.Constraint.Types**
   - `ConstraintViolation` is used by Clash modules
   - `ComponentSpec` can be converted to Clash constraint checks
   - Seamless interoperability between runtime and type-level checks

2. **VDHL.Constraint.Violation**
   - Clash checks produce compatible `ConstraintViolation` values
   - Type-level violations include source location information

3. **VDHL.SourceLocation**
   - Used in all violation reporting
   - Enables tracking of constraint violations to source

## Type-Level Safety Guarantees

The Clash integration provides compile-time guarantees for:

1. **Frequency Arithmetic**
   ```haskell
   -- Guarantees: outFreq = inFreq * factor
   connectPLL :: (outFreq ~ FreqMult inFreq factor) => ...
   ```

2. **Frequency Constraints**
   ```haskell
   -- Guarantees: freq ≤ maxFreq
   connectEncoder :: (CheckMaxFreq freq maxFreq) => ...
   ```

3. **Domain Relationships**
   ```haskell
   -- Enforces correct output frequency after division
   connectClockDivider :: (outFreq ~ FreqDiv inFreq divisor) => ...
   ```

## Dependencies

Added to cabal file:
```cabal
build-depends:
  , clash-prelude >= 1.8
  , clash-ghc >= 1.8
```

## GHC Extensions Required

Already enabled in the project:
- `DataKinds` - For type-level literals
- `TypeFamilies` - For type-level functions
- `TypeOperators` - For type-level operators
- `KindSignatures` - For explicit kind signatures

Additional extensions used in modules:
- `UndecidableInstances` - For type family evaluation
- `AllowAmbiguousTypes` - For type applications
- `ConstraintKinds` - For constraint synonyms

## Example Usage

See `/Users/tad/t4mber/vdhl-analyzer/examples/ClashExample.hs` for comprehensive examples including:

1. Type-safe PLL connection
2. Encoder with frequency constraint checking
3. Clock domain crossing validation
4. Constraint validation and composition
5. Type-level frequency arithmetic
6. Integration with existing VDHL constraints

## Usage Examples

### Example 1: Safe PLL Connection

```haskell
-- Create 50 MHz input
let inputDomain = mkClockDomain @50 "input"
let inputSignal = mkHWSignal @50 "clk_in" inputDomain

-- Create PLL (50 * 4 = 200)
let pll = mkPLL @50 @4 @200 "main_pll"

-- Connect - output type is HWSignal 200 a
let outputSignal = connectPLL pll inputSignal
```

### Example 2: Frequency Constraint Checking

```haskell
-- Create signal
let signal = mkHWSignal @100 "data" systemDomain

-- Create encoder with max 150 MHz
let encoder = mkEncoder @150 "encoder" 8

-- Try to connect (succeeds: 100 ≤ 150)
case connectEncoder encoder signal of
  Right _ -> putStrLn "OK"
  Left violation -> print violation
```

### Example 3: Domain Crossing

```haskell
-- Create crossing
let crossing = createCrossing
      "slow_to_fast"
      slowDomain
      fastDomain
      (FIFOCrossing 16)

-- Validate
case validateCrossing crossing of
  Right validCrossing -> -- use crossing
  Left err -> -- handle error
```

## Testing

To test the Clash integration:

```bash
# Build the project
cabal build

# Run the example
cabal run ClashExample
```

The example demonstrates:
- All type-level constraint checking features
- Integration with existing VDHL types
- Runtime validation
- Compile-time safety

## Contract Compliance

This implementation satisfies all requirements of ADC contract vdhl-analyzer-adc-006:

- ✅ Four modules created in `src/VDHL/Clash/`
- ✅ All required types implemented (`FreqMHz`, `FreqMult`, `FreqDiv`, etc.)
- ✅ Safe connection functions (`connectPLL`, `connectEncoder`)
- ✅ Type-level constraint checking (`CheckMaxFreq`, etc.)
- ✅ Clash dependencies added (clash-prelude >= 1.8, clash-ghc >= 1.8)
- ✅ Required GHC extensions enabled
- ✅ ADC-IMPLEMENTS markers in all modules
- ✅ Integration with existing VDHL.Constraint.* modules

## Future Enhancements

Potential future improvements:

1. **Additional Hardware Components**
   - Clock dividers with type-level division
   - Delay lines with latency tracking
   - Serializers/deserializers with rate tracking

2. **Enhanced Constraints**
   - Power consumption modeling
   - Thermal constraints
   - Signal integrity checks

3. **Visualization**
   - Generate clock domain crossing diagrams
   - Visualize frequency relationships
   - Export to hardware design tools

4. **Static Analysis**
   - Detect potential metastability issues
   - Analyze clock domain crossing safety
   - Generate timing reports

## License

BSD-3-Clause (same as parent project)

## Author

ADC Code Generator
Implementation Date: 2025-11-03
