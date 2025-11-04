# Clash Type-Level Examples

These examples demonstrate **compile-time hardware constraint checking** using Clash's type-level programming features. Each example corresponds to a VHDL file in `examples-vhdl/` and shows how the same design can be verified at compile time.

## Examples

### Example 01: PLL Violation (`01_pll_violation.hs`)
**VHDL:** `examples-vhdl/01_pll_frequency_violation.vhd`

**Design:** pixel_clk (50 MHz) → PLL (×4.16) → 208 MHz → Encoder (max 165 MHz)

**Type Error:**
```haskell
connectEncoder encoder signal208MHz
-- ERROR: Couldn't match type 'False' with 'True'
--   CheckMaxFreq 208 165 fails
```

**Key Learning:** The type checker prevents connecting a 208 MHz signal to a 165 MHz maximum component.

### Example 02: Multiple PLL Cascading (`02_multiple_pll_cascading.hs`)
**VHDL:** `examples-vhdl/02_multiple_pll_cascading.vhd`

**Design:** sys_clk (25 MHz) → PLL1 (×4) → 100 MHz → PLL2 (×3) → 300 MHz → Encoder (max 165 MHz)

**Type Error:**
```haskell
violationDesign = connectEncoder encoder stage2Clock
-- ERROR: Couldn't match type 'False' with 'True'
--   CheckMaxFreq 300 165 fails
```

**Key Learning:** Type-level arithmetic computes cascaded frequencies (25 * 4 * 3 = 300) and catches the violation.

### Example 03: Valid Design (`03_valid_design.hs`)
**VHDL:** `examples-vhdl/03_valid_design.vhd`

**Design:** pixel_clk (50 MHz) → PLL (×2) → 100 MHz → Encoder (max 165 MHz)

**Success:**
```haskell
validDesign = connectEncoder encoder outputClock
-- ✓ Type-checks: CheckMaxFreq 100 165 = True
```

**Key Learning:** Valid designs compile without errors, providing compile-time proof of correctness.

## Running the Examples

### Interactive Type Checking
```bash
cabal repl
```

```haskell
:load examples-clash/01_pll_violation.hs

-- Try to create a violation (this will fail):
let encoder = mkEncoder @165 "Enc" 8
let domain = mkClockDomain @208 "high"
let sig = mkHWSignal @208 "test" domain
connectEncoder encoder sig
-- ERROR: Type checker rejects this!
```

### Automated Testing
```bash
cabal test
```

The test suite (`test/VHDL/Clash/ExampleValidationSpec.hs`) verifies:
- ✅ Frequency calculations are correct
- ✅ Type-level arithmetic works
- ✅ Runtime validation catches violations
- ✅ Valid designs succeed

## Type-Level vs Runtime Checking

| Approach | When | Example | Status |
|----------|------|---------|--------|
| **Type-Level (Clash)** | Compile time | These examples | ✅ Working |
| **Runtime (Spellcraft)** | Parse time | `examples-vhdl/*.vhd` | ❌ In progress |

### Type-Level Advantages
- ✅ **Catch errors at compile time** - Before any code runs
- ✅ **Zero runtime cost** - All checking done by compiler
- ✅ **Impossible to violate** - Type system prevents invalid code
- ✅ **Clear error messages** - GHC tells you exactly what's wrong

### Type-Level Limitations
- ❌ Only works for Haskell/Clash code
- ❌ Can't analyze existing VHDL files
- ❌ Requires rewriting designs in Clash

## Example: Type Error in Action

```bash
$ cabal repl
> :set -XDataKinds -XTypeApplications
> import VHDL.Clash.Types
> import VHDL.Clash.FrequencyCheck

-- Try to create PLL with wrong output frequency
> mkPLL @50 @4 @180 "BadPLL"

<interactive>:5:1: error:
    • Couldn't match type '180' with '200'
        arising from a use of 'mkPLL'
    • In the expression: mkPLL @50 @4 @180 "BadPLL"

-- The type checker knows: 50 * 4 = 200, not 180!
```

## Comparison with VHDL Examples

| File | VHDL (Runtime) | Clash (Compile-Time) |
|------|----------------|----------------------|
| 01_pll_violation | `./spellcraft` should detect | Type error |
| 02_multiple_pll_cascading | `./spellcraft` should detect | Type error |
| 03_valid_design | No violation | ✅ Compiles |

**Current Status:**
- ✅ Clash examples: Type checking works perfectly
- ❌ VHDL analyzer: Not detecting violations yet (work in progress)

## Architecture

See `docs/ARCHITECTURE-TYPE-LEVEL-VS-RUNTIME.md` for detailed discussion of:
- Type-level vs runtime checking
- Why they're separate systems
- Potential bridge between them
- Dependent types approach

## Contract Compliance

These examples demonstrate **ADC-006** (Clash Type-Level Hardware Constraint Modeling):

✅ **Purpose Achieved:** "Success is achieved when frequency mismatches can be detected by the Haskell type checker"

**Evidence:** Try uncommenting the violation examples and observe the compile errors!

## Next Steps

1. **Fix Spellcraft** - Make runtime checking work (ADC-007)
2. **Bridge Systems** - Connect type-level and runtime checking
3. **More Examples** - Add clock domain crossing, timing constraints
4. **Documentation** - Expand type-level programming guide

---

**Generated:** 2025-11-03
**Contract:** vdhl-analyzer-adc-006
**Status:** Type-level checking fully operational ✅
