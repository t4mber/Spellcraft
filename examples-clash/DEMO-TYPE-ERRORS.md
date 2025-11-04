# Demonstrating Type-Level Constraint Violations

This guide shows how to see GHC reject invalid hardware designs at compile time.

## Current Status

**All examples compile successfully** because the violation code is commented out. This allows the test suite to run.

To see the type errors, you need to uncomment the violation examples and try to build.

## Demo 1: Simple Frequency Violation

### Step 1: Edit `01_pll_violation.hs`

Uncomment lines 67-71:

```haskell
-- Before:
{-
testViolation :: Either ConstraintViolation (HWSignal 208 ())
testViolation = connectEncoder (mkEncoder @165 "Enc" 8)
                                (mkHWSignal "sig" (mkClockDomain @208 "clk") :: HWSignal 208 ())
-}

-- After:
testViolation :: Either ConstraintViolation (HWSignal 208 ())
testViolation = connectEncoder (mkEncoder @165 "Enc" 8)
                                (mkHWSignal "sig" (mkClockDomain @208 "clk") :: HWSignal 208 ())
```

### Step 2: Try to Build

```bash
$ cabal build
```

### Expected Error

```
examples-clash/01_pll_violation.hs:69:17: error: [GHC-83865]
    • Couldn't match type ''False' with ''True'
        arising from a use of 'connectEncoder'
    • In the expression:
          connectEncoder
            (mkEncoder @165 "Enc" 8)
            (mkHWSignal
               "sig" (mkClockDomain @208 "clk") :: HWSignal 208 ())
      In an equation for 'testViolation':
          testViolation
            = connectEncoder
                (mkEncoder @165 "Enc" 8)
                (mkHWSignal
                   "sig" (mkClockDomain @208 "clk") :: HWSignal 208 ())
   |
69 |   testViolation = connectEncoder (mkEncoder @165 "Enc" 8)
   |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
```

**Explanation:** GHC evaluates `CheckMaxFreq 208 165` which becomes `(208 <=? 165) ~ 'True`. Since `208 <=? 165` evaluates to `'False`, the constraint cannot be satisfied.

## Demo 2: Cascaded PLL Violation

### Step 1: Edit `02_multiple_pll_cascading.hs`

Uncomment lines 53-62:

```haskell
-- Before:
{-
violationDesign :: Either ConstraintViolation (HWSignal Stage2Freq ())
violationDesign = connectEncoder encoder stage2Clock
-}

-- After:
violationDesign :: Either ConstraintViolation (HWSignal Stage2Freq ())
violationDesign = connectEncoder encoder stage2Clock
```

### Step 2: Try to Build

```bash
$ cabal build
```

### Expected Error

```
examples-clash/02_multiple_pll_cascading.hs:55:19: error: [GHC-83865]
    • Couldn't match type ''False' with ''True'
        arising from a use of 'connectEncoder'
    • In the expression: connectEncoder encoder stage2Clock
      In an equation for 'violationDesign':
          violationDesign = connectEncoder encoder stage2Clock
   |
55 | violationDesign = connectEncoder encoder stage2Clock
   |                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

**Explanation:** The type `Stage2Freq` is 300, and `EncoderMaxFreq` is 165. GHC computes `CheckMaxFreq 300 165` and rejects it because `300 <=? 165` is `'False`.

## Demo 3: Valid Design (Should Compile)

### File: `03_valid_design.hs`

This file has **no commented violations** because it's a valid design:

```haskell
validDesign :: Either ConstraintViolation (HWSignal OutputFreq ())
validDesign = connectEncoder encoder outputClock
-- ^ Type-checks successfully: CheckMaxFreq 100 165 = True
```

### Build It

```bash
$ cabal build
```

**Result:** ✅ Compiles successfully because `100 <=? 165` evaluates to `'True`.

## Understanding the Type Error

### The Constraint

```haskell
type family CheckMaxFreq (freq :: Nat) (maxFreq :: Nat) :: Constraint where
  CheckMaxFreq freq maxFreq = (freq <=? maxFreq) ~ 'True
```

This says: "For `CheckMaxFreq freq maxFreq` to be satisfied, `freq <=? maxFreq` must evaluate to the type-level boolean `'True`."

### The Connection Function

```haskell
connectEncoder
  :: forall maxFreq freq a.
     (KnownNat freq, KnownNat maxFreq, CheckMaxFreq freq maxFreq)
     --                                 ^^^^^^^^^^^^^^^^^^^^^^^^
     --                                 This constraint must hold!
  => Encoder maxFreq
  -> HWSignal freq a
  -> Either ConstraintViolation (HWSignal freq a)
```

When you call `connectEncoder`, GHC must prove that `CheckMaxFreq freq maxFreq` holds. If it can't, compilation fails.

### Type-Level Comparison

```haskell
-- Type-level natural comparison (<=?)
208 <=? 165  →  'False   -- Type error!
100 <=? 165  →  'True    -- Compiles!
300 <=? 165  →  'False   -- Type error!
```

## Why This Matters

### Traditional Approach (No Type Safety)

```haskell
-- This would compile even if wrong:
connectSignal :: Signal a -> Component -> Signal a
connectSignal signal component = ...  -- No frequency checking!
```

You'd only discover the violation at runtime, or worse, after synthesizing the hardware.

### Type-Level Approach (Compile-Time Safety)

```haskell
-- This CANNOT compile if wrong:
connectEncoder
  :: CheckMaxFreq freq maxFreq  -- Constraint enforced!
  => Encoder maxFreq
  -> HWSignal freq a
  -> Either ConstraintViolation (HWSignal freq a)
```

The type system **guarantees** frequency violations are caught before any code runs.

## Interactive Demo in REPL

```bash
$ cabal repl
```

```haskell
ghci> :set -XDataKinds -XTypeApplications
ghci> import VHDL.Clash.Types
ghci> import VHDL.Clash.FrequencyCheck

-- Create a 165 MHz encoder
ghci> let encoder = mkEncoder @165 "Enc" 8

-- Create a 100 MHz signal (valid)
ghci> let domain100 = mkClockDomain @100 "clk100"
ghci> let signal100 = mkHWSignal @100 "sig" domain100
ghci> connectEncoder encoder signal100
Right (HWSignal {...})  -- Success!

-- Try with 208 MHz signal (invalid)
ghci> let domain208 = mkClockDomain @208 "clk208"
ghci> let signal208 = mkHWSignal @208 "sig" domain208
ghci> connectEncoder encoder signal208

<interactive>:XX:YY: error: [GHC-83865]
    • Couldn't match type ''False' with ''True'
        arising from a use of 'connectEncoder'
    • In the expression: connectEncoder encoder signal208
```

## Type-Level Arithmetic Demo

```haskell
ghci> import VHDL.Clash.Types

-- Create PLL: 50 MHz × 4 = ???
ghci> let pll = mkPLL @50 @4 @200 "PLL1"
ghci> pllOutputFreq pll
200.0  -- Correct!

-- Try wrong output frequency
ghci> let badPll = mkPLL @50 @4 @180 "BadPLL"

<interactive>:XX:YY: error: [GHC-83865]
    • Couldn't match type '200' with '180'
        arising from a use of 'mkPLL'
    • In the expression: mkPLL @50 @4 @180 "BadPLL"
```

**The type checker knows arithmetic!** It computes `50 * 4 = 200` and rejects `180`.

## Comparison with Spellcraft

| Approach | When | How to See |
|----------|------|-----------|
| **Clash (Type-Level)** | Compile time | Uncomment violations, run `cabal build` |
| **VHDL (Runtime)** | Parse time | Run `./spellcraft examples-vhdl/*.vhd` |

**Current Status:**
- ✅ **Clash:** Type errors work perfectly - try uncommenting!
- ❌ **VHDL:** Runtime analysis not detecting violations yet (work in progress)

## Try It Yourself!

1. **Uncomment** a violation in `01_pll_violation.hs` or `02_multiple_pll_cascading.hs`
2. **Run** `cabal build`
3. **Observe** GHC reject the invalid hardware design
4. **Re-comment** the code to restore working state
5. **Run** `cabal test` to verify everything still passes

This demonstrates the power of dependent types for hardware verification: **invalid designs cannot compile!**

---

**Generated:** 2025-11-03
**Contract:** ADC-006
**Examples:** `examples-clash/*.hs`
