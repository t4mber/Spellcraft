# Architecture: Type-Level vs Runtime Analysis

**Date:** 2025-11-03
**Status:** Design Discussion
**Question:** Should we use dependent types for VHDL frequency analysis?

---

## The Core Question

> "Are we using dependent types for analyzing source range and destination range flow?"

**Short Answer:** No, and we have two approaches that are currently disconnected:

1. **Type-Level (Clash - ADC-006)**: Works in isolation ✅
2. **Runtime (Spellcraft - ADC-003/007)**: Currently broken ❌

---

## Current Architecture

### System 1: Type-Level Framework (Clash)

**Location:** `src/VHDL/Clash/*.hs`
**Status:** ✅ Working (tested with 15/15 passing tests)

```haskell
-- Frequencies are TYPE-LEVEL values
pll :: PLL 50 4                    -- Input: 50 MHz, multiply by 4
signal :: HWSignal 200 Bit         -- Output: 200 MHz

-- Type checker prevents violations at COMPILE TIME
encoder :: Encoder 165
badConnection = connectEncoder encoder signal200MHz
-- ERROR: Couldn't match type 'False' with 'True'
--   arising from: CheckMaxFreq 200 165
```

**Advantages:**
- ✅ Errors caught at compile time
- ✅ Zero runtime overhead
- ✅ Impossible to violate constraints
- ✅ Type checker does the work

**Limitations:**
- ❌ Only works for Haskell code
- ❌ Can't analyze VHDL files
- ❌ Requires users to write Clash/Haskell

### System 2: Runtime Analysis (VHDL Parser)

**Location:** `src/VHDL/Analysis/*.hs`
**Status:** ❌ Broken (edge creation incomplete)

```haskell
-- Frequencies are RUNTIME values
node :: ClockNode
node = ClockNode { cnFrequency = Just 200.0 }

-- Runtime checking
if actualFreq > maxFreq
  then Left (FrequencyViolation ...)
  else Right ()
```

**Advantages:**
- ✅ Can analyze existing VHDL files
- ✅ Works with any VHDL (no rewrite needed)
- ✅ Simpler engineering (no type-level magic)

**Current Reality:**
- ❌ **BROKEN**: Not detecting violations
- ❌ Edge creation incomplete
- ❌ Frequencies not propagating
- ❌ Effectively useless right now

**Example of Failure:**
```bash
$ ./spellcraft examples/02_multiple_pll_cascading.vhd
✓ Analysis complete. No issues found.  # WRONG! Should detect 300 MHz > 165 MHz
```

---

## The Architectural Gap

**We have two separate systems that don't talk to each other:**

```
┌─────────────────────────────────────────────────────────────┐
│  Type-Level (Clash)          Runtime (Spellcraft)        │
│  ✅ Working                   ❌ Broken                      │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Haskell/Clash Code          VHDL Files                      │
│       ↓                           ↓                           │
│  Type-checked at compile     Parsed at runtime               │
│       ↓                           ↓                           │
│  Violations prevented        Violations checked              │
│                                   ↓                           │
│                              ❌ NOT WORKING                   │
│                                                               │
│            No bridge between them!                            │
└───────────────────────────────────────────────────────────────┘
```

---

## Three Possible Approaches

### Approach A: Keep Separate (Current - Broken)

**Type-level for Haskell, Runtime for VHDL**

```haskell
-- Users writing Clash/Haskell
myDesign :: HWSignal 200 Bit
myDesign = ...  -- Type-checked ✅

-- Users with VHDL files
$ ./spellcraft design.vhd  -- Runtime checked ❌ (broken)
```

**Status:** Runtime part needs fixing!

### Approach B: Bridge Runtime → Type-Level (Advanced)

**Lift runtime VHDL analysis to type level**

```haskell
-- Parse VHDL, extract frequencies, construct Clash types
parseVHDL :: FilePath -> IO SomeClockGraph

data SomeClockGraph where
  SomeGraph :: (KnownNat srcFreq, KnownNat dstFreq)
            => ClockGraph srcFreq dstFreq
            -> SomeClockGraph

-- Use type checker at runtime!
checkWithTypes :: SomeClockGraph -> Either TypeError ()
```

**How it would work:**
1. Parse VHDL → Extract frequency values
2. Use singletons to lift values to type level
3. Construct Clash type-level graph
4. Invoke GHC type checker at runtime
5. Get type errors as violation reports

**Advantages:**
- ✅ True dependent-type checking for VHDL
- ✅ Leverages existing Clash infrastructure
- ✅ Same guarantees for VHDL as Haskell

**Challenges:**
- 🔴 Complex: Requires singletons pattern
- 🔴 Advanced Haskell: GADTs, type families, existentials
- 🔴 Engineering effort: 2-3 weeks work
- 🔴 Runtime performance: Type-level computation overhead
- 🔴 GHC internals: May need to invoke type checker programmatically

**Example Pattern:**
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Singletons

-- Singleton for runtime frequencies
data SFreq (n :: Nat) where
  SFreq :: KnownNat n => SFreq n

-- Convert runtime value to singleton
mkSFreq :: Integer -> (forall n. KnownNat n => SFreq n -> r) -> r
mkSFreq n k = case someNatVal n of
  SomeNat (Proxy :: Proxy m) -> k (SFreq :: SFreq m)

-- Use it
checkVHDL :: FilePath -> IO [TypeError]
checkVHDL file = do
  design <- parseVHDLFile file
  freq <- extractFrequency design
  mkSFreq freq $ \(sfreq :: SFreq f) ->
    -- Now 'f' is a type-level Nat!
    checkConstraints (Proxy @f)
```

### Approach C: Fix Runtime, Ignore Type-Level (Pragmatic)

**Just make the damn runtime analyzer work**

1. Fix edge creation (use port direction, not names)
2. Propagate frequencies correctly
3. Detect violations
4. Ship it

**Advantages:**
- ✅ Simple, direct
- ✅ Works for the actual use case (VHDL files)
- ✅ Can ship in days, not weeks
- ✅ No advanced type theory needed

**Disadvantages:**
- ❌ Doesn't leverage type-level work
- ❌ Runtime-only checking
- ❌ Two disconnected systems

---

## Recommended Path Forward

### Phase 1: Fix Runtime (NOW)
**Priority: CRITICAL**

The runtime analyzer is completely broken. Fix it:

1. **Fix edge creation** in `ClockGraph.hs:169-207`
   - Use `portConstraintDirection` not name heuristics
   - See ADC-007-PROGRESS.md Priority 1

2. **Add missing ports** to component specs
   - Encoder needs `video_out` output port
   - Update `ComponentLibs/TestComponents.hs`

3. **Test** on all examples
   - Should detect 300 MHz > 165 MHz violation
   - Should show clock path in error

**Time estimate:** 1-2 hours
**Benefit:** Actually usable tool

### Phase 2: Document Bridge Pattern (LATER)

Create example showing how to bridge runtime → type-level:

```haskell
-- Example: src/VHDL/Clash/Integration.hs
-- Show HOW to do it, don't implement it fully
```

**Time estimate:** 4-8 hours
**Benefit:** Proof of concept for future work

### Phase 3: Full Integration (FUTURE)

Implement complete runtime → type-level bridge:
- Singleton-based frequency lifting
- Existential type wrapping
- Type-level constraint checking at runtime

**Time estimate:** 2-3 weeks
**Benefit:** True dependent-type VHDL analysis

---

## The Honest Assessment

**Current state:**
- ❌ Runtime analysis: **BROKEN** (reports no issues when there are violations)
- ✅ Type-level analysis: **WORKS** but only for Haskell code
- ❌ Integration: **NONE** (two separate systems)

**What we told users (v0.2.0 release):**
> "Hardware constraint violations are now prevented by the type checker before synthesis!"

**Reality:**
> Type checking works for Haskell. VHDL analysis doesn't work at all.

**What we need to do:**
1. ✅ Fix runtime analysis (Priority 1)
2. 📋 Document the gap honestly
3. 📋 Show path to bridge (future work)

---

## Technical Deep Dive: Why Bridge is Hard

### Challenge 1: Lifting Values to Types

```haskell
-- We have runtime value
freq :: Integer
freq = 200

-- Need type-level nat
type Freq = 200  -- Can't do this dynamically!

-- Solution: Singletons
data SomeFreq where
  SomeFreq :: KnownNat n => Proxy n -> SomeFreq

toTypeLevel :: Integer -> SomeFreq
toTypeLevel n = case someNatVal n of
  SomeNat p -> SomeFreq p
```

### Challenge 2: Existential Unwrapping

```haskell
-- Type is hidden inside existential
data SomeGraph where
  SomeGraph :: (KnownNat f1, KnownNat f2) => Graph f1 f2 -> SomeGraph

-- To use it, need continuation-passing style
withGraph :: SomeGraph -> (forall f1 f2. Graph f1 f2 -> r) -> r
```

### Challenge 3: Type-Level Computation at Runtime

```haskell
-- Want to check: f1 * m > f2?
-- But f1, m, f2 are types, not values!

type family CheckOk (f1 :: Nat) (m :: Nat) (f2 :: Nat) :: Bool where
  CheckOk f1 m f2 = (f1 * m) <=? f2

-- Need to extract the result back to runtime
checkOk :: Proxy f1 -> Proxy m -> Proxy f2 -> Bool
checkOk _ _ _ = case sameNat (Proxy @(f1 * m)) (Proxy @f2) of ...
```

---

## Conclusion

**Current Architecture: Two Ships Passing in the Night**

1. **Type-level (Clash)**: Beautiful type system, works perfectly, useless for VHDL
2. **Runtime (Analyzer)**: Should work for VHDL, completely broken, not using types

**The Fix:**
- Don't overthink it
- Fix the runtime edge creation (30-60 min)
- Make the basic analyzer work
- Ship it
- Bridge to type-level is future work (2-3 weeks)

**The Dream:**
- Parse VHDL → Lift to types → Type-check → Get compile-time guarantees for VHDL files

**The Reality:**
- We can't even detect a simple frequency violation in a 44-line VHDL file

---

**Let's fix the runtime analyzer first, then talk about dependent types.**

See: `ADC-007-PROGRESS.md` for the specific fix needed.
