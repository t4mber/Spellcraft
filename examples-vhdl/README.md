# Spellcraft Example Files

This directory contains real-world examples demonstrating the VDHL analyzer's ability to detect hardware constraint violations.

## Examples Overview

### ✅ **Example 1: PLL Frequency Violation** (`01_pll_frequency_violation.vhd`)
**Contract**: ADC-003 (Clock Domain Propagation)

**Scenario**: The PRD reference example
- 50 MHz input clock
- PLL multiplies by 4.16 → 208 MHz
- YPbPr encoder max = 165 MHz
- **Violation**: 208 MHz > 165 MHz

**Expected Output**:
```
01_pll_frequency_violation.vhd:28:3: error: Frequency violation
  Component 'YPbPr_Encoder_A' port 'pixel_clk' receives 208.0 MHz
  but maximum is 165.0 MHz
```

### ✅ **Example 2: Cascaded PLL Violation** (`02_multiple_pll_cascading.vhd`)
**Contract**: ADC-003 (Clock Domain Propagation)

**Scenario**: Multiple PLLs in series
- 25 MHz × 4 = 100 MHz (first PLL)
- 100 MHz × 3 = 300 MHz (second PLL)
- **Violation**: 300 MHz > 165 MHz encoder maximum

**Expected Output**:
```
02_multiple_pll_cascading.vhd:36:3: error: Frequency violation
  Component 'YPbPr_Encoder_A' port 'pixel_clk' receives 300.0 MHz
  but maximum is 165.0 MHz
```

### ✅ **Example 3: Valid Design** (`03_valid_design.vhd`)
**Contract**: ADC-003 (Clock Domain Propagation)

**Scenario**: Correctly designed system
- 50 MHz × 3.0 = 150 MHz
- **No Violation**: 150 MHz < 165 MHz

**Expected Output**:
```
No violations found
```

### ✅ **Example 4: Boundary Violation** (`04_boundary_violation.vhd`)
**Contract**: ADC-003 (Clock Domain Propagation)

**Scenario**: Just barely exceeds limit
- 50 MHz × 3.302 = 165.1 MHz
- **Violation**: 165.1 MHz > 165.0 MHz (precision matters!)

**Expected Output**:
```
04_boundary_violation.vhd:20:3: error: Frequency violation
  Component 'YPbPr_Encoder_A' port 'pixel_clk' receives 165.1 MHz
  but maximum is 165.0 MHz
```

### ✅ **Example 5: Generic Range Violation** (`05_generic_range_violation.vhd`)
**Contract**: ADC-002 (Component Constraint Library)

**Scenario**: PLL multiplication factor out of range
- MULT_FACTOR = 12.5
- PLL_1 constraint: range 1.0 to 10.0
- **Violation**: 12.5 > 10.0

**Expected Output**:
```
05_generic_range_violation.vhd:13:3: error: Generic range violation
  Component 'PLL_1' generic 'MULT_FACTOR' = 12.5
  but range is 1.0 to 10.0
```

### ✅ **Example 6: Fan-Out Violation** (`06_fan_out_violation.vhd`)
**Contract**: ADC-002 (Component Constraint Library)

**Scenario**: Too many loads on PLL output
- 11 encoder instances driven by single PLL output
- PLL clk_out max fan-out = 10
- **Violation**: 11 > 10

**Expected Output**:
```
06_fan_out_violation.vhd:29:3: error: Fan-out violation
  Signal 'shared_clk' drives 11 loads
  but maximum fan-out is 10
```

## Running the Examples

### Analyze Individual Files

```bash
# Test the PRD violation example
./vdhl-analyzer examples/01_pll_frequency_violation.vhd

# Test the valid design (should pass)
./vdhl-analyzer examples/03_valid_design.vhd

# Verbose mode to see clock propagation
./vdhl-analyzer --verbose examples/02_multiple_pll_cascading.vhd
```

### Analyze All Examples

```bash
# Human-readable output
./vdhl-analyzer examples/*.vhd

# JSON output for parsing
./vdhl-analyzer --format json examples/*.vhd

# GCC-style for editor integration
./vdhl-analyzer --format gcc examples/*.vhd
```

### Expected Summary

When analyzing all examples together:
```
Found 5 errors, 0 warnings in 6 files
```

- ✅ Example 1: **1 frequency violation**
- ✅ Example 2: **1 frequency violation**
- ✅ Example 3: **No violations** (valid design)
- ✅ Example 4: **1 frequency violation** (boundary case)
- ✅ Example 5: **1 generic range violation**
- ✅ Example 6: **1 fan-out violation**

## Contract Coverage

These examples demonstrate all major features from the ADC contracts:

| Contract | Feature | Example |
|----------|---------|---------|
| **ADC-001** | VDHL parsing | All examples |
| **ADC-002** | Component constraints | Examples 5, 6 |
| **ADC-003** | Clock propagation | Examples 1, 2, 3, 4 |
| **ADC-003** | Frequency violation | Examples 1, 2, 4 |
| **ADC-002** | Generic validation | Example 5 |
| **ADC-002** | Fan-out checking | Example 6 |
| **ADC-005** | Error reporting | All examples |

## Clash Type-Level Integration

These examples also demonstrate the Clash type-level features (ADC-006):

```haskell
-- At compile time, Clash can detect:
type Example1 = FreqMult 50 4  -- Results in 200+ MHz
type Valid = CheckMaxFreq (FreqMult 50 4) 165  -- Would fail at compile time

-- The analyzer implements runtime detection of the same violations
```

See `src/VDHL/Clash/` for the type-level implementation.
