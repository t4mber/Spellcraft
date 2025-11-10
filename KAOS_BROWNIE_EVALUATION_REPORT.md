# Kaos Brownie Subtlety Evaluation Report

**Contract**: spellcraft-adc-011
**Date**: 2025-11-09
**Test Scope**: VHDL Hardware Violation Detection at Multiple Subtlety Levels

## Executive Summary

This report evaluates the Spellcraft VHDL analyzer's ability to detect hardware violations at different subtlety levels, from obvious errors to extremely subtle timing issues.

**Key Findings**:
- ✅ **Parser Success**: 4/4 variants parse successfully (100%)
- ❌ **Detection Rate**: 0/4 violations detected (0%)
- 📊 **Expected Detection**: 3/4 should be detectable (75%)
- 🎯 **Current Implementation**: Detection features not yet implemented

## Test Methodology

### Subtlety Levels Defined

1. **Level 1 - Obvious**: Syntax-valid but immediately detectable (e.g., signal named "NEVER_USED")
2. **Level 2 - Moderate**: Requires basic static analysis (e.g., latch inference)
3. **Level 3 - Subtle**: Requires cross-module analysis (e.g., silent overflow)
4. **Level 4 - Very Subtle**: Requires timing analysis (e.g., hidden CDC)
5. **Level 5 - Extremely Subtle**: Requires simulation-level analysis (e.g., race conditions)

### Test Corpus Generated

Source: `contrib/lzx/lumarian/enhance.vhd`

Generated 4 violation variants:
```
contrib/lzx-kaos-levels/
├── enhance-level1-undriven.vhd     (Subtlety 1)
├── enhance-level2-partial.vhd      (Subtlety 2)
├── enhance-level3-bitgrowth.vhd    (Subtlety 3)
└── enhance-level5-race.vhd         (Subtlety 5)
```

Note: Level 4 variant could not be generated (no suitable injection point in source file).

## Detailed Results

### Level 1: Completely Undriven Signal

**Violation**: Signal declared but never assigned
**Subtlety**: 1 (Obvious)
**Expected**: Should detect ✓
**Actual**: Not detected ❌

**Injected Code**:
```vhdl
signal THIS_SIGNAL_IS_NEVER_USED : std_logic; -- OBVIOUS BUG!
```

**Analysis**:
- **Difficulty**: Trivial - signal has obvious name and no assignments
- **Detection Method**: Unused signal check (not implemented)
- **Confidence**: Very High
- **Impact**: Currently the analyzer does not track signal usage

**Spellcraft Output**:
```
Parse results - VHDL errors: 0, designs: 1, Clash violations: 0
✓ Analysis complete. No issues found.
```

---

### Level 2: Incompletely Assigned Signal (Latch Inference)

**Violation**: Signal assigned in some code paths but not others
**Subtlety**: 2 (Moderate)
**Expected**: Should detect ✓
**Actual**: Not detected ❌

**Injected Code**:
```vhdl
signal s_sometimes_driven : std_logic;

-- Later in process:
if s_valid = '1' then
  s_sometimes_driven <= '1'; -- Only assigned here!
end if;
```

**Analysis**:
- **Difficulty**: Moderate - requires control flow analysis
- **Detection Method**: Control flow analysis (not implemented)
- **Confidence**: High
- **Impact**: Will infer a latch, causing synthesis warnings

**Spellcraft Output**:
```
Parse results - VHDL errors: 0, designs: 1, Clash violations: 0
Violations: []
```

---

### Level 3: Silent Overflow

**Violation**: Counter/accumulator without overflow protection
**Subtlety**: 3 (Subtle)
**Expected**: Should detect ✓
**Actual**: Not detected ❌

**Injected Code**:
```vhdl
signal s_accumulator : unsigned(G_WIDTH - 1 downto 0); -- Will overflow!

-- In process:
s_accumulator <= s_accumulator + 1; -- No bounds check!
```

**Analysis**:
- **Difficulty**: Subtle - requires arithmetic bounds checking
- **Detection Method**: Arithmetic bounds checker (not implemented)
- **Confidence**: Medium
- **Impact**: Silent data corruption when counter wraps

**Spellcraft Output**:
```
Parse results - VHDL errors: 0, designs: 1, Clash violations: 0
Violations: []
```

---

### Level 5: Delta Cycle Race Condition

**Violation**: Signal update order creates race between stages
**Subtlety**: 5 (Extremely Subtle)
**Expected**: Should NOT detect ✗ (too subtle for static analysis)
**Actual**: Not detected ✓ (Expected)

**Injected Code**:
```vhdl
signal s_data_stage1 : unsigned(G_WIDTH - 1 downto 0);
signal s_data_stage2 : unsigned(G_WIDTH - 1 downto 0);

-- In process:
if rising_edge(clk) then
  s_data_stage2 <= s_data_stage1; -- Read old or new value?
  s_data_stage1 <= a; -- Update might happen simultaneously
end if;
```

**Analysis**:
- **Difficulty**: Extremely Subtle - requires delta cycle analysis
- **Detection Method**: Delta cycle analyzer (not feasible for static analysis)
- **Confidence**: Very Low
- **Impact**: Non-deterministic behavior depending on simulator
- **Note**: This is expected to not be detected by static analysis

**Spellcraft Output**:
```
Parse results - VHDL errors: 0, designs: 1, Clash violations: 0
Violations: []
```

---

## Summary Statistics

### Detection Performance

| Subtlety Level | Generated | Expected Detection | Actual Detection | Success Rate |
|----------------|-----------|-------------------|------------------|--------------|
| Level 1 (Obvious) | 1 | ✓ Yes | ❌ No | 0% |
| Level 2 (Moderate) | 1 | ✓ Yes | ❌ No | 0% |
| Level 3 (Subtle) | 1 | ✓ Yes | ❌ No | 0% |
| Level 4 (Very Subtle) | 0 | - | - | N/A |
| Level 5 (Extremely Subtle) | 1 | ✗ No | ✗ No | 100% (expected) |
| **Total** | **4** | **3/4** | **0/3** | **0%** |

### Parser Performance

| Metric | Result |
|--------|--------|
| Files Parsed | 4/4 (100%) |
| Parse Errors | 0 |
| Syntax Validity | ✅ All valid VHDL |
| Clock Source Detection | ✅ 4/4 detected |
| Clock Propagation | ✅ Basic graphs built |

## Analysis

### What Works

1. **Parser Robustness** ✅
   - Successfully parses all violation variants
   - No false syntax errors
   - Handles complex VHDL constructs

2. **Basic Clock Analysis** ✅
   - Identifies clock source ports
   - Builds initial clock graphs
   - Propagates clock information

3. **AST Construction** ✅
   - Correctly builds abstract syntax trees
   - Preserves structure for analysis

### What Doesn't Work (Yet)

1. **Signal Usage Analysis** ❌
   - No tracking of signal assignments vs usage
   - Cannot detect undriven signals
   - No latch inference detection

2. **Control Flow Analysis** ❌
   - Does not analyze conditional assignments
   - Cannot detect incomplete signal coverage
   - No path-based analysis

3. **Arithmetic Safety** ❌
   - No bounds checking on arithmetic operations
   - Cannot detect potential overflows
   - No saturation analysis

4. **Advanced Timing** ❌
   - No delta cycle analysis (expected)
   - Limited CDC detection
   - No race condition detection

## Detection Gap Analysis

### High-Priority Gaps (Should Implement)

1. **Undriven Signal Detection** (Level 1)
   - **Impact**: High - causes undefined behavior
   - **Difficulty**: Low - simple AST traversal
   - **ROI**: Very High - catches common bugs

2. **Latch Inference Detection** (Level 2)
   - **Impact**: High - causes synthesis issues
   - **Difficulty**: Medium - requires control flow graph
   - **ROI**: High - common mistake

3. **Arithmetic Bounds Checking** (Level 3)
   - **Impact**: Medium - silent data corruption
   - **Difficulty**: Medium - needs type analysis
   - **ROI**: Medium - catches subtle bugs

### Low-Priority Gaps (Advanced Features)

4. **Delta Cycle Analysis** (Level 5)
   - **Impact**: Low - only in specific simulators
   - **Difficulty**: Very High - requires simulation
   - **ROI**: Low - too subtle for most cases

## Recommendations

### Immediate Next Steps

1. **Implement Signal Usage Tracker** (Week 1)
   ```haskell
   -- Track all signal declarations and assignments
   -- Report undriven or unused signals
   -- Confidence: Very High
   ```

2. **Add Control Flow Analysis** (Week 2-3)
   ```haskell
   -- Build control flow graph for processes
   -- Check all paths assign all signals
   -- Detect potential latch inference
   ```

3. **Implement Arithmetic Checker** (Week 4)
   ```haskell
   -- Track bit widths through expressions
   -- Warn on potential overflows
   -- Suggest saturation points
   ```

### Long-Term Goals

4. **Enhanced CDC Detection** (Month 2)
   - Multi-clock domain tracking
   - Synchronizer pattern recognition
   - Metastability risk analysis

5. **Comprehensive Timing Analysis** (Month 3)
   - Combinatorial path analysis
   - Setup/hold violation prediction
   - Clock skew detection

## Validation

### Test Reproducibility

To reproduce these results:

```bash
# Generate kaos variants
python3 scripts/kaos-brownie-subtlety.py \\
  --source contrib/lzx/lumarian/enhance.vhd \\
  --subtlety 5 \\
  --output contrib/lzx-kaos-levels

# Run Spellcraft analyzer
stack exec spellcraft -- contrib/lzx-kaos-levels/*.vhd

# Check manifest
cat contrib/lzx-kaos-levels/chaos-violations-level5.json
```

### Expected vs Actual

| Violation ID | Expected Detected | Actually Detected | Status |
|--------------|------------------|-------------------|--------|
| level1-undriven | Yes | No | ❌ Gap |
| level2-partial | Yes | No | ❌ Gap |
| level3-bitgrowth | Yes | No | ❌ Gap |
| level5-race | No | No | ✅ Expected |

## Conclusion

The Spellcraft VHDL analyzer currently excels at parsing and basic structural analysis but lacks violation detection features. This evaluation:

1. ✅ **Validates Parser Quality**: 100% success on complex, corrupted VHDL
2. ❌ **Identifies Feature Gaps**: 0% detection of expected violations
3. 📋 **Provides Roadmap**: Clear priorities for detection feature development
4. 🧪 **Establishes Baseline**: Test corpus for regression testing

### Success Criteria

The kaos brownie framework has successfully:
- Created reproducible test methodology
- Documented expected detection capabilities
- Identified implementation priorities
- Provided evaluation baseline

### Next Iteration

When detection features are implemented, re-run this evaluation to measure progress:

```bash
# Track detection rate over time
python3 scripts/kaos-compare.py \\
  --manifest contrib/lzx-kaos-levels/chaos-violations-level5.json \\
  --results kaos-detection-report.json \\
  --baseline KAOS_BROWNIE_EVALUATION_REPORT.md
```

---

**Report Generated**: 2025-11-09
**Framework**: Kaos Brownie (spellcraft-adc-011)
**Status**: Detection features await implementation
**Next Review**: After violation detection implementation
