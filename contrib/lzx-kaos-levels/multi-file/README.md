# KAOS ELF Level 6: Cross-File Violations

## Overview

Level 6 tests target violations that **only manifest when analyzing multiple files together**.
These violations cannot be detected with single-file analysis because they require knowledge
of component entity definitions in other files.

## Test Scenario

This test case creates a design hierarchy where:

1. `level6_component_a.vhd` - Defines entity `level6_alu` with output ports `result` and `valid`
2. `level6_component_b.vhd` - Defines entity `level6_register` with output port `q`
3. `level6_top.vhd` - Top-level entity that instantiates both components

## Expected Violations

### Detectable with Multi-File Context

When all three files are analyzed together, Spellcraft should:

1. **NOT report** `alu_result` as undriven (driven by `level6_alu.result` output)
2. **NOT report** `alu_valid` as undriven (driven by `level6_alu.valid` output)
3. **NOT report** `reg_q` as undriven (driven by `level6_register.q` output)
4. **REPORT** `orphan_signal` as undriven (genuinely undriven)
5. **REPORT** `dead_signal` as unused (assigned but never read)

### Single-File Analysis

When analyzing only `level6_top.vhd` (without component files), Spellcraft uses
a heuristic fallback that identifies ports named "result", "valid", "q" etc. as
likely outputs based on common naming conventions. This means even single-file
analysis may not produce false positives for well-named ports.

The multi-file analysis provides **definitive** output port detection based on
actual entity definitions, while single-file uses **heuristic** detection that
may miss unconventionally named output ports.

## Test Commands

```bash
# Single-file analysis (expect false positives)
stack exec spellcraft -- contrib/lzx-kaos-levels/multi-file/level6_top.vhd

# Multi-file analysis (expect reduced false positives)
stack exec spellcraft -- contrib/lzx-kaos-levels/multi-file/*.vhd

# Expected: 2 violations (orphan_signal undriven, dead_signal unused)
```

## Violation Tracking

| Signal | Expected Status | Single-File | Multi-File | Notes |
|--------|-----------------|-------------|------------|-------|
| `alu_result` | Driven by component | OK (heuristic) | OK (definitive) | Common output name detected |
| `alu_valid` | Driven by component | OK (heuristic) | OK (definitive) | Common output name detected |
| `reg_q` | Driven by component | OK (heuristic) | OK (definitive) | Common output name detected |
| `orphan_signal` | Truly undriven | DETECTED | DETECTED | True violation |
| `dead_signal` | Truly unused | DETECTED | DETECTED | True violation |

## Contract Reference

- **ADC-029**: Multi-File Analysis Context
- **ADC-012**: Signal Usage Analysis

## Subtlety Rating

- **Subtlety**: 3 (Moderate)
- **Requires**: Multi-file context resolution
- **Pattern**: Component output port mapping
