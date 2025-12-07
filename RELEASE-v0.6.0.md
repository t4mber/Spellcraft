# Spellcraft v0.6.0 Release

**Release Date:** 2025-12-04
**Type:** Minor Release
**Focus:** Violation Detection & Warning Infrastructure
**Status:** APPROVED FOR PRODUCTION

---

## Release Summary

Spellcraft v0.6.0 completes the ADC-012 violation detection framework with latch inference and unbounded counter detection, achieving **100% detection rate** on Kaos Elf levels 1-3. This release also adds warning vs error severity infrastructure and generate statement parsing.

---

## Highlights

### Latch Inference Detection (ADC-012 Priority 2)
Detects incomplete signal assignments that synthesize to latches:
```vhdl
process(clk)
begin
  if rising_edge(clk) then
    if enable = '1' then
      output <= data;  -- Missing else branch = latch!
    end if;
  end if;
end process;
```

### Unbounded Counter Detection (ADC-012 Priority 3)
Identifies counters without overflow protection:
```vhdl
counter <= counter + 1;  -- Will silently wrap!
```

### Warning vs Error Severity (ADC-014)
- Undriven signals → **Error** (red)
- Unused signals → **Warning** (yellow)
- Latch inference → **Warning** (yellow)
- `--strict` flag treats warnings as errors
- `--suppress-warnings` hides warnings

### Generate Statements (ADC-028)
```vhdl
gen: for i in 0 to 7 generate
  inst: component port map (...);
end generate;

gen: if ENABLE generate
  -- conditional hardware
end generate;
```

---

## Metrics

### Kaos Elf Detection

| Level | Violation | Status |
|-------|-----------|--------|
| 1 | Undriven signal | ✅ DETECTED |
| 2 | Latch inference | ✅ DETECTED |
| 3 | Unbounded counter | ✅ DETECTED |

### Parse Success

| Corpus | Files | Rate |
|--------|-------|------|
| LZX Lumarian | 13 | 100% |
| LZX Mirrorbound | 10 | 100% |
| Codeglow | 4 | 100% |
| Kaos Elf | 3 | 100% |

---

## New CLI Flags

```bash
# Treat warnings as errors
spellcraft --strict design.vhd

# Hide warning output
spellcraft --suppress-warnings design.vhd

# Aliases
spellcraft --warnings-as-errors design.vhd
```

---

## Files Added/Modified

### New Modules
- `src/VHDL/Analysis/ControlFlow.hs` - CFG construction, latch detection
- `src/VHDL/Analysis/ArithmeticBounds.hs` - Counter pattern detection

### Modified Modules
- `src/VHDL/Constraint/Types.hs` - Severity type, violation classification
- `src/VHDL/CLI/Options.hs` - New CLI flags
- `src/VHDL/CLI/Report.hs` - Severity-based reporting
- `src/VHDL/CLI/Format.hs` - Color-coded output
- `src/VHDL/AST.hs` - GenerateStatement types
- `src/VHDL/Parser.hs` - Generate statement parsing

### New Contracts
- `contracts/adc-028-parser-backlog.md`

---

## Upgrade Guide

### From v0.5.0

```bash
git pull
stack build
```

### Breaking Changes

**None** - v0.6.0 is fully backward compatible.

### Behavioral Changes

- Unused signals now reported as **warnings** (yellow) instead of errors
- Exit code 0 when only warnings present (use `--strict` for old behavior)

---

## Next Steps (v0.7.0)

- Multi-file analysis (resolve external entity references)
- Reduce remaining false positives

---

**Release Date**: 2025-12-04
**Version**: 0.6.0
**Status**: Production Ready
