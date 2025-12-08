# Spellcraft v0.8.0 Release

**Release Date:** 2025-12-08
**Type:** Minor Release
**Focus:** Enhanced Videomancer, CI/CD Integration, KAOS ELF Level 4
**Status:** APPROVED FOR PRODUCTION

---

## Release Summary

Spellcraft v0.8.0 extends the video hardware validation system with 4K/8K UHD and HDR support, adds CI/CD integration via JSON and SARIF export formats, introduces KAOS ELF Level 4 for clock domain crossing detection, and ensures full test coverage registration.

---

## Highlights

### Enhanced Videomancer (ADC-030)

#### Video Timing Standards
- **4K UHD**: 3840x2160 @ 60Hz (594 MHz pixel clock)
- **4K 120Hz**: 3840x2160 @ 120Hz (1188 MHz pixel clock)
- **8K UHD**: 7680x4320 @ 60Hz (2376 MHz pixel clock)
- **8K 120Hz**: 7680x4320 @ 120Hz (4752 MHz pixel clock)
- **DCI 4K Cinema**: 4096x2160 @ 24Hz

#### HDR Metadata Timing
- **HDR10**: Static metadata timing constraints
- **HDR10+**: Dynamic metadata timing
- **Dolby Vision**: Dual-layer metadata
- **HLG**: Hybrid Log-Gamma broadcast

#### Audio Timing Validation
- **PCM**: 48kHz, 96kHz, 192kHz sample rates
- **I2S**: BCLK, LRCLK, MCLK relationships
- **S/PDIF & AES3**: Professional audio timing
- **HDMI Embedded**: Up to 8 channel support

#### Pipeline Constraints
- Stage latency validation
- Throughput checking (Mega-pixels per second)
- Buffer depth and FIFO sizing

### Report Export (ADC-031)

#### SARIF 2.1.0 Format
```bash
spellcraft --format=sarif src/*.vhd > results.sarif
```
- Native GitHub Code Scanning integration
- VSCode SARIF Viewer support
- Rule IDs: SPELL000-SPELL005

#### JSON Format
```bash
spellcraft --format=json src/*.vhd > results.json
```
- Structured violation data
- CI/CD pipeline integration
- Machine-readable output

### KAOS ELF Level 4: The Dimensional Rift

*Where signals traverse the boundaries between temporal realms without proper escort...*

```vhdl
-- VIOLATION: Signal crosses clock domains without synchronization
process(clk_fast)
begin
  if rising_edge(clk_fast) then
    fast_signal <= slow_data;  -- Metastability awaits!
  end if;
end process;
```

**Detection:** Clock domain crossing analysis identifies unsynchronized domain transitions.

---

## Metrics

### KAOS ELF Detection

| Level | Name | Violation | Status |
|-------|------|-----------|--------|
| 1 | The Hollow Vessel | Undriven signal | VANQUISHED |
| 2 | The Flickering Flame | Latch inference | VANQUISHED |
| 3 | The Silent Overflow | Unbounded counter | VANQUISHED |
| 4 | The Dimensional Rift | Clock domain crossing | VANQUISHED |
| 6 | The Shadowbound Grimoire | Cross-file | VANQUISHED |

### Parse Success

| Corpus | Files | Rate |
|--------|-------|------|
| LZX Lumarian | 13 | 100% |
| LZX Mirrorbound | 10 | 100% |
| Codeglow | 4 | 100% |
| KAOS ELF | 4 | 100% |

### Test Suite

- **Unit Tests**: 63 passing
- **Integration Tests**: All KAOS ELF levels detected
- **Modules Registered**: Full coverage in cabal

---

## New CLI Options

```bash
# SARIF export for GitHub Code Scanning
spellcraft --format=sarif design.vhd

# JSON export for CI/CD
spellcraft --output-format=json design.vhd

# Text format (alias for human)
spellcraft --format=text design.vhd
```

---

## Files Added/Modified

### New Modules
- `src/VHDL/CLI/Export.hs` - JSON and SARIF export

### Modified Modules
- `src/VHDL/Videomancer/Config.hs` - 4K/8K/HDR/Audio timing
- `src/VHDL/Videomancer/Validation.hs` - Pipeline constraint checking
- `src/VHDL/CLI/Options.hs` - SARIF format option
- `src/VHDL/CLI/Report.hs` - Export integration
- `spellcraft.cabal` - Version 0.8.0, test module registration

### New Test Files
- `test/fixtures/kaos-elf/level4-cdc.vhd` - Clock domain crossing test

### Updated Documentation
- `test/fixtures/kaos-elf/README.md` - Level 4 documentation
- `CHANGELOG.md` - v0.8.0 release notes

---

## Upgrade Guide

### From v0.7.0

```bash
git pull
stack build
stack test  # Verify 63 tests pass
```

### Breaking Changes

**None** - v0.8.0 is fully backward compatible.

### New Dependencies

- None

---

## SARIF Integration

### GitHub Actions

```yaml
- name: Run Spellcraft
  run: |
    spellcraft --format=sarif src/*.vhd > results.sarif

- name: Upload SARIF
  uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: results.sarif
```

### VSCode

Install the SARIF Viewer extension, then open the generated `.sarif` file for inline violation display.

---

## Next Steps (v0.9.0)

- **LSP Integration**: Language Server Protocol for real-time IDE analysis
- **Clash Integration**: End-to-end Clash → VHDL → Analysis workflow
- **Array Indexing Fix**: Eliminate remaining false positive

---

**Release Date**: 2025-12-08
**Version**: 0.8.0
**Status**: Production Ready
