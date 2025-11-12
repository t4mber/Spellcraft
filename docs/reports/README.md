# Spellcraft Development Reports

Technical reports and evaluation documents for the Spellcraft VHDL analyzer.

## Release Reports (0.4.0)

### 2025-11-12-comprehensive-evaluation.md
**[START HERE]** Complete system evaluation for 0.4.0 release
- All test corpora results (27 production + 6 fixtures)
- Performance benchmarks (0.2s per file, 5.6s for 27 files)
- KAOS ELF violation detection validation (75% accuracy)
- Feature validation and quality metrics
- **Verdict**: ✅ APPROVED FOR 0.4.0 RELEASE

### 2025-11-12-corpus-metrics.md
Test corpus results summary
- Lumarian: 76% clean pass (13 files)
- Mirrorbound: 40% clean pass (10 files)
- KAOS ELF: 75% violation detection (4 files)
- Overall: 96% parse success, 55% clean pass

### 2025-11-12-signal-usage-tracker.md
Signal usage analysis feature (ADC-012)
- Component output port tracking heuristic
- Before/after metrics: 38% → 76% clean pass on Lumarian
- False positive reduction: 62% → 11%

### 2025-11-12-kaos-elf-evaluation.md
KAOS ELF violation injection framework (ADC-011)
- Test corpus generation and validation
- Violation detection accuracy by level
- Level 1-2 detection working, Level 3-5 planned

## Development Reports

### DIAGRAM_GENERATION_SUMMARY.md
LZX diagram generation task summary
- High-level patch diagrams (Lumarian, Mirrorbound)
- Mermaid diagram standards

### LINT_REPORT.md
Contract lint macro application report
- Documentation formatting fixes

## Quick Reference

**Run all tests**:
```bash
python3 tests/corpus_test.py
```

**Single file test**:
```bash
stack exec spellcraft -- contrib/lzx/lumarian/filter.vhd
```

**Performance metrics**: See "Performance Metrics" in 2025-11-12-comprehensive-evaluation.md

## Reading Order

1. **For release understanding**: Start with 2025-11-12-comprehensive-evaluation.md
2. **For test metrics**: See 2025-11-12-corpus-metrics.md
3. **For specific features**: See feature-specific reports (signal-usage-tracker, kaos-elf-evaluation)
