# Spellcraft Implementation Roadmap

**Generated**: 2025-11-10
**Updated**: 2025-12-04 (v0.6.0 Release)
**Status**: 0.6.0 Shipped - Full Violation Detection

## Current State (v0.6.0)

### Completed
- **VHDL Parser** (ADC-001, ADC-008): 100% success on 30 corpus files
- **Multi-Signal Declarations** (ADC-008): `signal a, b, c : type;` support
- **Based Literals** (ADC-008): Hex, binary, octal (`x"BEEF"`, `b"1010"`, `o"777"`)
- **Generate Statements** (ADC-028): For-generate and if-generate
- **Process Body Parsing** (ADC-013): Complete with if/elsif/else support
- **Signal Usage Tracker** (ADC-012 Priority 1): Undriven/unused signal detection
- **Latch Inference** (ADC-012 Priority 2): Incomplete assignment detection
- **Unbounded Counters** (ADC-012 Priority 3): Counter overflow detection
- **Warning Infrastructure** (ADC-014): Severity levels, --strict, --suppress-warnings
- **Kaos Elf Framework** (ADC-011): 100% detection accuracy (3/3 levels)

### Parse & Detection (v0.6.0)
| Corpus | Files | Parse | Detection |
|--------|-------|-------|-----------|
| LZX Lumarian | 13 | 100% | - |
| LZX Mirrorbound | 10 | 100% | - |
| Codeglow | 4 | 100% | - |
| Kaos Elf | 3 | 100% | 3/3 levels |
| **Total** | **30** | **100%** | **100%** |

### Known Limitations
1. **Array Indexing**: delay.vhd has 1 false positive (signal assigned via array index)
   - **Impact**: Analysis false positive, not parse failure
   - **Priority**: Low - isolated case

2. **External Entities**: 2 files reference external entities (expected limitation)
   - **Files**: lumarian.vhd, mirrorbound.vhd
   - **Resolution**: Requires ADC-015 (multi-file analysis)

## Priority Tasks (v0.7.0)

### Priority 1: Multi-File Analysis
**Contract**: To be created
**Timeline**: 2-3 weeks

**Tasks**:
1. Parse multiple files into shared context
2. Resolve external entity references
3. Cross-file signal tracking

**Success Metric**: lumarian.vhd and mirrorbound.vhd pass clean

### Priority 2: False Positive Reduction
**Timeline**: 1 week

**Tasks**:
1. Fix array indexing false positive (delay.vhd)
2. Improve component output heuristics

**Success Metric**: < 5% false positive rate on clean corpus

## Parser Enhancement Backlog

### Configuration Declarations
```vhdl
configuration cfg of entity_name is
  for architecture_name
  end for;
end configuration;
```
Not currently parsed. Low priority.

## Success Criteria

### Milestone 1: Production Ready (COMPLETE)
- [x] Parse 100% of LZX corpus
- [x] Parse multi-signal declarations
- [x] Parse based literals
- [x] < 5% false positive rate

### Milestone 2: Full Detection (COMPLETE)
- [x] Detect Level 1 violations (undriven signals)
- [x] Detect Level 2 violations (latch inference)
- [x] Detect Level 3 violations (unbounded counters)
- [x] 100% detection on Kaos Elf

### Milestone 3: User Experience (COMPLETE)
- [x] Clear error vs warning distinction
- [x] Configurable severity (--strict)
- [x] Suppressible warnings (--suppress-warnings)

### Milestone 4: Multi-File Analysis (v0.7.0 Target)
- [ ] Cross-file entity resolution
- [ ] Shared context for multiple files
- [ ] External entity reference tracking

## Contract References

| Contract | Title | Status | Priority |
|----------|-------|--------|----------|
| [ADC-008](contracts/adc-008-vhdl-parser-enhancement.qmd) | VHDL Parser Enhancement | Complete | - |
| [ADC-011](contracts/adc-011-kaos-elf.qmd) | Kaos Elf Testing | Complete | - |
| [ADC-012](contracts/adc-012-violation-detection.qmd) | Violation Detection Framework | Complete | - |
| [ADC-013](contracts/adc-013-process-parsing.qmd) | Process Body Parsing | Complete | - |
| [ADC-014](contracts/adc-014-warning-infrastructure.qmd) | Warning Infrastructure | Complete | - |
| [ADC-015](contracts/adc-015-architecture-body-parsing.qmd) | Architecture Body Parsing | Complete | - |
| [ADC-028](contracts/adc-028-parser-backlog.md) | Parser Backlog (Generate) | Complete | - |

---

## Version History

### v0.6.0 (2025-12-04)
- **100% Kaos Elf detection** (3/3 levels)
- Latch inference detection (ADC-012 Priority 2)
- Unbounded counter detection (ADC-012 Priority 3)
- Warning vs error infrastructure (ADC-014)
- Generate statement parsing (ADC-028)
- `--strict` and `--suppress-warnings` flags

### v0.5.0 (2025-12-04)
- **100% parse rate** on LZX corpus (was 96%)
- Multi-signal declaration support
- Based literal support (hex, binary, octal)
- For-loop direction parsing fix
- Codeglow corpus added

### v0.4.0 (2025-11-12)
- Component output tracking (82% false positive reduction)
- Process body parsing complete
- Signal usage analysis
- 96% parse rate on production code

### v0.3.0 (2025-11-04)
- Clash file support
- Stack build system
- 85% parse rate
