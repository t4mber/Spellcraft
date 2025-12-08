# Spellcraft Implementation Roadmap

**Generated**: 2025-11-10
**Updated**: 2025-12-08 (v0.8.0 Release)
**Status**: 0.8.0 Shipped - Multi-File Analysis, Enhanced Videomancer, CI/CD Export

## Current State (v0.8.0)

### Completed
- **VHDL Parser** (ADC-001, ADC-008): 100% success on 31 corpus files
- **Multi-Signal Declarations** (ADC-008): `signal a, b, c : type;` support
- **Based Literals** (ADC-008): Hex, binary, octal (`x"BEEF"`, `b"1010"`, `o"777"`)
- **Generate Statements** (ADC-028): For-generate and if-generate
- **Process Body Parsing** (ADC-013): Complete with if/elsif/else support
- **Signal Usage Tracker** (ADC-012 Priority 1): Undriven/unused signal detection
- **Latch Inference** (ADC-012 Priority 2): Incomplete assignment detection
- **Unbounded Counters** (ADC-012 Priority 3): Counter overflow detection
- **Warning Infrastructure** (ADC-014): Severity levels, --strict, --suppress-warnings
- **Multi-File Analysis** (ADC-029): Cross-file entity resolution and context building
- **Enhanced Videomancer** (ADC-030): 4K/8K UHD, HDR metadata, audio timing validation
- **Report Export** (ADC-031): JSON and SARIF 2.1.0 for CI/CD integration
- **KAOS ELF Framework** (ADC-011): 100% detection accuracy (4/4 levels + Level 6 multi-file)

### Parse & Detection (v0.8.0)
| Corpus | Files | Parse | Detection |
|--------|-------|-------|-----------|
| LZX Lumarian | 13 | 100% | - |
| LZX Mirrorbound | 10 | 100% | - |
| Codeglow | 4 | 100% | - |
| KAOS ELF | 4 | 100% | 4/4 levels |
| **Total** | **31** | **100%** | **100%** |

### Known Limitations
1. **Array Indexing**: delay.vhd has 1 false positive (signal assigned via array index)
   - **Impact**: Analysis false positive, not parse failure
   - **Priority**: Low - isolated case

2. **Level 5 (Delta Storm)**: Simulation/synthesis mismatch detection beyond current scope
   - **Impact**: Would require simulation semantics analysis
   - **Priority**: Deferred - complex implementation

## Priority Tasks (v0.9.0)

### Priority 1: Clash Integration
**Contract**: ADC-015 (to be extended)

**Tasks**:
1. Parse Clash-generated VHDL files
2. Map constraints from Clash domain annotations
3. Validate timing constraints post-synthesis

**Success Metric**: End-to-end Clash → VHDL → Analysis workflow

### Priority 2: LSP Integration
**Contract**: To be created

**Tasks**:
1. Implement Language Server Protocol
2. Real-time analysis in editors (VSCode, Neovim)
3. Inline violation highlighting

**Success Metric**: Working VSCode extension

### Priority 3: Array Indexing Fix
**Tasks**:
1. Track signal assignments through array indices
2. Eliminate delay.vhd false positive

**Success Metric**: 0% false positive rate on production corpus

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
- [x] Detect Level 4 violations (clock domain crossing)
- [x] 100% detection on KAOS ELF (4/4 levels)

### Milestone 3: User Experience (COMPLETE)
- [x] Clear error vs warning distinction
- [x] Configurable severity (--strict)
- [x] Suppressible warnings (--suppress-warnings)

### Milestone 4: Multi-File Analysis (COMPLETE)
- [x] Cross-file entity resolution
- [x] Shared context for multiple files
- [x] External entity reference tracking
- [x] Level 6 Shadowbound Grimoire detection

### Milestone 5: CI/CD Integration (COMPLETE)
- [x] JSON export format
- [x] SARIF 2.1.0 export format
- [x] GitHub Code Scanning integration

### Milestone 6: Enhanced Videomancer (COMPLETE)
- [x] 4K/8K UHD video timing
- [x] HDR metadata timing (HDR10, Dolby Vision, HLG)
- [x] Audio timing validation (PCM, I2S, S/PDIF)
- [x] Pipeline constraint checking

### Milestone 7: IDE Integration (v0.9.0 Target)
- [ ] Language Server Protocol implementation
- [ ] VSCode extension
- [ ] Real-time analysis

## Contract References

| Contract | Title | Status | Priority |
|----------|-------|--------|----------|
| [ADC-008](contracts/adc-008-vhdl-parser-enhancement.qmd) | VHDL Parser Enhancement | Complete | - |
| [ADC-011](contracts/adc-011-kaos-elf.qmd) | KAOS ELF Testing | Complete | - |
| [ADC-012](contracts/adc-012-violation-detection.qmd) | Violation Detection Framework | Complete | - |
| [ADC-013](contracts/adc-013-process-parsing.qmd) | Process Body Parsing | Complete | - |
| [ADC-014](contracts/adc-014-warning-infrastructure.qmd) | Warning Infrastructure | Complete | - |
| [ADC-015](contracts/adc-015-architecture-body-parsing.qmd) | Architecture Body Parsing | Complete | - |
| [ADC-028](contracts/adc-028-parser-backlog.md) | Parser Backlog (Generate) | Complete | - |
| [ADC-029](contracts/adc-029-multi-file-analysis.qmd) | Multi-File Analysis | Complete | - |
| [ADC-030](contracts/) | Video/Audio Timing Standards | Complete | - |
| [ADC-031](contracts/) | Report Export Formats | Complete | - |

---

## Version History

### v0.8.0 (2025-12-08)
- **Enhanced Videomancer**: 4K/8K UHD, HDR metadata, audio timing validation
- **Report Export**: JSON and SARIF 2.1.0 for GitHub Code Scanning
- **KAOS ELF Level 4**: Clock domain crossing detection (The Dimensional Rift)
- **Test Coverage**: 63 tests, all modules registered in cabal
- Multi-file analysis context improvements

### v0.7.0 (2025-12-07)
- **Multi-file analysis** (ADC-029): Cross-file entity resolution
- **KAOS ELF Level 6**: The Shadowbound Grimoire (cross-file violations)
- Moved KAOS ELF tests to test/fixtures with magical documentation

### v0.6.0 (2025-12-04)
- **100% KAOS ELF detection** (3/3 levels)
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
