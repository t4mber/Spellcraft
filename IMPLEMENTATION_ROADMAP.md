# Spellcraft Implementation Roadmap

**Generated**: 2025-11-10
**Updated**: 2025-12-04 (v0.5.0 Release)
**Status**: 0.5.0 Shipped - 100% Parse Rate Achieved

## Current State (v0.5.0)

### Completed
- **VHDL Parser** (ADC-001, ADC-008): 100% success on 27 corpus files
- **Multi-Signal Declarations** (ADC-008): `signal a, b, c : type;` support
- **Based Literals** (ADC-008): Hex, binary, octal (`x"BEEF"`, `b"1010"`, `o"777"`)
- **For-Loop Parsing**: Fixed `to` vs `downto` backtracking
- **Process Body Parsing** (ADC-013): Complete with if/elsif/else support
- **Component Output Tracking** (ADC-012 Priority 1): Heuristic-based implementation
- **Signal Usage Tracker** (ADC-012): Detection of undriven/unused signals
- **Kaos Elf Framework** (ADC-011): 75% detection accuracy

### Parse Success (v0.5.0)
| Corpus | Files | Parse Rate |
|--------|-------|------------|
| LZX Lumarian | 13 | 100% |
| LZX Mirrorbound | 10 | 100% |
| Codeglow | 4 | 100% |
| **Total** | **27** | **100%** |

### Known Limitations
1. **Array Indexing**: delay.vhd has 1 false positive (signal assigned via array index)
   - **Impact**: Analysis false positive, not parse failure
   - **Priority**: Low - isolated case

2. **External Entities**: 2 files reference external entities (expected limitation)
   - **Files**: lumarian.vhd, mirrorbound.vhd
   - **Resolution**: Requires ADC-015 (multi-file analysis)

## Priority Tasks (v0.6.0)

### Priority 1: Analysis Improvements
**Contract**: ADC-012 (Priorities 2-3)
**Timeline**: 2-3 weeks

**Tasks**:
1. **Control Flow Analysis**
   - Build CFG for processes
   - Detect latch inference (Level 2 chaos)

2. **Arithmetic Bounds**
   - Track bit widths
   - Detect overflows (Level 3 chaos)

**Success Metric**: Detect Level 2-3 chaos violations

### Priority 2: Warning Infrastructure
**Contract**: ADC-014 (Warning vs Error)
**Timeline**: 1 week

**Tasks**:
1. Create unified Severity type (Warning, Error)
2. Classify violations by severity:
   - **Warnings**: Unused signals, style issues
   - **Errors**: Undriven signals, logic bugs
3. Add `--strict` mode (treat warnings as errors)
4. Support suppression pragmas

**Success Metric**: Unused signals reported as warnings, clean output on well-written code

### Priority 3: Multi-File Analysis
**Contract**: ADC-015
**Timeline**: 2-3 weeks

**Tasks**:
1. Parse multiple files into shared context
2. Resolve external entity references
3. Cross-file signal tracking

**Success Metric**: lumarian.vhd and mirrorbound.vhd pass clean

## Parser Enhancement Backlog

### Conditional Signal Assignments
```vhdl
output <= a when sel = '1' else b;
```
Not currently parsed. Low priority - rare in LZX corpus.

### Generate Statements
```vhdl
gen: for i in 0 to N-1 generate
  inst: component port map (...);
end generate;
```
Not currently parsed. Medium priority for larger designs.

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

### Milestone 2: Full Detection (v0.6.0 Target)
- [x] Detect Level 1 violations (undriven signals)
- [ ] Detect Level 2 violations (latch inference)
- [ ] Detect Level 3 violations (overflow)
- [ ] < 5% false negatives

### Milestone 3: User Experience (v0.6.0 Target)
- [ ] Clear error vs warning distinction
- [ ] Configurable severity
- [ ] Suppressible warnings

## Contract References

| Contract | Title | Status | Priority |
|----------|-------|--------|----------|
| [ADC-008](contracts/adc-008-vhdl-parser-enhancement.qmd) | VHDL Parser Enhancement | Complete | - |
| [ADC-012](contracts/adc-012-violation-detection.qmd) | Violation Detection Framework | Partial | 1 |
| [ADC-013](contracts/adc-013-process-parsing.qmd) | Process Body Parsing | Complete | - |
| [ADC-014](contracts/adc-014-warning-infrastructure.qmd) | Warning Infrastructure | Planned | 2 |
| [ADC-015](contracts/adc-015-multi-file-analysis.qmd) | Multi-File Analysis | Planned | 3 |
| [ADC-011](contracts/adc-011-kaos-elf.qmd) | Kaos Elf Testing | Complete | - |

---

## Version History

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
