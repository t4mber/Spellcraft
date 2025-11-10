# Spellcraft Implementation Roadmap

**Generated**: 2025-11-10
**Status**: Active Development

## Current State

### ✅ Completed
- **VHDL Parser** (ADC-001, ADC-009): 100% success on test corpus
- **Signal Usage Tracker** (ADC-012 Priority 1): Detects undriven signals
- **Kaos Brownie Framework** (ADC-011): Generates test violations
- **Level 1 Detection**: Successfully identifies obvious violations

### 🚧 In Progress
- **Violation Detection** (ADC-012): Priority 1 complete, 2-3 pending

### ⚠️ Critical Issues
- **100% False Positive Rate**: All signals in processes appear undriven
- **Parser Limitations**: 8/13 Lumarian files fail to parse

## Priority Tasks

### 🔴 Priority 1: Eliminate False Positives
**Contract**: ADC-013 (Process Body Parsing)
**Timeline**: 1-2 weeks
**Impact**: Reduces false positives from 100% to <10%

**Tasks**:
1. Parse signal assignments in processes
2. Parse if/elsif/else statements
3. Extract signals from expressions
4. Update SignalUsage analyzer

**Success Metric**: Clean Lumarian files show 0 violations

### 🟡 Priority 2: Complete Violation Detection
**Contract**: ADC-012 (Priorities 2-3)
**Timeline**: 2-3 weeks
**Blocked By**: ADC-013 (need accurate signal tracking first)

**Tasks**:
1. **Control Flow Analysis** (Week 2-3)
   - Build CFG for processes
   - Detect latch inference (Level 2 chaos)

2. **Arithmetic Bounds** (Week 4)
   - Track bit widths
   - Detect overflows (Level 3 chaos)

**Success Metric**: Detect Level 2-3 chaos violations

### 🟢 Priority 3: Warning Infrastructure
**Contract**: ADC-014 (Warning vs Error)
**Timeline**: 1 week
**Blocked By**: ADC-013 (fix false positives first)

**Tasks**:
1. Create unified Severity type
2. Classify violations by severity
3. Update CLI reporting
4. Add configuration options

**Success Metric**: Unused signals reported as warnings, not errors

## Dependency Graph

```
ADC-013 (Process Parsing) [CRITICAL PATH]
    ├── Fixes false positives
    └── Enables:
        ├── ADC-012 Priority 2 (Control Flow)
        ├── ADC-012 Priority 3 (Arithmetic)
        └── ADC-014 (Warning Infrastructure)
```

## Next Actions

### Immediate (This Week)
1. **Start ADC-013** implementation
   - Begin with `parseSignalAssignment`
   - Add basic if/then/else support
   - Test on contrast.vhd

### Short Term (Next 2 Weeks)
2. Complete process parsing
3. Verify false positive elimination
4. Begin control flow analysis

### Medium Term (Next Month)
5. Complete ADC-012 priorities 2-3
6. Implement warning infrastructure
7. Achieve 100% chaos detection (Levels 1-3)

## Success Criteria

### Milestone 1: Production Ready (ADC-013 Complete)
- [ ] Parse all sequential statements
- [ ] < 5% false positive rate
- [ ] All clean Lumarian files pass

### Milestone 2: Full Detection (ADC-012 Complete)
- [ ] Detect Level 1 violations (undriven signals) ✅
- [ ] Detect Level 2 violations (latch inference)
- [ ] Detect Level 3 violations (overflow)
- [ ] < 5% false negatives

### Milestone 3: User Experience (ADC-014 Complete)
- [ ] Clear error vs warning distinction
- [ ] Configurable severity
- [ ] Suppressible warnings

## Risk Mitigation

### High Risk: Parser Complexity
**Mitigation**: Incremental implementation, extensive testing

### Medium Risk: Performance Impact
**Mitigation**: Profile and optimize hot paths

### Low Risk: Breaking Changes
**Mitigation**: Feature flags, gradual rollout

## Resource Requirements

- **Developer Time**: 4-6 weeks total
- **Testing**: Kaos brownie corpus + Lumarian/Mirrorbound
- **Documentation**: Update user guide with new features

## Contract References

| Contract | Title | Status | Priority |
|----------|-------|---------|----------|
| [ADC-012](contracts/adc-012-violation-detection.qmd) | Violation Detection Framework | Partial | 1 |
| [ADC-013](contracts/adc-013-process-parsing.qmd) | Process Body Parsing | Active | 1 |
| [ADC-014](contracts/adc-014-warning-infrastructure.qmd) | Warning Infrastructure | Planned | 3 |
| [ADC-011](contracts/adc-011-kaos-brownie.qmd) | Kaos Brownie Testing | Complete | - |

---

**Note**: This roadmap prioritizes fixing false positives (ADC-013) before adding new features, as the current 100% false positive rate makes the tool unusable in production.