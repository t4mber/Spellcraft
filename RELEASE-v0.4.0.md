# Spellcraft v0.4.0 Release

**Release Date:** 2025-11-10
**Type:** Minor Release
**Focus:** Parser Enhancement & Process Analysis

---

## 🎯 Release Summary

Spellcraft v0.4.0 brings significant parser enhancements with full work library support, complete process body parsing, and signal usage tracking infrastructure. This release implements three major ADC contracts and expands the test suite by 29%.

---

## ✨ Highlights

### Work Library Support (ADC-008)
Complete implementation of VHDL library and use clause parsing:
- Library declarations: `library work;`
- Multi-level packages: `use work.core_pkg.all;`
- Full VHDL-2008 compliance for context items

### Process Body Parsing (ADC-013)
Full process statement analysis with control flow:
- If/elsif/else statements
- Case statements with when clauses
- Loop statements (for/while)
- Wait statements
- Signal and variable assignments

### Signal Usage Infrastructure (ADC-012)
Foundation for violation detection:
- Signal declaration tracking
- Architecture statement categorization
- Process sensitivity list analysis

---

## 📊 Metrics

**Test Coverage:**
- Total Tests: 36 (↑29% from v0.3.0)
- Success Rate: 100% (36/36 passing)
- New Test Suites: Work Library (3), Process Parsing (8)

**Code Quality:**
- Zero compilation warnings in core modules
- Clean test execution (<1s total)
- Stable memory usage

**Build Performance:**
- Clean Build: ~15s
- Incremental: ~3s
- Test Suite: <1s

---

## 🔧 Technical Changes

### AST Enhancements

**New Types:**
```haskell
-- Library support
data LibraryDeclaration = ...
data UseClause = ...

-- Architecture statements
data SignalDecl = ...
data ArchStatement = ProcessStmt | ConcurrentAssignment | ComponentInstStmt

-- Process statements
data Statement = SignalAssignment | VariableAssignment | IfStatement
               | CaseStatement | LoopStatement | WaitStatement | NullStatement

-- Expressions
data Expression = IdentifierExpr | LiteralExpr | BinaryExpr | UnaryExpr
                | FunctionCall | IndexedName | Aggregate
```

### Parser Improvements

**Enhanced Modules:**
- `src/VHDL/Parser.hs` - Process and expression parsing
- `src/VHDL/AST.hs` - New statement and expression types
- `test/VHDL/Parser/WorkLibrarySpec.hs` - New test suite

**ADC Compliance:**
- ADC-008 Phase 1: ✅ Complete
- ADC-012 Priority 1: ✅ Complete
- ADC-013: ✅ Complete

---

## 🧪 Testing

### Test Suite Breakdown

**Type-Level Tests (25):**
- Clash frequency checking
- PLL constraint validation
- Domain crossing verification

**Work Library Tests (3):**
- Library declaration parsing
- Use clause parsing
- Multi-level package names

**Process Parsing Tests (8):**
- If/elsif/else statements
- Case statements
- Loop statements
- Signal assignments
- Expression parsing

---

## 📖 Examples

### Work Library Support

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.core_pkg.all;      -- ✅ Now supported!
use work.timing_pkg.types;   -- ✅ Multi-level packages

entity my_design is
  port (clk : in std_logic);
end entity;
```

### Process Parsing

```vhdl
process (clk, rst)
begin
  if rst = '1' then           -- ✅ If statements
    counter <= (others => '0');
  elsif rising_edge(clk) then -- ✅ Elsif support
    case state is             -- ✅ Case statements
      when IDLE =>
        counter <= counter + 1;
      when others =>
        counter <= (others => '0');
    end case;
  end if;
end process;
```

---

## 🔄 Migration Guide

### From v0.3.0 to v0.4.0

**No Breaking Changes** - This is a fully backward-compatible release.

**New Capabilities:**
1. Work library clauses now parse correctly
2. Process bodies are fully analyzed
3. Signal usage can be tracked

**API Changes:**
- None - all existing APIs remain stable

**Recommended Actions:**
1. Update to v0.4.0: `stack install` or `cabal install`
2. Run existing code - should work without changes
3. Leverage new features for deeper analysis

---

## 🐛 Known Issues

**ADC-008 Phase 2 (Multi-Unit):**
- Multi-entity/architecture files partially supported
- Standalone architectures (without entity in same file) not yet supported
- Target: v0.5.0

**ADC-012 Priorities 2-3:**
- Control flow analysis pending
- Arithmetic bounds checking pending
- Target: v0.5.0

---

## 🚀 What's Next

### v0.5.0 Roadmap

**Parser Enhancements:**
- Multi-unit file support (ADC-008 Phase 2)
- Standalone architecture support
- Package declaration parsing

**Violation Detection:**
- Latch inference detection (ADC-012 Priority 2)
- Arithmetic overflow detection (ADC-012 Priority 3)
- Control flow completeness checking

**Analysis Improvements:**
- Signal usage violation detection
- Process sensitivity list validation
- Unreachable code detection

---

## 👥 Contributors

- ADC Code Generator (automated implementation)
- Spellcraft Team (design and testing)
- Claude Code (development assistant)

---

## 📝 Contract Compliance

This release satisfies the following ADC contracts:

✅ **ADC-008 Phase 1**: Work Library Support
✅ **ADC-012 Priority 1**: Signal Usage Infrastructure
✅ **ADC-013**: Process Body Parsing

**Contract Verification:**
- All acceptance tests passing
- Success metrics achieved
- Documentation complete

---

## 🔗 Resources

- **Documentation**: See README.md
- **Changelog**: See CHANGELOG.md
- **Contracts**: See contracts/ directory
- **Examples**: See examples-vhdl/ and examples-clash/

---

## 📦 Installation

**With Stack:**
```bash
stack build
stack test
stack install
```

**With Cabal:**
```bash
cabal build
cabal test
cabal install
```

**Verify Installation:**
```bash
spellcraft --version
# Expected: Spellcraft v0.4.0
```

---

**Download:** https://github.com/t4mber/Spellcraft/releases/tag/v0.4.0
**Report Issues:** https://github.com/t4mber/Spellcraft/issues
**License:** BSD-3-Clause
