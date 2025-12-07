# ADC-028: Parser Backlog - Conditional Assignments and Generate Statements

**Contract ID:** spellcraft-adc-028
**Status:** Active
**Created:** 2025-12-06
**Priority:** Medium

## Overview

Implement remaining parser backlog items to complete VHDL-2008 concurrent statement support:
1. Conditional Signal Assignments (verify existing implementation)
2. Generate Statements (for-generate and if-generate)

## Interface

### AST Extensions

```haskell
-- ADC-IMPLEMENTS: spellcraft-adc-028
data ArchStatement
  = ProcessStmt { ... }
  | ConcurrentAssignment { ... }
  | ComponentInstStmt ComponentInst
  | GenerateStmt GenerateStatement  -- NEW

-- ADC-IMPLEMENTS: spellcraft-adc-028
data GenerateStatement = GenerateStatement
  { genLabel :: Identifier
  , genScheme :: GenerateScheme
  , genStatements :: [ArchStatement]
  , genLocation :: SourceLocation
  }

-- ADC-IMPLEMENTS: spellcraft-adc-028
data GenerateScheme
  = ForGenerate Identifier Expression Expression SliceDirection
    -- for i in start to/downto end
  | IfGenerate Expression
    -- if condition
```

### Parser Extensions

```haskell
-- ADC-IMPLEMENTS: spellcraft-adc-028
-- Parse generate statement
generateStmt :: Parser ArchStatement

-- Pattern: label: for i in start to end generate ... end generate [label];
-- Pattern: label: if condition generate ... end generate [label];
```

## Behavior

### Conditional Signal Assignments (ADC-027 - Verify)

The following VHDL patterns must parse successfully:

```vhdl
-- Simple conditional
output <= a when sel = '1' else b;

-- Chained conditional
output <= a when sel = "00" else
          b when sel = "01" else
          c;

-- With complex expressions
result <= x + y when enable = '1' else (others => '0');
```

**Status:** Already implemented via `parseConditionalExpr` in Parser.hs.
Verification needed to confirm all patterns work.

### Generate Statements (NEW)

#### For-Generate

```vhdl
gen_label: for i in 0 to N-1 generate
  inst: component_name port map (
    clk => clk,
    data => data(i)
  );
end generate [gen_label];
```

Must support:
- Required label before `for`
- Loop variable (`i`) becomes available in generate scope
- `to` and `downto` directions
- Nested component instantiations
- Nested signal assignments
- Optional label after `end generate`

#### If-Generate

```vhdl
gen_label: if CONDITION generate
  -- declarations (optional)
  -- statements
end generate [gen_label];
```

Must support:
- Required label before `if`
- Boolean/expression condition
- Nested statements
- Optional label after `end generate`

## Parity

### Files to Modify

| File | Changes |
|------|---------|
| `src/VHDL/AST.hs` | Add `GenerateStmt`, `GenerateStatement`, `GenerateScheme` |
| `src/VHDL/Parser.hs` | Add `generateStmt` parser, update `archStatement` |
| `src/VHDL/Lexer.hs` | Add `generate` to reserved keywords |

### Test Fixtures

| File | Purpose |
|------|---------|
| `test/fixtures/conditional_assign.vhd` | Test conditional signal assignments |
| `test/fixtures/generate_stmt.vhd` | Test for-generate and if-generate |

### Test Specifications

| File | Purpose |
|------|---------|
| `test/VHDL/Parser/GenerateSpec.hs` | Unit tests for generate statements |

## Success Criteria

1. **Parse Rate**: Maintain 100% on existing LZX corpus (27 files)
2. **Conditional Assignments**: Parse all forms without error
3. **For-Generate**: Parse for-generate with to/downto
4. **If-Generate**: Parse if-generate with conditions
5. **Test Coverage**: All 39+ existing tests pass
6. **New Tests**: Add at minimum 4 new tests for backlog features

## Dependencies

- ADC-008: VHDL Parser Enhancement (foundation)
- ADC-013: Process Body Parsing (statement parsing patterns)
- ADC-027: Conditional Expression Support (already implemented)

## References

- IEEE 1076-2008 Section 11.8 (Generate Statements)
- IEEE 1076-2008 Section 10.5.3 (Conditional Signal Assignments)
- IMPLEMENTATION_ROADMAP.md Parser Enhancement Backlog
