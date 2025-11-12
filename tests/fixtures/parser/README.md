# Parser Test Fixtures

Minimal VHDL test cases for parser development and debugging.

## Files

### test-combo.vhd
**Purpose**: Test combinatorial logic sensitivity list parsing
**Features**:
- Process with signal in sensitivity list
- Signal assignment inside process
- Tests self-referential signal pattern (signal in its own sensitivity list)

### test-if.vhd
**Purpose**: Test if statement with rising_edge
**Features**:
- Clock signal parsing
- `rising_edge()` function call
- If statement inside process
- Sequential signal assignment

### test-keyword.vhd
**Purpose**: Test VHDL keyword parsing
**Features**: (needs inspection)

### test-process2.vhd
**Purpose**: Test process parsing variations
**Features**: (needs inspection)

### test-sensitivity.vhd
**Purpose**: Test sensitivity list parsing
**Features**: (needs inspection)

### test-signal-ref.vhd
**Purpose**: Test signal reference extraction
**Features**: (needs inspection)

## Usage

These fixtures are used during parser development to test specific VHDL constructs in isolation.

### Quick Test
```bash
# Test a single fixture
stack exec spellcraft -- tests/fixtures/parser/test-if.vhd

# Test all fixtures
for f in tests/fixtures/parser/test-*.vhd; do
  echo "Testing $f..."
  stack exec spellcraft -- "$f"
done
```

### Expected Behavior

All these fixtures should:
- ✅ Parse successfully (100% parse success)
- ✅ Have no signal usage violations (clean code)
- ❌ NOT have any intentional violations (unlike kaos corpus)

These are **minimal valid VHDL** for testing parser correctness, not for testing violation detection.

## Adding New Fixtures

When adding a new parser test case:

1. Create a minimal VHDL file testing ONE specific construct
2. Name it descriptively: `test-<feature>.vhd`
3. Add entry to this README documenting what it tests
4. Verify it parses correctly: `stack exec spellcraft -- test-<feature>.vhd`

## Difference from Other Test Corpora

| Corpus | Purpose | Size | Violations |
|--------|---------|------|------------|
| **fixtures/parser/** | Parser development | Minimal | None |
| contrib/lzx/lumarian/ | Real-world testing | Full modules | None |
| contrib/lzx/mirrorbound/ | Real-world testing | Full modules | None |
| contrib/lzx-kaos-levels/ | Violation detection | Full modules | Intentional |

These fixtures are for **parser unit testing**, not integration testing or violation detection.
