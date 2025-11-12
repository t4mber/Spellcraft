# Spellcraft Test Suite

## Test Fixtures

### corpus_test.py

Comprehensive corpus testing script that validates Spellcraft against three test corpora:

**Usage**:
```bash
python3 tests/corpus_test.py
```

**Output**: Detailed metrics for:
- Lumarian corpus (13 files) - Production video DSP
- Mirrorbound corpus (10 files) - Production video processor
- Kaos corpus (4 files) - Intentional violations for detection validation

**Metrics Reported**:
- Parse success rate
- Clean pass rate (files with no violations)
- Violation detection accuracy
- Per-file status breakdown

## Test Corpora

### contrib/lzx/lumarian/
Clean production VHDL for video enhancement (signal processing)
- **Purpose**: Validate false positive rate on well-written code
- **Expected**: High clean pass rate (target: 75%+)

### contrib/lzx/mirrorbound/
Clean production VHDL for video delay and mirroring
- **Purpose**: Test different architectural patterns
- **Expected**: High clean pass rate (target: 70%+)

### contrib/lzx-kaos-levels/
Intentionally broken VHDL with injected violations
- **Purpose**: Validate violation detection accuracy
- **Expected**: Violations correctly detected in 3/4 files

## Running Tests

### Quick Test (Single File)
```bash
stack exec spellcraft -- contrib/lzx/lumarian/filter.vhd
```

### Full Corpus Test
```bash
python3 tests/corpus_test.py
```

### Manual Testing
```bash
# Test all Lumarian files
for f in contrib/lzx/lumarian/*.vhd; do
  echo "Testing $f..."
  stack exec spellcraft -- "$f"
done
```

## Expected Results (0.4.0)

| Corpus | Parse Success | Clean Pass |
|--------|---------------|------------|
| Lumarian | 100% | 76% |
| Mirrorbound | 90% | 40% |
| Kaos | 100% | 25% (expected) |
| **Overall** | **96%** | **55%** |

## Interpreting Results

**✅ PASS**: No violations detected
**⚠️ VIOLATIONS**: Issues found (may be false positives or real issues)
**❌ FAILED**: Parser could not process the file

### Known Issues
- delay.vhd: Array indexing false positive
- lumarian.vhd, multiplier.vhd: External entity references (expected)
- video_timing_generator.vhd: Parse failure (under investigation)
