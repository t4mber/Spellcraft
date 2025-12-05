# Spellcraft v0.5.0 Release

**Release Date:** 2025-12-04
**Type:** Minor Release
**Focus:** Parser Enhancements & 100% LZX Parse Rate
**Status:** APPROVED FOR PRODUCTION

---

## Release Summary

Spellcraft v0.5.0 achieves **100% parse success** on the LZX corpus (23 files) through parser enhancements for multi-signal declarations, based literals, and for-loop direction parsing. This release also adds the codeglow VHDL corpus with full parsing support.

### Key Achievement

**100% Parse Rate** on all test corpora:
- LZX Lumarian: 13/13 files
- LZX Mirrorbound: 10/10 files
- Codeglow: 4/4 files

---

## Highlights

### Multi-Signal Declaration Support (ADC-008)
VHDL allows declaring multiple signals in one statement:
```vhdl
signal sig_a, sig_b, sig_c : std_logic;
signal data_r, data_g, data_b : unsigned(7 downto 0);
```
The parser now correctly handles this pattern, returning separate `SignalDecl` entries for each signal name.

### Based Literal Support (ADC-008)
VHDL based literals for signal initialization:
```vhdl
signal lfsr_state : unsigned(15 downto 0) := x"BEEF";
signal mask : std_logic_vector(3 downto 0) := b"1010";
```
Supports hex (`x"..."`, `X"..."`), binary (`b"..."`, `B"..."`), and octal (`o"..."`, `O"..."`).

### For-Loop Direction Parsing Fix
Fixed backtracking issue where `downto` keyword prevented `to` from matching:
```vhdl
for i in 0 to N loop  -- Now parses correctly
for j in 15 downto 0 loop  -- Still works
```

### Codeglow Corpus
Added 4 VHDL files from the codeglow project demonstrating:
- Multi-signal declarations
- Hex literal initializers
- Function declarations in architecture
- Complex signal assignments

---

## Metrics

### Parse Success Rate

| Corpus | v0.4.0 | v0.5.0 | Change |
|--------|--------|--------|--------|
| **LZX Lumarian** | 100% | 100% | - |
| **LZX Mirrorbound** | 90% | 100% | **+10%** |
| **Codeglow** | 50% | 100% | **+50%** |
| **Overall** | 96% | 100% | **+4%** |

### Test Suite

- **Unit Tests:** 39/39 passing
- **New Fixtures:** 3 files added
- **Regression:** None

---

## Technical Changes

### Parser (src/VHDL/Parser.hs)

**Multi-Signal Declarations:**
```haskell
signalDecl :: Parser [SignalDecl]
signalDecl = do
  names <- identifier `sepBy1` comma
  void colon
  sigType <- typeSpec
  initValue <- optional (symbol ":=" >> parseExpression)
  void semi
  pure [SignalDecl name sigType initValue loc | name <- names]
```

**For-Loop Direction Fix:**
```haskell
-- Before (broken)
dir <- (keyword "downto" >> pure DownTo) <|> (keyword "to" >> pure To)

-- After (fixed)
dir <- try (keyword "downto" >> pure DownTo) <|> (keyword "to" >> pure To)
```

### Lexer (src/VHDL/Lexer.hs)

**Based Literals:**
```haskell
basedLiteral :: Parser Text
basedLiteral = lexeme $ do
  base <- oneOf ['x', 'X', 'b', 'B', 'o', 'O']
  _ <- char '"'
  digits <- many (noneOf ['\"'])
  _ <- char '"'
  pure $ T.pack (base : '"' : digits ++ "\"")
```

### AST (src/VHDL/AST.hs)

**New Literal Type:**
```haskell
data Literal
  = IntLiteral Integer
  | RealLiteral Double
  | StringLiteral Text
  | CharLiteral Char
  | BitLiteral Bool
  | BasedLiteral Text  -- NEW: x"BEEF", b"1010", o"777"
```

---

## Files Changed

### Modified
- `src/VHDL/Parser.hs` - Multi-signal, for-loop fix
- `src/VHDL/Lexer.hs` - Based literal parser
- `src/VHDL/AST.hs` - BasedLiteral type
- `src/VHDL/Analysis/SignalUsage.hs` - Pattern match update
- `test/VHDL/Parser/WorkLibrarySpec.hs` - New tests
- `.gitignore` - Simplified contrib rules

### Added
- `contracts/adc-008-vhdl-parser-enhancement.qmd` (renamed from adc-009)
- `contracts/README.md`
- `contrib/t4mber/codeglow/` - 7 files (4 VHDL, 3 docs)
- `test/fixtures/codeglow_pattern.vhd`
- `test/fixtures/multi_signal_decl.vhd`
- `test/fixtures/test_hex_init.vhd`

---

## Upgrade Guide

### From v0.4.0

1. **Update installation:**
   ```bash
   git pull
   stack build
   ```

2. **Verify parse improvements:**
   ```bash
   stack exec spellcraft -- contrib/lzx/lumarian/*.vhd
   stack exec spellcraft -- contrib/lzx/mirrorbound/*.vhd
   ```

3. **Run tests:**
   ```bash
   stack test
   ```

### Breaking Changes

**None** - v0.5.0 is fully backward compatible with v0.4.0

---

## Known Limitations

### Signal Usage Analysis
Some files report false positives for signals assigned via:
- Array indexing patterns
- External entity references (requires multi-file analysis)

These are analysis limitations, not parse failures.

---

## Next Steps (v0.6.0 Roadmap)

### Parser Improvements
1. Conditional signal assignments (`when ... else`)
2. Generate statements
3. Configuration declarations

### Analysis Improvements
1. Multi-file analysis (ADC-015)
2. Control flow analysis (ADC-012 Priority 2)
3. Arithmetic bounds checking (ADC-012 Priority 3)

---

## Release Verdict

**Spellcraft v0.5.0 is APPROVED for production use.**

The system successfully:
- Parses 100% of LZX corpus (23 files)
- Parses 100% of codeglow corpus (4 files)
- Handles multi-signal declarations correctly
- Supports hex/binary/octal literals
- Maintains all 39 tests passing

---

**Release Date**: 2025-12-04
**Version**: 0.5.0
**Status**: Production Ready
