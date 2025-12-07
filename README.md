# Spellcraft

```
    ✧                                                                    ·
  ·    _____ ____  _____ __    __    __________  ___   ____________   ✨
      / ___// __ \/ ___// /   / /   / ____/ __ \/   | / ____/_  __/  ✦
  ⋆  \__ \/ /_/ //  _  / /   / /   / /   / /_/ / /|  / /_    / /  ·
 ✧  ___/ / ____// __/ / /___/ /___/ /___/ _, _/  __ /  __/✨/ /      ·
   /____/_/    /____/_____/_____/\____/_/ |_/_/  |_/_/     /_/    ⋆
        ·           ✦      ·    ˚       ✧        ·          ✨
    ✨     ·    ⋆      ✧       ·    ˚      ⋆         ·        ✦

         Safe · Programmable ✦ Electronics · Linting · Library ˚ &
              Code ⋆ Review · Analysis ✧ Framework · Tool
```

A hardware design verification tool that helps you _craft_ your hardware designs correctly. Spellcraft verifies physical constraints (frequencies, thermal limits, etc.) before synthesis using Haskell/Clash's type-level programming features.

## Features

- **Signal Usage Analysis**: Detect undriven and unused signals in VHDL designs
- **Latch Inference Detection**: Find incomplete assignments that synthesize to latches
- **Unbounded Counter Detection**: Identify counters without overflow protection
- **Warning vs Error Severity**: Configurable with `--strict` and `--suppress-warnings`
- **Generate Statement Parsing**: For-generate and if-generate support
- **Clear Error Messages**: Precise file:line:column error reporting with color coding
- **Comprehensive Testing**: 100% parse success on LZX corpus, 3/3 Kaos Elf levels
- **Fast Performance**: Sub-second analysis (~0.2s per file)

## Status

**Version:** 0.6.0
**Release Date:** 2025-12-04
**Build:** Passing
**Tests:** 100% parse success, 39/39 unit tests, 3/3 Kaos Elf levels detected
**Features:** VHDL parsing, violation detection, warning infrastructure, generate statements
**Quality:** 100% parse rate, latch inference + unbounded counter detection

## Installation

### Prerequisites

**Option 1: Using Cabal directly**
- GHC 9.2 or later
- Cabal 3.0 or later
- Clash (will be installed via cabal)

**Option 2: Using Stack (recommended)**
- Stack 2.0 or later
- GHC and Cabal will be managed by Stack

### Build

**Using Cabal:**
```bash
cabal update
cabal build
```

**Using Stack:**
```bash
# Install Stack if you haven't already
# macOS: brew install haskell-stack
# Linux: curl -sSL https://get.haskellstack.org/ | sh

# Install Cabal via Stack
stack install cabal-install

# Build the project
stack build
```

### Install

**Using Cabal:**
```bash
cabal install
```

**Using Stack:**
```bash
stack install
```

## Usage

```bash
# Analyze VHDL files
stack exec spellcraft -- mydesign.vhd

# Analyze multiple files
stack exec spellcraft -- *.vhd

# Example with production code
stack exec spellcraft -- contrib/lzx/lumarian/filter.vhd

# Run comprehensive test suite
python3 tests/corpus_test.py
```

### Output Example

```
contrib/lzx-kaos-levels/enhance-level1-undriven.vhd:44:3: error: Signal declared but never assigned (undriven)
  Signal: 'THIS_SIGNAL_IS_NEVER_USED'
✗ Found 1 error(s), 0 warning(s)
```

For clean code:
```
✓ Analysis complete. No issues found.
```

## Testing

Spellcraft includes comprehensive test infrastructure:

- **Test Corpora**: 27 production VHDL files (Lumarian, Mirrorbound, KAOS ELF)
- **Parser Fixtures**: 6 unit test files for parser development
- **Automated Testing**: `python3 tests/corpus_test.py`

### Test Results (v0.6.0)

| Corpus | Files | Parse | Detection |
|--------|-------|-------|-----------|
| LZX Lumarian | 13 | 100% | - |
| LZX Mirrorbound | 10 | 100% | - |
| Codeglow | 4 | 100% | - |
| Kaos Elf | 3 | 100% | 3/3 levels |
| **Overall** | **30** | **100%** | **100%** |

See `RELEASE-v0.6.0.md` for full release notes.

## Project Structure

```
spellcraft/
├── src/VHDL/
│   ├── Parser.hs           # VHDL parser
│   ├── AST.hs              # AST types
│   └── Analysis/
│       └── SignalUsage.hs  # Signal usage analysis
├── tests/
│   ├── corpus_test.py      # Comprehensive testing
│   └── fixtures/parser/    # Parser unit tests
├── contrib/
│   ├── lzx/lumarian/       # Production VHDL (13 files)
│   ├── lzx/mirrorbound/    # Production VHDL (10 files)
│   └── lzx-kaos-levels/    # Violation test cases (4 files)
├── docs/reports/           # Evaluation reports
├── contracts/              # ADC contracts
└── policies/               # Development policies
```

## Philosophy: Hardware as Spellcraft

Just as a wizard carefully crafts each spell with precise incantations and components, hardware engineers craft designs with careful attention to physical constraints. Spellcraft helps you:

- **Learn the craft**: Clear error messages guide you toward correct designs
- **Catch mistakes early**: Find constraint violations before synthesis
- **Build confidence**: Type-level guarantees where possible
- **Understand tradeoffs**: See exactly where and why constraints are violated

## License

BSD-3-Clause
