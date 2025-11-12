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
- **Component Output Tracking**: Heuristic-based detection of component output ports
- **Process Body Parsing**: Full support for if/elsif/else, signal assignments, and control flow
- **Work Library Support**: Complete VHDL library and use clause parsing
- **Clear Error Messages**: Precise file:line:column error reporting with source location tracking
- **Comprehensive Testing**: 96% parse success on 27 production VHDL files
- **Fast Performance**: Sub-second analysis (~0.2s per file)

## Status

**Version:** 0.4.0
**Release Date:** 2025-11-12
**Build:** ✅ Passing
**Tests:** ✅ 96% parse success (26/27 files), 75% violation detection
**Features:** ✅ VHDL parsing, signal usage analysis, component output tracking
**Quality:** ✅ 55% clean pass on production code, 0 crashes

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

### Test Results (v0.4.0)

| Corpus | Files | Parse Success | Clean Pass |
|--------|-------|---------------|------------|
| Lumarian | 13 | 100% | 76% |
| Mirrorbound | 10 | 90% | 40% |
| KAOS ELF | 4 | 100% | 75% detection |
| **Overall** | **27** | **96%** | **55%** |

See `docs/reports/2025-11-12-comprehensive-evaluation.md` for full evaluation report.

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
