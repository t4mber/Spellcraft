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
- **Clock Domain Crossing**: Detect unsynchronized signals crossing clock domains
- **Multi-File Analysis**: Cross-file entity resolution and context building
- **Warning vs Error Severity**: Configurable with `--strict` and `--suppress-warnings`
- **Generate Statement Parsing**: For-generate and if-generate support
- **Enhanced Videomancer**: 4K/8K UHD, HDR metadata, and audio timing validation
- **CI/CD Integration**: JSON and SARIF export for GitHub Code Scanning
- **Clear Error Messages**: Precise file:line:column error reporting with color coding
- **Comprehensive Testing**: 100% parse success, 4/4 KAOS ELF levels detected
- **Fast Performance**: Sub-second analysis (~0.2s per file)

## Status

**Version:** 0.8.0
**Release Date:** 2025-12-08
**Build:** Passing
**Tests:** 100% parse success, 63/63 unit tests, 4/4 KAOS ELF levels detected
**Features:** VHDL parsing, violation detection, multi-file analysis, videomancer, CI/CD export
**Quality:** 100% parse rate, full violation detection suite, SARIF/JSON export

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

# Analyze multiple files (enables cross-file analysis)
stack exec spellcraft -- *.vhd

# Export as SARIF for GitHub Code Scanning
stack exec spellcraft -- --format=sarif src/*.vhd > results.sarif

# Export as JSON for CI/CD pipelines
stack exec spellcraft -- --format=json src/*.vhd > results.json

# Videomancer mode for video hardware
stack exec spellcraft -- --videomancer --config lumarian.json src/*.vhd

# Run test suite
stack test
```

### Output Example

```
test/fixtures/kaos-elf/level1-undriven.vhd:44:3: error: Signal declared but never assigned (undriven)
  Signal: 'THIS_SIGNAL_IS_NEVER_USED'
✗ Found 1 error(s), 0 warning(s)
```

For clean code:
```
✓ Analysis complete. No issues found.
```

## Testing

Spellcraft includes comprehensive test infrastructure:

- **KAOS ELF**: Chaos Analysis & Organized System for Evaluating Logic Flaws (4 levels)
- **Unit Tests**: 63 tests covering parser, analysis, and multi-file context
- **Parser Fixtures**: Test files for parser development

### Test Results (v0.8.0)

| Corpus | Files | Parse | Detection |
|--------|-------|-------|-----------|
| LZX Lumarian | 13 | 100% | - |
| LZX Mirrorbound | 10 | 100% | - |
| Codeglow | 4 | 100% | - |
| KAOS ELF | 4 | 100% | 4/4 levels |
| **Overall** | **31** | **100%** | **100%** |

### KAOS ELF Levels

| Level | Name | Violation Type | Status |
|-------|------|----------------|--------|
| 1 | The Hollow Vessel | Undriven signal | VANQUISHED |
| 2 | The Flickering Flame | Latch inference | VANQUISHED |
| 3 | The Silent Overflow | Unbounded counter | VANQUISHED |
| 4 | The Dimensional Rift | Clock domain crossing | VANQUISHED |
| 6 | The Shadowbound Grimoire | Cross-file violations | VANQUISHED |

See `RELEASE-v0.8.0.md` for full release notes.

## Project Structure

```
spellcraft/
├── src/VHDL/
│   ├── Parser.hs                    # VHDL parser
│   ├── AST.hs                       # AST types
│   ├── Analysis/
│   │   ├── SignalUsage.hs           # Signal usage analysis
│   │   ├── ControlFlow.hs           # Latch inference detection
│   │   ├── ArithmeticBounds.hs      # Counter overflow detection
│   │   └── MultiFile.hs             # Cross-file context building
│   ├── Videomancer/                 # Video hardware validation
│   └── CLI/
│       ├── Report.hs                # Analysis reporting
│       └── Export.hs                # JSON/SARIF export
├── test/
│   ├── fixtures/kaos-elf/           # KAOS ELF violation tests
│   │   ├── level1-undriven.vhd      # The Hollow Vessel
│   │   ├── level2-partial.vhd       # The Flickering Flame
│   │   ├── level3-bitgrowth.vhd     # The Silent Overflow
│   │   ├── level4-cdc.vhd           # The Dimensional Rift
│   │   └── multi-file/              # The Shadowbound Grimoire
│   └── VHDL/                        # Unit test specs
├── contracts/                       # ADC contracts
└── docs/                            # Documentation
```

## CI/CD Integration

Spellcraft includes GitHub Actions workflows for automated analysis. See [`.github/README.md`](.github/README.md) for full documentation.

### Quick Start

```yaml
# .github/workflows/analyze.yml
- name: Analyze VHDL
  run: stack exec spellcraft -- --format=json src/*.vhd > report.json

- name: Upload SARIF to GitHub Security
  uses: github/codeql-action/upload-sarif@v3
  with:
    sarif_file: results.sarif
```

### Export Formats

| Format | Use Case |
|--------|----------|
| `--format=text` | Human-readable output (default) |
| `--format=json` | CI pipelines, custom tooling |
| `--format=sarif` | GitHub Code Scanning integration |

## Philosophy: Hardware as Spellcraft

Just as a wizard carefully crafts each spell with precise incantations and components, hardware engineers craft designs with careful attention to physical constraints. Spellcraft helps you:

- **Learn the craft**: Clear error messages guide you toward correct designs
- **Catch mistakes early**: Find constraint violations before synthesis
- **Build confidence**: Type-level guarantees where possible
- **Understand tradeoffs**: See exactly where and why constraints are violated

## License

BSD-3-Clause
