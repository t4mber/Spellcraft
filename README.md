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

- **Frequency Constraint Checking**: Detect when clock frequencies exceed component maximums
- **Combinatorial Complexity Analysis**: Warn about long arithmetic chains that may cause timing issues
- **Type-Safe Design**: Leverage Haskell's type system to model physical constraints
- **Clear Error Messages**: Precise file:line:column error reporting with source location tracking
- **Dual Analysis**: Both compile-time (Clash/type-level) and runtime (VHDL parsing) analysis
- **Clock Source Detection**: Automatically identify clock sources from entity ports
- **Frequency Propagation**: Track clock frequencies through PLLs and component hierarchies

## Status

**Version:** 0.3.0
**Build:** ✅ Passing
**Tests:** ✅ 25/25 passing
**Features:** ✅ VHDL + Clash analysis support

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
# Analyze VHDL files (runtime analysis)
spellcraft examples-vhdl/simple.vhd

# Analyze Clash files (compile-time type checking via GHC)
spellcraft my-design.hs

# Analyze both VHDL and Clash files together
spellcraft examples-vhdl/*.vhd examples-clash/*.hs

# With Cabal
cabal exec spellcraft -- examples-vhdl/simple.vhd

# With Stack
stack exec spellcraft -- examples-vhdl/simple.vhd

# Analyze multiple files
spellcraft examples-vhdl/*.vhd

# Verbose output
spellcraft --verbose examples-vhdl/simple.vhd

# JSON output for tool integration
spellcraft --format json examples-vhdl/simple.vhd

# GCC-style output for editor integration
spellcraft --format gcc examples-vhdl/simple.vhd

# Get help
spellcraft --help
```

## Contract-Driven Development

This project uses ADC (Agent Design Contracts) for development. See:
- `contracts/` - Contract specifications
- `adc-schema.qmd` - Contract schema definition
- `roles/` - ADC role definitions

## Project Structure

```
src/
  VHDL/
    AST.hs                  -- VHDL AST types
    Parser.hs               -- VHDL parser
    Lexer.hs                -- Tokenization
    SourceLocation.hs       -- Location tracking
    Constraint/             -- Constraint library
      Types.hs              -- Constraint types
      Library.hs            -- Component library
      Units.hs              -- Physical units
      Violation.hs          -- Violation detection
    Clash/                  -- Type-level analysis
      Types.hs              -- Frequency types
      FrequencyCheck.hs     -- Type-level checks
      Domains.hs            -- Clock domains
      Constraints.hs        -- Constraints
    Analysis/               -- Runtime analysis passes
      ClockGraph.hs         -- Clock connectivity graph
      ClockSource.hs        -- Clock source detection
      Propagation.hs        -- Frequency propagation
      Violation.hs          -- Violation detection
      FrequencyCalc.hs      -- Frequency calculations
      Combinatorial.hs      -- Complexity analysis
      Process.hs            -- Process AST
      Expression.hs         -- Expression AST
    CLI/                    -- Command-line interface
      Options.hs            -- Option parsing
      Format.hs             -- Output formatting
      Report.hs             -- Report generation
      Color.hs              -- ANSI colors
  ComponentLibs/
    TestComponents.hs       -- Test component library
  Main.hs                   -- Entry point

examples-vhdl/              -- VHDL example designs
  simple.vhd                -- Basic example
  pll_violation.vhd         -- Frequency violation example
  ...                       -- More VHDL examples

examples-clash/             -- Clash type-level examples
  01_pll_violation.hs       -- Compile-time violation detection
  02_multiple_pll_cascading.hs
  03_valid_design.hs
  ...                       -- More Clash examples
```

## Example

Given this VHDL design (frequency violation scenario):

```vhdl
-- 50 MHz pixel clock
signal pixel_clk : std_logic;

-- PLL multiplies by 4.16 → 208 MHz
pll_inst : component PLL_1
  generic map (MULT_FACTOR => 4.16)
  port map (clk_in => pixel_clk, clk_out => high_clk);

-- YPbPr encoder max is 165 MHz
encoder_inst : component YPbPr_Encoder_A
  port map (pixel_clk => high_clk);  -- VIOLATION!
```

Spellcraft reports:

```
examples-vhdl/pll_violation.vhd:23:5: error: Frequency violation
  Component 'YPbPr_Encoder_A' port 'pixel_clk' receives 208.0 MHz
  but maximum is 165.0 MHz

  Clock path:
    pixel_clk (50.0 MHz) → PLL_1 → high_clk (208.0 MHz) → encoder_inst.pixel_clk
```

## Clash Type-Level Examples

Spellcraft also includes **compile-time** hardware constraint checking using Clash's type-level programming. These examples demonstrate how frequency violations can be caught by the Haskell type checker before any code runs.

### Running Clash Examples

**With the CLI (recommended):**
```bash
# Spellcraft automatically compiles Clash files and reports type errors
spellcraft my-design.hs

# Example output for a frequency violation:
# my-design.hs:10:13: error: Frequency violation
#   Component 'Type-level frequency constraint violation'
#   port 'type_check' receives 1.0 MHz but maximum is 1.0 MHz
```

**Interactively:**
```bash
# Interactive type checking
cabal repl

# Load an example
:load examples-clash/01_pll_violation.hs

# Try to create a violation (this will fail at compile time!)
let encoder = mkEncoder @165 "Enc" 8
let sig = mkHWSignal @208 "test" domain
connectEncoder encoder sig
-- ERROR: Couldn't match type 'False' with 'True'
```

### Available Examples

- **01_pll_violation.hs** - PLL output (208 MHz) exceeds encoder max (165 MHz)
- **02_multiple_pll_cascading.hs** - Cascaded PLLs (25 → 100 → 300 MHz) violate constraints
- **03_valid_design.hs** - Valid design that passes all type checks
- **04_vhdl_style_syntax.hs** - VHDL-style syntax for Clash designs
- **05_dsl_style.hs** - DSL approach for hardware specification

See `examples-clash/README.md` for detailed documentation on type-level checking.

## Philosophy: Hardware as Spellcraft

Just as a wizard carefully crafts each spell with precise incantations and components, hardware engineers craft designs with careful attention to physical constraints. Spellcraft helps you:

- **Learn the craft**: Clear error messages guide you toward correct designs
- **Catch mistakes early**: Find constraint violations before synthesis
- **Build confidence**: Type-level guarantees where possible
- **Understand tradeoffs**: See exactly where and why constraints are violated

## License

BSD-3-Clause
