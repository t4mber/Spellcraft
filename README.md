# VDHL Hardware-Safety Static Analyzer

A static analysis tool for VDHL that verifies physical hardware constraints (frequencies, thermal limits, etc.) before synthesis using Haskell/Clash's type-level programming features.

## Features

- **Frequency Constraint Checking**: Detect when clock frequencies exceed component maximums
- **Combinatorial Complexity Analysis**: Warn about long arithmetic chains that may cause timing issues
- **Type-Safe Design**: Leverage Haskell's type system to model physical constraints
- **Clear Error Messages**: Precise file:line:column error reporting

## Installation

### Prerequisites

- GHC 9.2 or later
- Cabal 3.0 or later
- Clash (will be installed via cabal)

### Build

```bash
cabal update
cabal build
```

### Install

```bash
cabal install
```

## Usage

```bash
# Run the analyzer (wrapper script)
./vdhl-analyzer examples/simple.vhd

# Or use cabal directly (until installed)
cabal run vdhl-analyzer -- examples/simple.vhd

# Analyze multiple files
./vdhl-analyzer examples/*.vhd

# Verbose output
./vdhl-analyzer --verbose examples/simple.vhd

# JSON output for tool integration
./vdhl-analyzer --format json examples/simple.vhd

# GCC-style output for editor integration
./vdhl-analyzer --format gcc examples/simple.vhd

# Get help
./vdhl-analyzer --help
```

## Contract-Driven Development

This project uses ADC (Agent Design Contracts) for development. See:
- `contracts/` - Contract specifications
- `adc-schema.qmd` - Contract schema definition
- `roles/` - ADC role definitions

## Project Structure

```
src/
  VDHL/
    AST.hs                  -- VDHL AST types
    Parser.hs               -- VDHL parser
    Lexer.hs                -- Tokenization
    SourceLocation.hs       -- Location tracking
    Constraint/             -- Constraint library
      Types.hs              -- Constraint types
      Library.hs            -- Component library
      Units.hs              -- Physical units
      Violation.hs          -- Violation detection
    Analysis/               -- Analysis passes
      ClockGraph.hs         -- Clock connectivity graph
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
```

## Example

Given this VDHL (PRD violation scenario):

```vdhl
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

The analyzer reports:

```
examples/pll_violation.vhd:23:5: error: Frequency violation
  Component 'YPbPr_Encoder_A' port 'pixel_clk' receives 208.0 MHz
  but maximum is 165.0 MHz

  Clock path:
    pixel_clk (50.0 MHz) → PLL_1 → high_clk (208.0 MHz) → encoder_inst.pixel_clk
```

## License

BSD-3-Clause
