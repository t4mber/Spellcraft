# Spellcraft ADC Contracts

This directory will contain Agent Design Contracts (ADC) for the Spellcraft project.

## Contract Naming Convention

Contracts follow the pattern: `adc-DDD-<topic>.qmd`

Where:
- `DDD` = Three-digit contract number (001, 002, etc.)
- `<topic>` = Short descriptive name for the contract's focus

## Current Contracts

| Contract | Topic | Status |
|----------|-------|--------|
| adc-001-vhdl-parser.qmd | VHDL Parser and AST Builder | Active |
| adc-002-component-library.qmd | Component Constraint Library | Active |
| adc-003-clock-propagation.qmd | Clock Domain Propagation Analyzer | Active |
| adc-004-combinatorial-analysis.qmd | Combinatorial Path Complexity Analyzer | Active |
| adc-005-cli-reporting.qmd | Command-Line Interface and Error Reporting | Active |
| adc-006-clash-type-level.qmd | Clash Type-Level Constraint Modeling | Active |
| adc-007-clock-sources.qmd | Clock Source Detection and Signal Flow | Draft |

## Usage

These contracts define the specification for each component of the Spellcraft hardware verification tool. Each contract includes:

- **Purpose**: What the component achieves
- **Scope**: What's included and excluded
- **Interface**: Public API and data structures
- **Dependencies**: Which other contracts this depends on
- **Success Criteria**: How to measure completion

## ADC Schema

Contracts follow the ADC (Agent Design Contracts) methodology. See `adc-schema.qmd` for the complete schema definition.

## Migration Notes

This directory was separated from the main Spellcraft repository to:
1. Keep contracts independent of implementation
2. Enable contract reuse across related projects
3. Simplify contract version management
4. Allow contracts to be referenced as a separate concern

Previous location: `spellcraft/contracts/spellcraft-adc-*.qmd`
New location: `spellcraft-contracts/adc-*.qmd`
