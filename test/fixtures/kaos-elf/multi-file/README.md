# KAOS ELF Level 6: The Shadowbound Grimoire

*Where violations lurk not within a single tome, but span across the scattered pages of a fractured spellbook...*

## The Arcane Challenge

Level 6 introduces **cross-dimensional violations** — defects that can only be perceived when the practitioner holds *multiple scrolls simultaneously* and traces the ethereal threads connecting them.

## The Grimoire Structure

```
test/fixtures/kaos-elf/multi-file/
├── level6_top.vhd           # The Binding Circle (top-level summoner)
├── level6_component_a.vhd   # The Arithmantic Engine (ALU familiar)
├── level6_component_b.vhd   # The Memory Vessel (register familiar)
└── README.md                # Forbidden knowledge
```

## The Phantasmal Architecture

### The Arithmantic Engine (`level6_component_a.vhd`)
An ALU entity with two output conduits:
- `result` — carries computed essence
- `valid` — signals manifestation complete

### The Memory Vessel (`level6_component_b.vhd`)
A register entity with singular output:
- `q` — the stored quintessence

### The Binding Circle (`level6_top.vhd`)
The master summoner that instantiates both familiars:

```vhdl
-- Signals bound to familiar outputs (LEGITIMATE BINDINGS)
signal alu_result : signed(15 downto 0);   -- Fed by arithmantic engine
signal alu_valid  : std_logic;              -- Fed by arithmantic engine
signal reg_q      : signed(15 downto 0);   -- Fed by memory vessel

-- CURSED SIGNALS (THE VIOLATIONS)
signal orphan_signal : std_logic;          -- Declared but NEVER BOUND
signal dead_signal   : std_logic;          -- Bound to void, never read
```

## The Shadowbound Violations

### Violation 1: The Orphan Signal
```vhdl
signal orphan_signal : std_logic;  -- A spirit without a source
```
This signal exists in declaration only — no familiar feeds it, no assignment gives it life. It is **undriven**, a hollow vessel waiting eternally for essence that never arrives.

**Detection requires:** Knowing that NO component in the multi-file context provides this signal.

### Violation 2: The Dead Signal
```vhdl
signal dead_signal : std_logic;
dead_signal <= alu_valid;  -- Receives essence...
-- ...but is NEVER READ. Its power dissipates into the aether.
```
Energy flows in but never out. The signal is **unused** — a battery connected to nothing.

## Why Multi-File Scrying is Required

When analyzing `level6_top.vhd` **in isolation**, a lesser tool would cry:
> "alu_result is undriven! alu_valid is undriven! reg_q is undriven!"

But these are **false accusations**! The signals ARE driven — by component instantiations whose definitions exist in *other files*.

Only by constructing the **AnalysisContext** — a unified map of all entities across the dimensional boundaries — can Spellcraft correctly perceive:

```haskell
buildContext :: [VHDLDesign] -> AnalysisContext
-- Weaves the scattered grimoire pages into coherent knowledge

isKnownComponentOutput :: AnalysisContext -> ComponentInst -> Identifier -> Bool
-- "Does this familiar's true form include an output port of this name?"
```

## The Incantation

```bash
# Single-scroll analysis (INCOMPLETE VISION)
stack exec spellcraft -- test/fixtures/kaos-elf/multi-file/level6_top.vhd
# May report false phantoms

# Multi-scroll analysis (TRUE SIGHT)
stack exec spellcraft -- test/fixtures/kaos-elf/multi-file/*.vhd
# Correctly identifies ONLY the true violations:
#    - orphan_signal (undriven)
#    - dead_signal (unused)
```

## The Lesson

*"A spell is not merely its visible runes, but the invisible threads connecting it to all other workings in the practitioner's library."*

Level 6 teaches that **context is everything**. Violations that seem absent in isolation may lurk in the spaces between files, and false violations may vanish when the full picture emerges.

---

## Violation Tracking

| Signal | True Status | Single-File | Multi-File | Notes |
|--------|-------------|-------------|------------|-------|
| `alu_result` | Driven by component | OK (heuristic) | OK (definitive) | Bound to familiar output |
| `alu_valid` | Driven by component | OK (heuristic) | OK (definitive) | Bound to familiar output |
| `reg_q` | Driven by component | OK (heuristic) | OK (definitive) | Bound to familiar output |
| `orphan_signal` | Truly undriven | DETECTED | DETECTED | The Orphan — true violation |
| `dead_signal` | Truly unused | DETECTED | DETECTED | The Dead — true violation |

## Contract Reference

- **ADC-029**: Multi-File Analysis Context
- **ADC-012**: Signal Usage Analysis

---

**KAOS ELF Rating:** Level 6 (Cross-Dimensional)
**Detection Difficulty:** Requires multi-file awareness
**Spellcraft Status:** VANQUISHED
