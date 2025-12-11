# KAOS ELF: The Chaos Evaluation Framework

*"In the realm of hardware design, chaos lurks in the shadows of poorly crafted spells..."*

## The KAOS ELF Grimoire

KAOS ELF (Chaos Analysis & Organized System for Evaluating Logic Flaws) is Spellcraft's test corpus of intentionally cursed VHDL designs. Each level presents increasingly subtle violations that challenge the analyzer's perception.

## The Levels of Chaos

### Level 1: The Hollow Vessel
**File:** `level1-undriven.vhd`
**Violation:** Undriven signal declaration

A signal declared but never assigned — an empty vessel that will synthesize to unknown values, corrupting all downstream logic.

```vhdl
signal THIS_SIGNAL_IS_NEVER_USED : std_logic;  -- The Hollow One
```

**Detection:** Basic signal usage tracking

---

### Level 2: The Flickering Flame
**File:** `level2-partial.vhd`
**Violation:** Latch inference from incomplete assignment

A signal assigned in some code paths but not others. The synthesizer will infer a latch to "remember" the value — often unintentionally.

```vhdl
if condition then
  s_sometimes_driven <= '1';  -- Assigned here...
end if;                        -- ...but not in else!
```

**Detection:** Control flow analysis, path coverage

---

### Level 3: The Silent Overflow
**File:** `level3-bitgrowth.vhd`
**Violation:** Unbounded counter without overflow protection

A counter that increments forever, eventually wrapping silently. No saturation, no error — just silent corruption.

```vhdl
s_accumulator <= s_accumulator + 1;  -- Grows without bound
```

**Detection:** Arithmetic bounds checking, counter pattern recognition

---

### Level 4: The Dimensional Rift
**File:** `level4-cdc.vhd`
**Violation:** Clock Domain Crossing without synchronization

Signals that cross between clock domains without proper synchronizers create dimensional rifts in your design. The receiving flip-flop may capture a metastable value — neither 0 nor 1, but an unstable state that resolves unpredictably.

```vhdl
-- Fast domain (200 MHz)
s_data_crossing <= data_in;

-- Slow domain (50 MHz) - THE RIFT!
s_data_slow <= s_data_crossing;  -- No synchronizer!
```

**The Curse of Metastability:**
- Signal oscillates between logic levels
- May resolve to wrong value
- Corrupts downstream logic
- Only manifests on real hardware, often passes simulation

**The Proper Incantation (2-FF Synchronizer):**
```vhdl
process(clk_slow)
begin
  if rising_edge(clk_slow) then
    sync_ff1 <= async_signal;  -- May go metastable
    sync_ff2 <= sync_ff1;      -- Settles to stable value
  end if;
end process;
```

**Detection:** Clock domain analysis, signal tracing across process boundaries

---

### Level 5: The Delta Storm
**File:** `level5-race.vhd`
**Violation:** Delta cycle race condition

A signal that reads and writes in the same delta cycle, creating simulation/synthesis mismatch. This is intentionally malformed VHDL that tests parser robustness.

**Detection:** Beyond current scope (requires simulation semantics)

---

### Level 6: The Shadowbound Grimoire
**Directory:** `multi-file/`
**Violation:** Cross-file signal resolution

Violations that only manifest when analyzing multiple files together. Requires building a unified context across the dimensional boundaries of separate source files.

See [multi-file/README.md](multi-file/README.md) for the full arcane documentation.

**Detection:** Multi-file analysis context (ADC-029)

---

## The Incantations

```bash
# Test all single-file levels
stack exec spellcraft -- test/fixtures/kaos-elf/level1-undriven.vhd
stack exec spellcraft -- test/fixtures/kaos-elf/level2-partial.vhd
stack exec spellcraft -- test/fixtures/kaos-elf/level3-bitgrowth.vhd

# Test multi-file level
stack exec spellcraft -- test/fixtures/kaos-elf/multi-file/*.vhd
```

## Detection Matrix

| Level | Name | Violation Type | Spellcraft Status |
|-------|------|----------------|-------------------|
| 1 | The Hollow Vessel | Undriven signal | VANQUISHED |
| 2 | The Flickering Flame | Latch inference | VANQUISHED |
| 3 | The Silent Overflow | Unbounded counter | VANQUISHED |
| 4 | The Dimensional Rift | Clock domain crossing | FUTURE |
| 5 | The Delta Storm | Race condition | BEYOND SCOPE |
| 6 | The Shadowbound Grimoire | Cross-file | VANQUISHED |

## Contract References

- **ADC-011**: KAOS ELF Testing Framework
- **ADC-012**: Violation Detection (Levels 1-3)
- **ADC-029**: Multi-File Analysis (Level 6)

---

*"May your designs be free of chaos, and your signals ever driven."*
