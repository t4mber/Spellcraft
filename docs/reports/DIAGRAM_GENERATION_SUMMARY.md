# LZX Diagram Generation Summary

**Date**: 2025-11-12
**Status**: ✅ Complete

## Objective

Create high-level block diagrams for LZX Lumarian and Mirrorbound video synthesizer programs, following the design philosophy: **"A patch with LZX P Series modules"** - representing systems as functional blocks (multiplier, filter, delay, etc.) rather than VHDL implementation details.

## Deliverables

### 1. High-Level Patch Diagrams (PRIMARY DELIVERABLES)

#### ✅ Lumarian Video Enhancement Patch
**File**: `contrib/lzx/lumarian/LUMARIAN_PATCH.md`

**Signal Chain**:
```
Video Input (YUV)
  ↓
LUMA: Contrast → Gamma → Enhance (Filter+Rectifier+Multiply) → Invert
CHROMA: Invert → Saturation → Delay (36 samples)
  ↓
Video Output (Enhanced YUV)
```

**Key Functions**:
- **CONTRAST**: Multiply + offset (brightness/contrast control)
- **GAMMA**: Power curve for tone mapping
- **ENHANCE**: High-pass filter + rectifier + multiply-add (detail boost)
- **SATURATION**: Chroma amplitude scaling
- **DELAY**: Latency compensation for chroma

**Controls**: 8 main knobs + 4 switches (12 registers)

**Creative Use**: Professional color grading, vintage film looks, hyper-real detail enhancement

---

#### ✅ Mirrorbound Delay & Mirror Patch
**File**: `contrib/lzx/mirrorbound/MIRRORBOUND_PATCH.md`

**Signal Chain**:
```
Video Input (YUV) + Sync
  ↓
TIMING: Sync Detector → Accumulators (Vertical & Horizontal Ramps)
  ↓
OFFSETS: Contrast A/B (Position + Displace) → AB Selector
  ↓
DELAY: Mirror Delay Line (2048 samples, dual read heads) → Blanking
  ↓
Video Output (Delayed + Mirrored)
```

**Key Functions**:
- **ACCUMULATORS**: Vertical/horizontal ramp generators
- **CONTRAST A/B**: Transform ramps into delay offsets
- **AB SELECTOR**: Choose between A or B offset per pixel
- **MIRROR DELAY**: Dual-channel delay with reverse read
- **BLANKING**: Position-based keying

**Controls**: 10 main knobs + 6 switches (13 registers)

**Creative Use**: Infinite mirror tunnels, temporal feedback, spatial displacement, geometric keying

---

### 2. Component Diagrams (SUPPLEMENTARY)

#### ✅ Enhance Module (Implementation Detail)
**File**: `contrib/lzx/lumarian/enhance_diagram.md`

Detailed breakdown of the detail enhancement pipeline showing all sub-components and internal signals.

#### ✅ Filter Module (Algorithm Detail)
**File**: `contrib/lzx/lumarian/filter_diagram.md`

In-depth explanation of sigma-delta mixing, IIR filtering, and multiplier-free implementation with performance specs.

---

### 3. Documentation & Guidelines

#### ✅ Diagram README
**File**: `contrib/lzx/README_DIAGRAMS.md`

**Contents**:
- Viewing methods (GitHub, VS Code, mermaid.live, CLI)
- Diagram conventions (color coding, notation, flow direction)
- Template for creating new diagrams
- Checklist of available/pending diagrams

#### ✅ Parsing Anti-Patterns Policy
**File**: `policy/parsing-anti-patterns.md`

**Purpose**: Document parser anti-patterns discovered during ADC-013 implementation

**Key Anti-Patterns**:
- AP-001: `manyTill` with consuming parsers (causes infinite loops)
- AP-002: `choice` without `try` wrappers (prevents backtracking)
- AP-003: Consuming whitespace before all alternatives
- AP-004: `manyTill` with ambiguous terminator

**Recommended Patterns**:
- RP-001: `many` with `notFollowedBy` guards
- RP-002: Lexeme-based whitespace handling
- RP-003: Top-level `try` in `choice`
- RP-004: Lookahead for disambiguation

---

## Design Philosophy Applied

### "A Patch with LZX P Series Modules"

All diagrams follow this principle:

1. **Functional Blocks**: Use familiar synthesis terms
   - MULTIPLIER (not "fixed-point arithmetic unit")
   - FILTER (not "IIR state machine")
   - DELAY (not "FIFO buffer")
   - RECTIFIER (not "absolute value circuit")

2. **Signal Flow**: Top-to-bottom or left-to-right
   - Clear input → processing → output
   - Controls feed in from sides
   - No implementation clutter

3. **Knobs & Switches**: User-facing controls
   - Position, Displace, Gain (not register addresses)
   - Mirror, Invert (not boolean flags)
   - Threshold, Rate (not accumulator divisors)

4. **Creative Context**: Real-world applications
   - "Vintage film look" (not "parameter set 0x42")
   - "Infinite mirror tunnel" (not "recursive feedback mode")
   - "Hyper-real detail" (not "edge gain > 1.5")

---

## Technical Implementation

### Mermaid Diagram Standards

**Format**: All diagrams use Mermaid with proper Quarto directives
```markdown
\```{mermaid}
%%| fig-width: 7
%%| fig-height: 10
%%| out-width: "85%"
%%| fig-align: center

graph TB
  ...
\```
```

**Layout**: Vertical (TB) not horizontal (LR) to prevent cutoff

**Colors**:
- Inputs: Light Blue (#e1f5ff)
- Outputs: Light Red (#ffe1e1)
- Processing: Light Green (#e1ffe1)
- Controls: Light Purple (#e1e1ff)
- Timing/Logic: Light Yellow (#fff5e1)

**Node Format**: `NAME["Label<br/>Description"]`

---

## Parsing Anti-Patterns Documentation

### Context

During ADC-013 (Process Body Parsing) implementation, we encountered critical parser bugs:
- Infinite loops from `manyTill` with consuming parsers
- Backtracking failures from missing `try` wrappers
- Expression parser hanging on simple identifiers

### Solution

Created comprehensive anti-pattern documentation (`policy/parsing-anti-patterns.md`) with:
- 4 major anti-patterns with detailed explanations
- 4 recommended patterns with examples
- Historical context from ADC-013 debugging
- Test case templates
- Policy enforcement guidelines

### Impact

Future parser development will avoid these pitfalls, saving hours of debugging time.

---

## File Structure

```
contrib/lzx/
├── README_DIAGRAMS.md           # Main documentation
│
├── lumarian/
│   ├── LUMARIAN_PATCH.md        # ✅ HIGH-LEVEL PATCH DIAGRAM
│   ├── enhance_diagram.md       # ✅ Component detail
│   ├── filter_diagram.md        # ✅ Algorithm detail
│   └── (other .vhd files)
│
└── mirrorbound/
    ├── MIRRORBOUND_PATCH.md     # ✅ HIGH-LEVEL PATCH DIAGRAM
    └── (other .vhd files)

policy/
└── parsing-anti-patterns.md     # ✅ Parser policy documentation
```

---

## Viewing the Diagrams

### Recommended Method: GitHub
1. Navigate to the files in GitHub
2. Diagrams render automatically in the preview

### Alternative: VS Code
1. Install "Markdown Preview Mermaid Support" extension
2. Open any diagram file
3. Use preview pane (Cmd+Shift+V)

### Export to PNG/SVG
```bash
# Install mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Render a diagram
mmdc -i contrib/lzx/lumarian/LUMARIAN_PATCH.md -o lumarian_patch.png

# Render all diagrams
find contrib/lzx -name "*PATCH.md" -o -name "*_diagram.md" | \
  xargs -I {} mmdc -i {} -o {}.png
```

---

## Next Steps

### Additional Diagrams (Optional)
- Component-level diagrams for:
  - Multiplier implementations
  - Rectifier modes
  - Gamma correction lookup
  - Contrast calculation
  - Mirror delay line internals

### Integration
- Add diagram links to main project README
- Include diagrams in Quarto documentation
- Export PNG versions for presentations

### Updates
- Keep diagrams in sync with VHDL changes
- Update when new controls/modes are added
- Revise based on user feedback

---

## Success Criteria

- [x] High-level patch diagrams for Lumarian and Mirrorbound
- [x] Follow "LZX P Series modules" design philosophy
- [x] Use functional block terminology (not implementation details)
- [x] Include control mappings and creative applications
- [x] Proper Mermaid/Quarto formatting
- [x] Comprehensive viewing instructions
- [x] Document parsing anti-patterns from ADC-013

**Status**: ✅ All criteria met

---

**Generated with**: Claude Code ADC Loop
**Contract**: N/A (diagram generation task)
**Related**: ADC-013 (Process Body Parsing)
