# Codeglow Design Notes

## Memory Requirements Analysis

### Current Implementation: Zero-Buffer Design ✅

The current codeglow implementation requires **NO frame or line buffering**:

- **Architecture:** Single-pixel streaming pipeline
- **Memory:** Only ~20-30 pipeline registers per pixel
- **Latency:** ~9 clock cycles (constant)
- **Throughput:** 1 pixel/clock
- **Compatible with:** Streaming video hardware (no SDRAM/BRAM)

### How It Works

Each pixel is processed independently based on:
1. **Coordinates (x, y):** Position in frame
2. **Luminance:** Brightness value
3. **Control parameters:** Ray count, length, width, color, shimmer

**Processing per pixel:**
```
Input (x, y, luma) → [Geometric calculation] → Output (R, G, B)
  ↓
  Calculate angle: θ = approx_atan2(y, x)
  Calculate radius: r ≈ max(|x|, |y|) + min(|x|, |y|)/2
  Modulate by angle: intensity = f(θ mod ray_spacing)
  Attenuate by distance: intensity *= falloff(r, ray_length)
  Map to color gradient
```

## Current Behavior

### What It Does
Creates a **geometric ray pattern** across the entire frame:
- Rays emanate from a fixed center point (typically frame center)
- Pattern is modulated by input luminance
- Creates angular gradients and star shapes
- Shimmer adds temporal animation

### What It Doesn't Do
- Does NOT detect bright pixels as glow sources
- Does NOT spread rays from multiple bright sources
- Does NOT create true "glows" that accumulate

**Think of it as:** A kaleidoscopic overlay that responds to video content, not a true glow effect.

## Use Cases for Zero-Buffer Design

✅ **Geometric pattern generation**
- Angular/radial gradients
- Kaleidoscopic effects
- Star field overlays
- Animated textures

✅ **Video synthesis**
- LZX-style modular video synthesis
- Real-time VJ performance
- Feedback loops with video mixers

✅ **Luminance-reactive patterns**
- Brightness-modulated overlays
- Color grading with geometric masks
- Dynamic vignetting

## Alternative Designs (Require Memory)

### Option A: True Trapcode Starglow (High Memory)

**Requirements:**
- Full frame buffer (1920×1080×10bit = 2.5 MB)
- Multi-pass rendering
- Bright pixel detection + accumulation

**Algorithm:**
1. Pass 1: Detect bright pixels (threshold)
2. Pass 2: For each bright pixel, render rays
3. Pass 3: Blur/spread rays (optional)
4. Pass 4: Composite with original

**Memory:** ~2.5-5 MB SDRAM

### Option B: Temporal Accumulation (Low Memory)

**Requirements:**
- Small FIFO for brightest N pixels (~16-32)
- Single line buffer for temporal decay
- Frame counter for persistence

**Algorithm:**
```vhdl
-- Frame N: Detect brightest pixels
if luma > threshold then
  store (x, y, luma) in FIFO  -- Keep top 16
end if

-- Frame N+1: Render rays from stored positions
for each stored pixel:
  if (current_x, current_y) within ray distance:
    add ray contribution to output
  end if
end for

-- Decay over time
output = output * decay_factor
```

**Memory:**
- FIFO: 16 × (12bit x + 12bit y + 10bit luma) = 544 bits
- Line buffer (optional): 1920 × 10bit = 19.2 kbit
- **Total:** ~20 kbit (feasible for BRAM)

### Option C: Single-Line Glow (Minimal Memory)

**Requirements:**
- One line buffer only
- Process glows that fit within scanline

**Algorithm:**
```vhdl
-- As each line is scanned:
1. Store line in buffer
2. Detect bright pixels in current + previous line
3. Render vertical rays only (within 2-line window)
4. Output with limited vertical spread
```

**Memory:** 1 line = ~20 kbit
**Limitation:** Only vertical rays, limited spread

## Recommended Implementations by Hardware

### iCE40 HX1K/4K (Minimal BRAM)
- **Use:** Current zero-buffer design
- **Reason:** Limited BRAM (4-16 kbit)
- **Effect:** Geometric overlay, not true glow

### iCE40 HX8K / ECP5-25 (Moderate BRAM)
- **Use:** Option B (Temporal accumulation)
- **Reason:** Sufficient BRAM (32-64 kbit)
- **Effect:** Glow from brightest pixels

### ECP5-45/85 + SDRAM (High Memory)
- **Use:** Option A (Full Trapcode Starglow)
- **Reason:** External SDRAM available
- **Effect:** True multi-source glow with blur

## Hardware Target Profiles

### Profile: No Memory Available
**Current codeglow works as-is!**
- Streaming pipeline only
- Geometric pattern generator
- No bright pixel detection

### Profile: Small BRAM (16-64 kbit)
**Implement Option B:**
- Add bright pixel detector with FIFO
- Store top 16-32 brightest pixels per frame
- Render rays from stored positions
- Optional: 1-line decay buffer

**Modifications needed:**
1. Add `bright_pixel_detector.vhd`
2. Add small FIFO (16 deep × 34 bits)
3. Add ray rendering from stored positions
4. Modify top-level to include detection pass

### Profile: SDRAM Available
**Implement Option A:**
- Full frame buffering
- Multi-pass rendering
- Gaussian blur for spread
- True accumulation

**Requires significant redesign.**

## Implementation Roadmap

### Phase 1: Current Design (✅ Complete)
- Zero-buffer streaming pipeline
- Geometric ray generation
- Color gradient mapping
- Shimmer/animation

### Phase 2: Bright Pixel Detection (Future)
```vhdl
-- New module: bright_pixel_detector.vhd
entity bright_pixel_detector is
  port (
    -- Input
    pixel_in : in unsigned(G_WIDTH-1 downto 0);
    x_coord  : in unsigned(G_COORD_WIDTH-1 downto 0);
    y_coord  : in unsigned(G_COORD_WIDTH-1 downto 0);
    threshold: in unsigned(G_WIDTH-1 downto 0);

    -- FIFO interface (brightest N pixels)
    fifo_wr_en   : out std_logic;
    fifo_wr_data : out std_logic_vector(33 downto 0); -- x,y,luma
    fifo_full    : in  std_logic
  );
end entity;
```

### Phase 3: Multi-Source Ray Rendering (Future)
```vhdl
-- Modify star_ray_generator to accept source position
entity star_ray_generator is
  port (
    -- Add source position (where rays originate)
    source_x : in unsigned(G_COORD_WIDTH-1 downto 0);
    source_y : in unsigned(G_COORD_WIDTH-1 downto 0);

    -- Current pixel position
    pixel_x  : in unsigned(G_COORD_WIDTH-1 downto 0);
    pixel_y  : in unsigned(G_COORD_WIDTH-1 downto 0);

    -- ... rest of interface
  );
end entity;
```

### Phase 4: Temporal Accumulation (Future)
- Add line buffer for decay
- Implement persistence control
- Add frame counter

## Performance Estimates

### Current Design (Zero-Buffer)
- **LUTs:** ~800-1200
- **FFs:** ~400-600
- **BRAM:** 0 blocks
- **Clock:** 74.25 MHz+ (720p/1080p capable)

### With Option B (Temporal Accumulation)
- **LUTs:** ~1200-1800 (+50%)
- **FFs:** ~600-900 (+50%)
- **BRAM:** 1-2 blocks (16-32 kbit)
- **Clock:** 74.25 MHz+ (still video-rate)

### With Option A (Full Buffer)
- **LUTs:** ~2000-3000
- **FFs:** ~1000-1500
- **BRAM:** 32+ blocks (or external SDRAM)
- **Clock:** 74.25 MHz minimum
- **SDRAM controller:** Required

## Questions for Hardware Selection

1. **What FPGA are you targeting?**
   - iCE40 HX (which size?)
   - Lattice ECP5 (which density?)
   - Other?

2. **Available memory:**
   - BRAM blocks available?
   - External SDRAM?
   - SRAM?

3. **Video format:**
   - Resolution? (720p, 1080p, SD?)
   - Frame rate? (60Hz, 30Hz?)
   - Color depth? (10-bit, 8-bit?)

4. **Desired effect:**
   - Geometric overlay (current design works!)
   - Glow from bright pixels (needs detection + FIFO)
   - True Trapcode-style (needs frame buffer)

## Design Philosophy: LZX-Inspired

Following the LZX examples, codeglow prioritizes:
- **Streaming architecture** (no buffering unless necessary)
- **Modular processing** (pipeline stages)
- **Real-time control** (no frame latency)
- **Analog-style thinking** (per-pixel processing)

This makes it compatible with **modular video synthesis** workflows where:
- Effects process in real-time
- No frame delays
- Control parameters respond immediately
- Can be chained/mixed with other modules

## Conclusion

**Current codeglow is production-ready for:**
- ✅ Hardware with NO frame/line buffers
- ✅ Streaming video pipelines
- ✅ Real-time geometric effects
- ✅ LXZ-style modular video synthesis

**For true "glow from bright pixels" effect:**
- Requires at least Option B (small FIFO + optional line buffer)
- Needs ~20-40 kbit BRAM
- Feasible for mid-range FPGAs (iCE40 HX8K, ECP5-25+)

---

**Last Updated:** 2025-11-14
**Status:** Zero-buffer design complete and tested
**Next:** Awaiting hardware target specification for memory-based enhancements
