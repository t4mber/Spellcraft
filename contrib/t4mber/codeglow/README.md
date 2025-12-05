# Codeglow - FPGA Star Glow Effect Generator

**Version:** 1.0.0
**Author:** t4mber
**Date:** 2025-11-14
**License:** BSD-3-Clause

## Overview

Codeglow is a real-time VHDL implementation of a star-shaped glow effect inspired by Trapcode Starglow. It creates multicolored, animated glint effects on video streams, suitable for FPGA-based video synthesis and processing systems.

## Features

### Star Ray Generation
- **Configurable ray count:** 2-16 directional points
- **Adjustable streak length:** Control ray extension
- **Variable width/softness:** Sharp or diffuse rays
- **Polar coordinate synthesis:** Efficient radial pattern generation

### Color Gradient Mapping
- **4 gradient modes:**
  - **Warm:** Black → Orange → Yellow → White (realistic glow)
  - **Cool:** Black → Blue → Cyan → White (cold/icy)
  - **Rainbow:** Spectral colors (magical/dreamy)
  - **Custom:** User-defined 2-color gradient
- **Saturation control:** Adjust color intensity
- **Smooth interpolation:** Linear color blending

### Shimmer/Animation
- **4 modulation waveforms:**
  - **Sine:** Smooth, organic shimmer
  - **Triangle:** Linear ramp
  - **Pulse:** Binary on/off flicker
  - **Random:** Sparkle/twinkle effect (LFSR-based)
- **Adjustable rate:** Control animation speed
- **Variable depth:** Set modulation intensity

## Architecture

```
pixel_luma_in ──→ [Star Ray Generator] ──→ [Shimmer Modulator] ──→ [Color Gradient Mapper] ──→ RGB out
     +                    ↑                        ↑                          ↑
     └──── (x,y coords) ──┘                        │                          │
                                              shimmer params              color params
```

### Pipeline Stages

1. **Star Ray Generator** (4 stages)
   - Input registration
   - Polar coordinate approximation
   - Ray modulation (angle-based pattern)
   - Distance falloff

2. **Shimmer Modulator** (2 stages)
   - LFO waveform generation
   - Intensity modulation

3. **Color Gradient Mapper** (3 stages)
   - Gradient selection
   - Color calculation
   - Saturation adjustment

**Total Pipeline Latency:** ~9 clock cycles

## Module Descriptions

### `codeglow.vhd` (Top-Level)
Main entity integrating all submodules. Accepts video input with luminance and coordinates, outputs RGB with configurable glow effect.

**Generics:**
- `G_WIDTH`: Video bit width (default: 10-bit)
- `G_COORD_WIDTH`: Coordinate space (default: 12-bit = 4096×4096)

**Control Signals:**
- `num_rays`: Ray count (4 bits, 2-16)
- `ray_length`, `ray_width`: Geometric parameters
- `color_mode`: Gradient selection (2 bits)
- `shimmer_rate`, `shimmer_depth`, `shimmer_mode`: Animation controls

### `star_ray_generator.vhd`
Generates directional star patterns using polar coordinate approximation. Implements fast atan2-like angle detection and radial distance calculation.

**Algorithm:**
- Octant-based angle approximation (no trig)
- Max/min radius estimation
- Triangular wave ray pattern
- Distance-based intensity falloff

### `color_gradient_mapper.vhd`
Maps monochrome intensity to RGB colors using predefined or custom gradients. Supports saturation control for vibrant or desaturated looks.

**Gradient Implementations:**
- Warm: Piecewise linear (black→orange→white)
- Cool: Blue-dominant piecewise
- Rainbow: 3-region spectral cycle
- Custom: Linear interpolation between 2 color stops

### `shimmer_modulator.vhd`
Temporal animation using phase accumulator and waveform generators. Includes 16-bit LFSR for pseudo-random sparkle effects.

**Features:**
- Phase accumulator for smooth LFO
- Multi-waveform support (sine/tri/pulse/random)
- Bidirectional modulation (±depth)
- Overflow protection

## Usage Example

```vhdl
-- Instantiate codeglow
glow_fx : entity work.codeglow
  generic map (
    G_WIDTH       => 10,
    G_COORD_WIDTH => 12
  )
  port map (
    clk           => pixel_clk,     -- 74.25 MHz for 720p
    reset         => reset,
    enable        => video_enable,

    -- Input
    pixel_luma_in => luma_detect,   -- Bright pixels → glow source
    x_coord       => x_counter,
    y_coord       => y_counter,

    -- Star configuration: 8-point star, medium length
    num_rays      => "1000",        -- 8 rays
    ray_length    => "0011111111",  -- ~25% of range
    ray_width     => "0001111111",  -- ~12.5% softness

    -- Warm gradient, high saturation
    color_mode    => "00",          -- Warm
    saturation    => "1111111111",  -- Full color
    color_stop0_r => (others => '0'),  -- Unused for preset modes
    -- ... (other color stops)

    -- Slow shimmer with sine wave
    shimmer_rate  => "0000000100",  -- Slow
    shimmer_depth => "0011111111",  -- 25% modulation
    shimmer_mode  => "00",          -- Sine

    -- Output RGB
    red_out       => r_glow,
    green_out     => g_glow,
    blue_out      => b_glow,
    valid_out     => glow_valid
  );
```

## Resource Utilization (Estimated)

**Target:** iCE40 HX8K / Lattice ECP5

| Resource | Count | Notes |
|----------|-------|-------|
| LUTs | ~800-1200 | Depends on bit width |
| Flip-Flops | ~400-600 | Pipeline registers |
| Block RAM | 0 | No memory required |
| DSP Blocks | 0 | Optimized for LUT-based math |

**Clock Frequency:** 74.25 MHz+ (720p video rate)

## Performance

- **Throughput:** 1 pixel/clock (full video rate)
- **Latency:** 9 cycles (121 ns @ 74.25 MHz)
- **Combinatorial Depth:** Minimal (registered pipeline)

## Testing

Run Spellcraft analyzer to verify signal usage and detect violations:

```bash
cd contrib/t4mber/codeglow
stack exec spellcraft -- *.vhd
```

Expected: Clean parse with no signal usage violations.

## Design Patterns (Following LZX Style)

✅ **Generic bit widths** for flexibility
✅ **Pipeline stages** for timing closure
✅ **Valid signals** for data flow control
✅ **Saturating arithmetic** to prevent overflow
✅ **Functional approximations** (no transcendental functions)
✅ **No reset for data path** (only for state machines)
✅ **Descriptive signal names** with prefixes (s1_, s2_)

## Applications

- **Video synthesis:** Add magical glows to generated patterns
- **Post-processing:** Enhance bright areas in captured video
- **VJ/Live performance:** Real-time effect control via MIDI/OSC
- **Creative coding:** FPGA-based generative art

## Future Enhancements

- [ ] Blur/spread control (Gaussian approximation)
- [ ] Multiple glow sources (brightness threshold + peak detection)
- [ ] Rotation control (phase offset)
- [ ] Chromatic aberration simulation
- [ ] GPU-style bloom (multi-pass downsample/upsample)

## References

- **Trapcode Starglow:** https://www.maxon.net/trapcode/starglow
- **LZX Industries:** Video synthesis hardware design patterns
- **Spellcraft:** VHDL static analysis and verification

## License

BSD-3-Clause - See repository root for full license text.

---

**Crafted with ✨ spellcraft ✨**
