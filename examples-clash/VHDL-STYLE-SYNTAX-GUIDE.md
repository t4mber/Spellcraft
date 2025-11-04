# Writing VHDL-Style Code in Clash

This guide demonstrates various techniques for making Clash code read more like VHDL, making it easier for hardware engineers to adopt type-level constraint checking.

## Approaches Ranked by VHDL Similarity

### Level 1: Standard Clash (Least VHDL-like)

**Files:** `01_pll_violation.hs`, `02_multiple_pll_cascading.hs`, `03_valid_design.hs`

**Style:** Pure functional Haskell with type applications

```haskell
type PixelClockFreq = 50
type HighClockFreq = 200

pixelDomain :: ClockDomain PixelClockFreq
pixelDomain = mkClockDomain @PixelClockFreq "pixel_clk"

pll :: PLL PixelClockFreq PLLMultiplier
pll = mkPLL @PixelClockFreq @PLLMultiplier @HighClockFreq "PLL_1"

highClock :: HWSignal HighClockFreq ()
highClock = connectPLL pll pixelClock
```

**Pros:**
- ✅ Type-safe with compile-time checking
- ✅ Clean functional style
- ✅ Full type inference

**Cons:**
- ❌ Doesn't look like VHDL
- ❌ Requires Haskell knowledge
- ❌ Not immediately familiar to hardware engineers

### Level 2: Record Syntax with Where Clauses (More VHDL-like)

**Files:** `04_vhdl_style_syntax.hs`

**Style:** Use record syntax and `where` clauses to mimic VHDL's structural style

```haskell
videoProcessor :: EntityPorts 50 -> EntityPorts 200
videoProcessor ports =
  EntityPorts { pixel_clk = high_clk_signal, video_out = video_output }
  where
    -- Signal declarations (like VHDL signals)
    high_clk_signal = pll_1.clk_out
    video_output = encoder_inst.video_out

    -- Component instantiation: PLL_1
    pll_1 = ComponentPLL
      { component_name = "PLL_1"
      , clk_in = ports.pixel_clk
      , clk_out = connectPLL pll_spec ports.pixel_clk
      }
      where
        pll_spec = mkPLL @50 @4 @200 "PLL_1"
```

**Compare to VHDL:**

```vhdl
architecture structural of Video_Processor is
  signal high_clk : std_logic;
begin
  pll_1 : PLL_1
    port map (
      clk_in  => pixel_clk,
      clk_out => high_clk
    );
end architecture;
```

**Pros:**
- ✅ Hierarchical structure like VHDL
- ✅ Signal and component separation
- ✅ Still fully type-checked

**Cons:**
- ❌ Record syntax adds boilerplate
- ❌ Still requires `where` clause understanding

### Level 3: DSL with Builder Pattern (Most VHDL-like)

**Files:** `05_dsl_style.hs`

**Style:** Domain-specific language that mirrors VHDL syntax

```haskell
architecture_structural = Entity
  { entityName = "Video_Processor"
  , ports = [port_pixel_clk, port_video_out]
  , signals = [signal_high_clk]
  , components = [component_pll_inst, component_encoder_inst]
  }
  where
    signal_high_clk = Signal "high_clk" "std_logic"

    component_pll_inst = Component
      { componentType = "PLL_1"
      , instanceName = "pll_inst"
      , genericMap =
          [ "CLK_IN_FREQ" :=> 50.0
          , "CLK_OUT_FREQ" :=> 200.0
          ]
      , portMap =
          [ ("clk_in", "pixel_clk")
          , ("clk_out", "high_clk")
          ]
      }
```

**Compare to VHDL:**

```vhdl
architecture structural of Video_Processor is
  signal high_clk : std_logic;
begin
  pll_inst : PLL_1
    generic map (
      CLK_IN_FREQ  => 50.0,
      CLK_OUT_FREQ => 200.0
    )
    port map (
      clk_in  => pixel_clk,
      clk_out => high_clk
    );
end architecture;
```

**Pros:**
- ✅ Nearly identical to VHDL structure
- ✅ Generic map and port map explicit
- ✅ Easy for VHDL engineers to read

**Cons:**
- ❌ More runtime data structures
- ❌ Requires execution layer to validate types
- ❌ Can't catch all errors at compile time

### Level 4: Quasi-Quoters (Future: Actual VHDL Syntax)

**Status:** Not yet implemented (would require Template Haskell)

**Style:** Write actual VHDL syntax that gets compiled to Clash

```haskell
[vhdl|
  architecture structural of Video_Processor is
    signal high_clk : std_logic;
  begin
    pll_inst : PLL_1
      generic map (CLK_IN_FREQ => 50.0, CLK_OUT_FREQ => 200.0)
      port map (clk_in => pixel_clk, clk_out => high_clk);

    encoder_inst : YPbPr_Encoder_A
      port map (pixel_clk => high_clk, video_out => video_out);
  end architecture;
|]
```

**How it would work:**
1. Parse VHDL syntax at compile time (Template Haskell)
2. Extract frequencies from generic maps
3. Generate type-level Clash code with constraints
4. GHC type-checks the generated code

**Pros:**
- ✅ Write actual VHDL
- ✅ Still get compile-time type checking
- ✅ Zero learning curve for VHDL engineers

**Cons:**
- ❌ Complex implementation
- ❌ Requires parser and code generator
- ❌ Limited to structural VHDL subset

## Recommended Approach by Use Case

### For Prototyping: Level 1 (Standard Clash)

```haskell
-- Quick and clean
let pll = mkPLL @50 @4 @200 "PLL_1"
let output = connectPLL pll input
```

**Use when:**
- Exploring designs interactively
- Writing quick tests
- You're comfortable with Haskell

### For Production Hardware: Level 2 (Record Syntax)

```haskell
videoProcessor ports =
  EntityPorts { pixel_clk = high_clk, video_out = video }
  where
    high_clk = connectPLL pll (ports.pixel_clk)
    pll = mkPLL @50 @4 @200 "PLL_1"
```

**Use when:**
- Building real hardware designs
- Need clear structural hierarchy
- Want balance of readability and type safety

### For Team Adoption: Level 3 (DSL)

```haskell
architecture_structural = Entity
  { entityName = "Video_Processor"
  , components =
      [ Component "PLL_1" "pll_inst"
          [ "CLK_IN_FREQ" :=> 50.0 ]
          [ ("clk_in", "pixel_clk") ]
      ]
  }
```

**Use when:**
- Onboarding VHDL engineers
- Need management buy-in
- Want maximum readability

## Key Haskell Features for VHDL-like Syntax

### 1. RecordWildCards

```haskell
{-# LANGUAGE RecordWildCards #-}

data PortMap = PortMap { clk_in :: Signal, clk_out :: Signal }

example PortMap{..} =
  -- clk_in and clk_out are automatically in scope
  connectPorts clk_in clk_out
```

**VHDL equivalent:** Positional port mapping vs named port mapping

### 2. NamedFieldPuns

```haskell
{-# LANGUAGE NamedFieldPuns #-}

example ports =
  let PortMap { clk_in, clk_out } = ports
  in connectPorts clk_in clk_out
```

**VHDL equivalent:** Port map with named associations

### 3. OverloadedRecordDot (GHC 9.2+)

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

example ports =
  connectPorts ports.clk_in ports.clk_out
```

**VHDL equivalent:** Signal selection with dot notation

### 4. DuplicateRecordFields

```haskell
{-# LANGUAGE DuplicateRecordFields #-}

data PLL = PLL { name :: String }
data Encoder = Encoder { name :: String }  -- OK: duplicate field name
```

**VHDL equivalent:** Different components can have ports with the same name

### 5. DataKinds + TypeApplications

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- Type-level natural for frequency
mkPLL @50 @4 @200 "PLL_1"
```

**VHDL equivalent:** Generic parameters with compile-time values

## Side-by-Side Comparison

### VHDL

```vhdl
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Video_Processor is
  port (
    pixel_clk : in  std_logic;
    video_out : out std_logic_vector(23 downto 0)
  );
end entity;

architecture structural of Video_Processor is
  signal high_clk : std_logic;
begin
  pll_inst : PLL_1
    generic map (
      CLK_IN_FREQ  => 50.0,
      CLK_OUT_FREQ => 200.0,
      MULTIPLY_BY  => 4
    )
    port map (
      clk_in  => pixel_clk,
      clk_out => high_clk
    );

  encoder_inst : YPbPr_Encoder_A
    generic map (BIT_DEPTH => 8)
    port map (
      pixel_clk => high_clk,
      video_out => video_out
    );
end architecture;
```

### Clash (Level 1: Standard)

```haskell
type PixelClockFreq = 50
type HighClockFreq = 200

videoProcessor :: HWSignal 50 () -> HWSignal 200 ()
videoProcessor pixel_clk =
  let high_clk = connectPLL pll pixel_clk
      pll = mkPLL @50 @4 @200 "pll_inst"
      encoder = mkEncoder @165 "encoder_inst" 8
  in case connectEncoder encoder high_clk of
       Right sig -> sig
       Left _ -> high_clk
```

### Clash (Level 2: Record Syntax)

```haskell
data EntityPorts freq = EntityPorts
  { pixel_clk :: HWSignal freq ()
  , video_out :: HWSignal freq ()
  }

videoProcessor :: EntityPorts 50 -> EntityPorts 200
videoProcessor input =
  EntityPorts
    { pixel_clk = high_clk
    , video_out = encoder_output
    }
  where
    high_clk = connectPLL pll_inst (input.pixel_clk)
    pll_inst = mkPLL @50 @4 @200 "pll_inst"

    encoder_output = case connectEncoder encoder_inst high_clk of
      Right sig -> sig
      Left _ -> high_clk
    encoder_inst = mkEncoder @165 "encoder_inst" 8
```

### Clash (Level 3: DSL)

```haskell
videoProcessor = Entity
  { entityName = "Video_Processor"
  , ports =
      [ Port "pixel_clk" In "std_logic"
      , Port "video_out" Out "std_logic_vector(23 downto 0)"
      ]
  , signals =
      [ Signal "high_clk" "std_logic"
      ]
  , components =
      [ Component "PLL_1" "pll_inst"
          [ "CLK_IN_FREQ" :=> 50.0
          , "CLK_OUT_FREQ" :=> 200.0
          , "MULTIPLY_BY" :=> 4.0
          ]
          [ ("clk_in", "pixel_clk")
          , ("clk_out", "high_clk")
          ]
      , Component "YPbPr_Encoder_A" "encoder_inst"
          [ "BIT_DEPTH" :=> 8.0 ]
          [ ("pixel_clk", "high_clk")
          , ("video_out", "video_out")
          ]
      ]
  }
```

## Type Safety Comparison

| Aspect | VHDL | Clash (Standard) | Clash (DSL) |
|--------|------|------------------|-------------|
| Frequency checking | Synthesis tool | Compile-time | Runtime + Compile |
| Generic maps | Runtime | Type-level | Runtime |
| Port connections | Elaboration | Type-checked | String-based |
| Error detection | Late (synthesis) | Early (compile) | Mixed |

## Recommendations

### Starting Out (Week 1-2)

Use **Level 1 (Standard Clash)** to learn the concepts:
- Focus on understanding type-level programming
- Get comfortable with `@` syntax for type applications
- Learn how `CheckMaxFreq` constraint works

### Building Real Designs (Month 1-3)

Move to **Level 2 (Record Syntax)** for production:
- Organize code like VHDL architectures
- Use `where` clauses for signal declarations
- Group related components in records

### Team Adoption (Month 3+)

Consider **Level 3 (DSL)** for wider adoption:
- Create library of VHDL-like builders
- Document DSL usage for team
- Build tooling to convert between representations

## Example Migration Path

### Step 1: Start with VHDL

```vhdl
pll_inst : PLL_1
  generic map (CLK_IN_FREQ => 50.0)
  port map (clk_in => pixel_clk);
```

### Step 2: Direct Translation (Level 1)

```haskell
let pll = mkPLL @50 @4 @200 "pll_inst"
let output = connectPLL pll pixel_clk
```

### Step 3: Add Structure (Level 2)

```haskell
videoProcessor input = output
  where
    pll_inst = mkPLL @50 @4 @200 "pll_inst"
    output = connectPLL pll_inst (input.pixel_clk)
```

### Step 4: Full DSL (Level 3)

```haskell
videoProcessor = Entity { ... }
  where
    pll_inst = Component "PLL_1" "pll_inst"
      [ "CLK_IN_FREQ" :=> 50.0 ]
      [ ("clk_in", "pixel_clk") ]
```

## Future Work: VHDL Parser

The ultimate goal would be a VHDL parser that generates type-checked Clash:

```bash
$ vhdl-to-clash video_processor.vhd > VideoProcessor.hs
$ cabal build  # Type errors if frequencies violate constraints!
```

This would allow:
1. Write actual VHDL (familiar syntax)
2. Get compile-time checking (impossible with pure VHDL)
3. Generate optimized HDL (through Clash backend)
4. Bridge both worlds

See `ARCHITECTURE-TYPE-LEVEL-VS-RUNTIME.md` for more on this vision.

---

**Files:**
- `04_vhdl_style_syntax.hs` - Record syntax examples
- `05_dsl_style.hs` - DSL builder examples
- `01_pll_violation.hs` - Standard Clash examples

**Status:** Educational examples - pick the style that fits your team!
