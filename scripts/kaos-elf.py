#!/usr/bin/env python3
"""
Kaos Elf VHDL Violation Injector
Contract: spellcraft-adc-011

This script systematically injects known hardware violations into working VHDL
designs to create a comprehensive test suite for violation detection.

Usage:
    python scripts/kaos-elf.py --source contrib/lzx/lumarian/enhance.vhd

The script will:
1. Read the source VHDL file
2. Generate variants with single violations injected
3. Create a manifest documenting all violations
4. Output test corpus to contrib/lzx-kaos/

IMPLEMENTATION INSTRUCTIONS FOR CLAUDE OPUS 4:
==============================================

You are implementing a VHDL kaos elf generator. Your goal is to inject
realistic hardware violations into clean VHDL code for testing purposes.

STEP 1: ANALYZE SOURCE FILE
---------------------------
Read and understand the VHDL structure:
- Identify all signal declarations
- Find all process blocks and their sensitivity lists
- Locate clock signals (typically named clk, clock, or have _clk suffix)
- Map reset signals (rst, reset, or _rst suffix)
- Find arithmetic operations (*, +, -, /)
- Identify state machines and control logic

STEP 2: PLAN VIOLATIONS (3 per category minimum)
------------------------------------------------

Category 1: Clock Domain Violations (CATASTROPHIC)
Example injections:
  a) Remove synchronizer FFs between clock domains
  b) Add direct assignment between different clock processes
  c) Use signal from one clock domain in another without CDC

Category 2: Combinatorial Hazards (SUBTLE)
Example injections:
  a) Create long combinatorial chain (5+ logic levels)
  b) Add feedback loop without register
  c) Create glitch-prone multiplexer from multiple async sources

Category 3: Signal Integrity (SUBTLE)
Example injections:
  a) Declare signal but never assign it
  b) Assign signal in two different process blocks (multiple drivers)
  c) Remove signal from process sensitivity list

Category 4: Timing Violations (CATASTROPHIC)
Example injections:
  a) Add excessive logic between registers
  b) Create high fanout (signal driving 50+ destinations)
  c) Add async input without synchronization

Category 5: Reset Violations (CATASTROPHIC)
Example injections:
  a) Remove reset from register initialization
  b) Mix async and sync reset in same process
  c) Use different reset polarities (active high vs low)

Category 6: Resource Violations (IMPLEMENTATION)
Example injections:
  a) Force non-optimal RAM inference pattern
  b) Break DSP inference for multipliers
  c) Create inefficient shift register pattern

Category 7: Functional Errors (SUBTLE)
Example injections:
  a) Off-by-one in array indexing: (WIDTH downto 0) instead of (WIDTH-1 downto 0)
  b) Missing overflow protection in addition
  c) Signed/unsigned mix in arithmetic

STEP 3: GENERATE VARIANTS
-------------------------
For EACH violation:

1. Copy source file to new variant:
   enhance-cat{N}-violation{M}.vhd

2. Inject SINGLE violation at appropriate location

3. Add marker comment at injection point:
   -- KAOS-ELF: cat1-violation1
   -- VIOLATION: Unregistered CDC crossing
   -- SEVERITY: catastrophic
   -- ORIGINAL: signal s_sync : std_logic;
   -- INJECTED: Removed synchronizer flip-flops

4. Update entity name to include violation ID:
   entity enhance_kaos_cat1_v1 is

5. Preserve all other code structure

STEP 4: GENERATE MANIFEST
-------------------------
Create kaos-violations.json with entry for each violation:

{
  "source": "contrib/lzx/lumarian/enhance.vhd",
  "generated_at": "2025-11-09T23:30:00Z",
  "total_violations": 21,
  "violations": [
    {
      "id": "cat1-violation1",
      "category": "Clock Domain Violations",
      "type": "Unregistered CDC",
      "severity": "catastrophic",
      "file": "enhance-cat1-violation1.vhd",
      "line": 67,
      "description": "Direct signal assignment between clk_a and clk_b domains",
      "injection": {
        "original_lines": [
          "-- Two-stage synchronizer",
          "signal sync_stage1 : std_logic;",
          "signal sync_stage2 : std_logic;"
        ],
        "corrupted_lines": [
          "-- VIOLATION: Synchronizer removed",
          "signal sync_direct : std_logic;"
        ],
        "method": "Removed flip-flop stages from CDC synchronizer"
      },
      "expected_detection": {
        "should_detect": true,
        "detection_method": "clock_domain_analyzer",
        "confidence": "high",
        "currently_implemented": false
      }
    }
  ]
}

STEP 5: OUTPUT
--------------
Write to contrib/lzx-kaos/:
- All variant .vhd files (21+ files)
- kaos-violations.json (manifest)
- README.md (generation summary)

README should include:
- Source file information
- Number of violations per category
- Instructions for running tests
- Expected vs actual detection rates

CRITICAL REQUIREMENTS:
---------------------
1. All generated VHDL must be syntactically valid
2. Each file has exactly ONE violation
3. Violations are realistic (not artificial)
4. Inline markers are clear and complete
5. Manifest is valid JSON with all required fields
6. Entity names are unique per variant

REALISTIC VIOLATION EXAMPLES:
----------------------------

Good (realistic):
  - Removing one FF from a 2-FF synchronizer
  - Adding direct wire between clock domains
  - Forgetting to reset a state machine register
  - Off-by-one in loop bound (10 downto 1 instead of 10 downto 0)

Bad (artificial):
  - Inserting syntax errors
  - Random signal name changes
  - Breaking type systems
  - Adding nonsense code

OUTPUT FORMAT:
-------------
Print progress messages:
  "Analyzing source file..."
  "Generating Category 1 violations..."
  "Created enhance-cat1-violation1.vhd"
  "..."
  "Writing manifest..."
  "Generation complete: 21 violations in 21 files"

"""

import argparse
import json
import sys
from pathlib import Path
from datetime import datetime

def main():
    parser = argparse.ArgumentParser(
        description='Generate VHDL kaos elf test variants',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument(
        '--source',
        required=True,
        help='Source VHDL file to inject violations into'
    )
    parser.add_argument(
        '--output',
        default='contrib/lzx-chaos',
        help='Output directory for chaos variants (default: contrib/lzx-chaos)'
    )

    args = parser.parse_args()

    print("""
╔═══════════════════════════════════════════════════════════════╗
║          CHAOS MONKEY VHDL VIOLATION INJECTOR                 ║
║                 Contract: spellcraft-adc-011                  ║
╚═══════════════════════════════════════════════════════════════╝

This is a template script. To generate violations, you need to:

1. Use Claude Opus 4 to implement the full generator
2. Follow the instructions in the docstring above
3. Or implement the generator manually following the contract

The generator must:
- Analyze VHDL structure intelligently
- Inject realistic hardware violations
- Generate comprehensive manifest
- Create one variant file per violation

For now, this script serves as documentation and specification.

To proceed:
  1. Copy this script
  2. Implement the violation injection logic
  3. Use the instructions above as your guide

Contract: spellcraft-adc-011
Source: {}
Output: {}
""".format(args.source, args.output))

    print("\nNOTE: This is a specification template.")
    print("Implementation requires Claude Opus 4 or manual coding.")
    print("See contract adc-011-kaos-elf.qmd for full requirements.\n")

    return 0

if __name__ == '__main__':
    sys.exit(main())
