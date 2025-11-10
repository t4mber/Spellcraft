#!/usr/bin/env python3
"""
Chaos Monkey with Subtlety Levels
Contract: spellcraft-adc-011 (Enhanced)

Generates VHDL violations at different subtlety levels to test
detection capabilities progressively.

Subtlety Levels:
- Level 1 (Obvious): Syntax-valid but immediately detectable issues
- Level 2 (Moderate): Requires basic static analysis to detect
- Level 3 (Subtle): Requires cross-module or timing analysis
- Level 4 (Very Subtle): Only detectable with advanced analysis
- Level 5 (Extremely Subtle): Edge cases that require deep understanding

Usage:
    python scripts/chaos-monkey-subtlety.py \\
        --source contrib/lzx/lumarian/enhance.vhd \\
        --subtlety 1-5 \\
        --output contrib/lzx-chaos-levels
"""

import argparse
import json
import re
import sys
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple

class SubtleChaosMonkey:
    def __init__(self, source_path: str, output_dir: str, subtlety: int):
        self.source_path = Path(source_path)
        self.output_dir = Path(output_dir)
        self.subtlety = subtlety
        self.violations = []
        self.entity_name = None
        self.arch_name = None

    def read_source(self) -> str:
        with open(self.source_path, 'r') as f:
            return f.read()

    def analyze_structure(self, content: str) -> Dict:
        structure = {
            'entity_match': re.search(r'entity\s+(\w+)\s+is', content),
            'arch_match': re.search(r'architecture\s+(\w+)\s+of', content),
            'signals': re.findall(r'signal\s+(\w+)\s*:', content),
            'processes': len(re.findall(r'\bprocess\b', content)),
            'clock_signals': re.findall(r'(\w*clk\w*|clock)\s*:', content, re.IGNORECASE),
        }

        if structure['entity_match']:
            self.entity_name = structure['entity_match'].group(1)
        if structure['arch_match']:
            self.arch_name = structure['arch_match'].group(1)

        return structure

    # ===== SUBTLETY LEVEL 1: OBVIOUS =====

    def inject_level1_undriven(self, content: str) -> Tuple[str, Dict]:
        """Level 1: Completely undriven signal with obvious name"""
        lines = content.split('\n')

        for i, line in enumerate(lines):
            if 'begin' in line.lower() and 'architecture' not in line.lower():
                marker = [
                    "  -- CHAOS-MONKEY: level1-undriven",
                    "  -- SUBTLETY: 1 (Obvious)",
                    "  -- VIOLATION: Signal declared but completely undriven",
                    "  signal THIS_SIGNAL_IS_NEVER_USED : std_logic; -- OBVIOUS BUG!"
                ]
                lines.insert(i, '\n'.join(marker))

                return '\n'.join(lines), {
                    "id": "level1-undriven",
                    "subtlety": 1,
                    "category": "Signal Integrity",
                    "type": "Completely Undriven Signal",
                    "severity": "obvious",
                    "line": i + 1,
                    "description": "Signal with obvious name declared but never assigned",
                    "detection_difficulty": "trivial",
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "unused_signal_check",
                        "confidence": "very_high"
                    }
                }
        return content, None

    # ===== SUBTLETY LEVEL 2: MODERATE =====

    def inject_level2_partial_assignment(self, content: str) -> Tuple[str, Dict]:
        """Level 2: Signal assigned in one process but not all code paths"""
        lines = content.split('\n')

        # Find first process and add conditionally assigned signal
        for i, line in enumerate(lines):
            if 'begin' in line.lower() and i > 20:
                marker = [
                    "  -- CHAOS-MONKEY: level2-partial",
                    "  -- SUBTLETY: 2 (Moderate)",
                    "  -- VIOLATION: Signal assigned in some paths, not others",
                    "  signal s_sometimes_driven : std_logic;"
                ]
                lines.insert(i, '\n'.join(marker))

                # Find a process to add partial assignment
                for j in range(i+1, len(lines)):
                    if 'if rising_edge' in lines[j]:
                        # Add partial assignment (only in one branch)
                        lines.insert(j+1, "      if s_valid = '1' then")
                        lines.insert(j+2, "        s_sometimes_driven <= '1'; -- Only assigned here!")
                        lines.insert(j+3, "      end if;")
                        break

                return '\n'.join(lines), {
                    "id": "level2-partial",
                    "subtlety": 2,
                    "category": "Signal Integrity",
                    "type": "Incompletely Assigned Signal",
                    "severity": "moderate",
                    "line": i + 1,
                    "description": "Signal assigned in some code paths but not others (latch inference)",
                    "detection_difficulty": "moderate",
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "control_flow_analysis",
                        "confidence": "high"
                    }
                }
        return content, None

    # ===== SUBTLETY LEVEL 3: SUBTLE =====

    def inject_level3_bit_growth(self, content: str) -> Tuple[str, Dict]:
        """Level 3: Silent bit-width growth without overflow protection"""
        lines = content.split('\n')

        # Find a signal with WIDTH-1 downto 0
        for i, line in enumerate(lines):
            if 'signal' in line and 'downto 0' in line and 'WIDTH - 1' in line:
                # Add a signal that will grow without protection
                marker = [
                    "  -- CHAOS-MONKEY: level3-bitgrowth",
                    "  -- SUBTLETY: 3 (Subtle)",
                    "  -- VIOLATION: Arithmetic operation without overflow protection",
                    "  signal s_accumulator : unsigned(G_WIDTH - 1 downto 0); -- Will overflow!"
                ]
                lines.insert(i+1, '\n'.join(marker))

                # Find process and add accumulation without bounds check
                for j in range(i+2, len(lines)):
                    if 'if rising_edge' in lines[j]:
                        lines.insert(j+1, "      -- SUBTLE BUG: Accumulator can overflow silently")
                        lines.insert(j+2, "      s_accumulator <= s_accumulator + 1;")
                        break

                return '\n'.join(lines), {
                    "id": "level3-bitgrowth",
                    "subtlety": 3,
                    "category": "Functional Errors",
                    "type": "Silent Overflow",
                    "severity": "subtle",
                    "line": i + 2,
                    "description": "Counter/accumulator will silently overflow without saturation",
                    "detection_difficulty": "subtle",
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "arithmetic_bounds_checker",
                        "confidence": "medium"
                    }
                }
        return content, None

    # ===== SUBTLETY LEVEL 4: VERY SUBTLE =====

    def inject_level4_timing_assumption(self, content: str) -> Tuple[str, Dict]:
        """Level 4: Timing assumption that breaks under certain conditions"""
        lines = content.split('\n')

        for i, line in enumerate(lines):
            if 'signal' in line and 's_valid' in line:
                marker = [
                    "  -- CHAOS-MONKEY: level4-timing",
                    "  -- SUBTLETY: 4 (Very Subtle)",
                    "  -- VIOLATION: Timing assumption - valid flag not synchronized properly",
                    "  signal s_cross_domain_valid : std_logic; -- Crosses from different clock!"
                ]
                lines.insert(i+1, '\n'.join(marker))

                # Add usage that assumes same-clock timing
                for j in range(i+2, len(lines)):
                    if 'if rising_edge' in lines[j]:
                        lines.insert(j+1, "      -- VERY SUBTLE: This valid signal comes from different clock domain")
                        lines.insert(j+2, "      if s_cross_domain_valid = '1' then")
                        lines.insert(j+3, "        -- Process data, but timing not guaranteed!")
                        lines.insert(j+4, "      end if;")
                        break

                return '\n'.join(lines), {
                    "id": "level4-timing",
                    "subtlety": 4,
                    "category": "Clock Domain",
                    "type": "Hidden CDC Timing Assumption",
                    "severity": "very_subtle",
                    "line": i + 2,
                    "description": "Valid flag from different clock domain used without proper CDC",
                    "detection_difficulty": "very_subtle",
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "multi_clock_analyzer",
                        "confidence": "low"
                    }
                }
        return content, None

    # ===== SUBTLETY LEVEL 5: EXTREMELY SUBTLE =====

    def inject_level5_race_condition(self, content: str) -> Tuple[str, Dict]:
        """Level 5: Race condition that only manifests under specific timing"""
        lines = content.split('\n')

        for i, line in enumerate(lines):
            if 'begin' in line.lower() and i > 20:
                marker = [
                    "  -- CHAOS-MONKEY: level5-race",
                    "  -- SUBTLETY: 5 (Extremely Subtle)",
                    "  -- VIOLATION: Race condition in signal update order",
                    "  signal s_data_stage1 : unsigned(G_WIDTH - 1 downto 0);",
                    "  signal s_data_stage2 : unsigned(G_WIDTH - 1 downto 0);"
                ]
                lines.insert(i, '\n'.join(marker))

                # Add two processes that create race condition
                for j in range(i+6, len(lines)):
                    if 'process' in lines[j]:
                        # Insert problematic concurrent assignments
                        lines.insert(j+1, "    -- EXTREMELY SUBTLE: These assignments can race!")
                        lines.insert(j+2, "    if rising_edge(clk) then")
                        lines.insert(j+3, "      s_data_stage2 <= s_data_stage1; -- Read old or new value?")
                        lines.insert(j+4, "      s_data_stage1 <= a; -- Update might happen simultaneously")
                        lines.insert(j+5, "    end if;")
                        break

                return '\n'.join(lines), {
                    "id": "level5-race",
                    "subtlety": 5,
                    "category": "Timing",
                    "type": "Delta Cycle Race Condition",
                    "severity": "extremely_subtle",
                    "line": i + 1,
                    "description": "Signal update order creates race between stages",
                    "detection_difficulty": "extremely_subtle",
                    "expected_detection": {
                        "should_detect": False,
                        "detection_method": "delta_cycle_analyzer",
                        "confidence": "very_low",
                        "note": "Requires simulation-level analysis"
                    }
                }
        return content, None

    def generate_variant(self, injector_func, violation_id: str) -> bool:
        """Generate a single variant file"""
        content = self.read_source()
        corrupted_content, violation_info = injector_func(content)

        if not violation_info:
            print(f"  ⚠ Could not inject {violation_id}")
            return False

        # Update entity name
        if self.entity_name:
            new_entity = f"{self.entity_name}_chaos_{violation_id.replace('-', '_')}"
            corrupted_content = corrupted_content.replace(
                f"entity {self.entity_name}",
                f"entity {new_entity}"
            )
            corrupted_content = corrupted_content.replace(
                f"end {self.entity_name}",
                f"end {new_entity}"
            )
            corrupted_content = corrupted_content.replace(
                f" of {self.entity_name} ",
                f" of {new_entity} "
            )

        # Write file
        variant_filename = f"{self.source_path.stem}-{violation_id}.vhd"
        variant_path = self.output_dir / variant_filename

        with open(variant_path, 'w') as f:
            f.write(corrupted_content)

        violation_info['file'] = variant_filename
        self.violations.append(violation_info)

        print(f"  ✓ Level {violation_info['subtlety']}: {variant_filename}")
        return True

    def generate_manifest(self):
        manifest = {
            "source": str(self.source_path),
            "generated_at": datetime.now().isoformat(),
            "subtlety_range": f"1-{self.subtlety}",
            "total_violations": len(self.violations),
            "by_subtlety": {
                str(i): len([v for v in self.violations if v['subtlety'] == i])
                for i in range(1, self.subtlety + 1)
            },
            "violations": self.violations
        }

        manifest_path = self.output_dir / f'chaos-violations-level{self.subtlety}.json'
        with open(manifest_path, 'w') as f:
            json.dump(manifest, f, indent=2)

        print(f"\n✓ Generated manifest: {manifest_path}")

    def generate(self):
        print(f"\n📊 Analyzing source file...")
        content = self.read_source()
        structure = self.analyze_structure(content)
        print(f"   Entity: {self.entity_name}")

        self.output_dir.mkdir(parents=True, exist_ok=True)
        print(f"\n📁 Output: {self.output_dir}")

        print(f"\n🔨 Generating subtlety levels 1-{self.subtlety}...")

        if self.subtlety >= 1:
            print("\n  Subtlety 1: Obvious Violations")
            self.generate_variant(self.inject_level1_undriven, "level1-undriven")

        if self.subtlety >= 2:
            print("\n  Subtlety 2: Moderate Violations")
            self.generate_variant(self.inject_level2_partial_assignment, "level2-partial")

        if self.subtlety >= 3:
            print("\n  Subtlety 3: Subtle Violations")
            self.generate_variant(self.inject_level3_bit_growth, "level3-bitgrowth")

        if self.subtlety >= 4:
            print("\n  Subtlety 4: Very Subtle Violations")
            self.generate_variant(self.inject_level4_timing_assumption, "level4-timing")

        if self.subtlety >= 5:
            print("\n  Subtlety 5: Extremely Subtle Violations")
            self.generate_variant(self.inject_level5_race_condition, "level5-race")

        self.generate_manifest()

        print(f"\n✅ Generated {len(self.violations)} violation variants")

def main():
    parser = argparse.ArgumentParser(description='Generate violations at subtlety levels')
    parser.add_argument('--source', required=True, help='Source VHDL file')
    parser.add_argument('--subtlety', type=int, default=5, choices=range(1,6),
                       help='Maximum subtlety level (1-5)')
    parser.add_argument('--output', default='contrib/lzx-chaos-levels',
                       help='Output directory')

    args = parser.parse_args()

    print("""
╔═══════════════════════════════════════════════════════════════╗
║     CHAOS MONKEY - SUBTLETY LEVEL EVALUATION                  ║
║            Contract: spellcraft-adc-011                       ║
╚═══════════════════════════════════════════════════════════════╝
""")

    monkey = SubtleChaosMonkey(args.source, args.output, args.subtlety)
    monkey.generate()

    return 0

if __name__ == '__main__':
    sys.exit(main())
