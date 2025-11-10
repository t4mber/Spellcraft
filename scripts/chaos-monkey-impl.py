#!/usr/bin/env python3
"""
Chaos Monkey VHDL Violation Injector - Initial Implementation
Contract: spellcraft-adc-011

This is a working implementation that generates a subset of violations
as a proof of concept. Demonstrates the pattern for full implementation.
"""

import argparse
import json
import re
import sys
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple

class VHDLChaosMonkey:
    def __init__(self, source_path: str, output_dir: str):
        self.source_path = Path(source_path)
        self.output_dir = Path(output_dir)
        self.violations = []
        self.entity_name = None
        self.arch_name = None

    def read_source(self) -> str:
        """Read the source VHDL file"""
        with open(self.source_path, 'r') as f:
            return f.read()

    def analyze_structure(self, content: str) -> Dict:
        """Analyze VHDL structure to find injection points"""
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

    def inject_cat1_violation1(self, content: str) -> Tuple[str, Dict]:
        """Category 1: Unregistered CDC (remove synchronizer)"""
        lines = content.split('\n')

        # Find a signal declaration to corrupt
        for i, line in enumerate(lines):
            if 'signal' in line and 's_' in line and i > 10:
                original_line = line
                # Add violation marker
                violation_marker = [
                    "  -- CHAOS-MONKEY: cat1-violation1",
                    "  -- VIOLATION: Unregistered CDC crossing",
                    "  -- SEVERITY: catastrophic",
                    f"  -- ORIGINAL: {line.strip()}",
                    "  -- INJECTED: Signal crosses clock domains without synchronization",
                    line + " -- WARNING: This signal now crosses clock domains unsafely!"
                ]

                lines[i] = '\n'.join(violation_marker)

                violation_info = {
                    "id": "cat1-violation1",
                    "category": "Clock Domain Violations",
                    "type": "Unregistered CDC",
                    "severity": "catastrophic",
                    "line": i + 1,
                    "description": "Signal crosses clock domains without proper synchronization",
                    "injection": {
                        "original_lines": [original_line.strip()],
                        "corrupted_lines": violation_marker,
                        "method": "Added cross-clock-domain usage comment"
                    },
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "clock_domain_analyzer",
                        "confidence": "high",
                        "currently_implemented": False
                    }
                }

                return '\n'.join(lines), violation_info

        return content, None

    def inject_cat3_violation1(self, content: str) -> Tuple[str, Dict]:
        """Category 3: Undriven signal"""
        lines = content.split('\n')

        # Find the signal declaration section
        for i, line in enumerate(lines):
            if 'begin' in line.lower() and 'architecture' not in line.lower():
                # Insert an undriven signal just before 'begin'
                violation_marker = [
                    "  -- CHAOS-MONKEY: cat3-violation1",
                    "  -- VIOLATION: Undriven signal",
                    "  -- SEVERITY: subtle",
                    "  -- INJECTED: Signal declared but never assigned",
                    "  signal s_chaos_undriven : std_logic; -- This signal is never driven!"
                ]

                lines.insert(i, '\n'.join(violation_marker))

                violation_info = {
                    "id": "cat3-violation1",
                    "category": "Signal Integrity Issues",
                    "type": "Undriven Signal",
                    "severity": "subtle",
                    "line": i + 1,
                    "description": "Signal declared in architecture but never assigned a value",
                    "injection": {
                        "original_lines": [],
                        "corrupted_lines": violation_marker,
                        "method": "Added signal declaration without assignment"
                    },
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "signal_usage_analyzer",
                        "confidence": "high",
                        "currently_implemented": False
                    }
                }

                return '\n'.join(lines), violation_info

        return content, None

    def inject_cat7_violation1(self, content: str) -> Tuple[str, Dict]:
        """Category 7: Off-by-one error in array indexing"""
        lines = content.split('\n')

        # Find a downto declaration
        for i, line in enumerate(lines):
            match = re.search(r'(\w+)\s*-\s*1\s+downto\s+0', line)
            if match:
                original_line = line
                # Change "WIDTH-1 downto 0" to "WIDTH downto 0"
                corrupted_line = re.sub(
                    r'(\w+)\s*-\s*1\s+downto\s+0',
                    r'\1 downto 0',
                    line
                )

                violation_marker = [
                    "  -- CHAOS-MONKEY: cat7-violation1",
                    "  -- VIOLATION: Off-by-one error",
                    "  -- SEVERITY: subtle",
                    f"  -- ORIGINAL: {original_line.strip()}",
                    "  -- INJECTED: Changed WIDTH-1 downto 0 to WIDTH downto 0 (wrong!)",
                    corrupted_line + " -- BUG: Off-by-one! Should be WIDTH-1"
                ]

                lines[i] = '\n'.join(violation_marker)

                violation_info = {
                    "id": "cat7-violation1",
                    "category": "Functional Errors",
                    "type": "Off-by-One Error",
                    "severity": "subtle",
                    "line": i + 1,
                    "description": "Array indexing uses full width instead of width-1",
                    "injection": {
                        "original_lines": [original_line.strip()],
                        "corrupted_lines": violation_marker,
                        "method": "Changed array bounds from WIDTH-1 to WIDTH"
                    },
                    "expected_detection": {
                        "should_detect": True,
                        "detection_method": "type_checker",
                        "confidence": "medium",
                        "currently_implemented": False
                    }
                }

                return '\n'.join(lines), violation_info

        return content, None

    def generate_variant(self, injector_func, violation_id: str) -> bool:
        """Generate a single variant file with one violation"""
        content = self.read_source()
        corrupted_content, violation_info = injector_func(content)

        if not violation_info:
            print(f"  ⚠ Could not inject {violation_id} - no suitable injection point")
            return False

        # Update entity name to include violation ID
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

        # Write variant file
        variant_filename = f"{self.source_path.stem}-{violation_id}.vhd"
        variant_path = self.output_dir / variant_filename

        with open(variant_path, 'w') as f:
            f.write(corrupted_content)

        # Update violation info with filename
        violation_info['file'] = variant_filename
        self.violations.append(violation_info)

        print(f"  ✓ Created {variant_filename}")
        return True

    def generate_manifest(self) -> None:
        """Generate the chaos-violations.json manifest"""
        manifest = {
            "source": str(self.source_path),
            "generated_at": datetime.utcnow().isoformat() + 'Z',
            "total_violations": len(self.violations),
            "categories_tested": len(set(v['category'] for v in self.violations)),
            "violations": self.violations
        }

        manifest_path = self.output_dir / 'chaos-violations.json'
        with open(manifest_path, 'w') as f:
            json.dump(manifest, f, indent=2)

        print(f"\n✓ Generated manifest: {manifest_path}")

    def generate_readme(self) -> None:
        """Generate README.md for the chaos corpus"""
        readme_content = f"""# Chaos Monkey VHDL Test Corpus

**Contract**: spellcraft-adc-011
**Generated**: {datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S')} UTC
**Source**: {self.source_path}

## Overview

This directory contains VHDL variants with systematically injected hardware violations
for testing the Spellcraft analyzer's detection capabilities.

## Statistics

- **Total Violations**: {len(self.violations)}
- **Categories Tested**: {len(set(v['category'] for v in self.violations))}
- **Source File**: {self.source_path.name}

## Violations by Category

"""
        # Group violations by category
        by_category = {}
        for v in self.violations:
            cat = v['category']
            if cat not in by_category:
                by_category[cat] = []
            by_category[cat].append(v)

        for cat, viols in sorted(by_category.items()):
            readme_content += f"### {cat}\n\n"
            for v in viols:
                readme_content += f"- **{v['id']}**: {v['type']} ({v['severity']})\n"
                readme_content += f"  - File: `{v['file']}`\n"
                readme_content += f"  - {v['description']}\n\n"

        readme_content += """
## Testing

Run the analyzer on all variants:

```bash
stack exec spellcraft -- contrib/lzx-chaos/*.vhd --report chaos-detection-report.json
```

## Expected Results

See `chaos-violations.json` for expected detection results. Each violation includes:
- `should_detect`: Whether the analyzer should catch this violation
- `detection_method`: Which analyzer component should detect it
- `currently_implemented`: Whether detection is implemented yet

## Validation

Compare actual detections against manifest:

```bash
python scripts/chaos-compare.py \\
  --manifest contrib/lzx-chaos/chaos-violations.json \\
  --results chaos-detection-report.json \\
  --output chaos-test-results.md
```
"""

        readme_path = self.output_dir / 'README.md'
        with open(readme_path, 'w') as f:
            f.write(readme_content)

        print(f"✓ Generated README: {readme_path}")

    def generate(self) -> None:
        """Main generation workflow"""
        print("\n📊 Analyzing source file...")
        content = self.read_source()
        structure = self.analyze_structure(content)
        print(f"   Entity: {self.entity_name}")
        print(f"   Architecture: {self.arch_name}")
        print(f"   Signals: {len(structure['signals'])}")
        print(f"   Processes: {structure['processes']}")

        # Create output directory
        self.output_dir.mkdir(parents=True, exist_ok=True)
        print(f"\n📁 Output directory: {self.output_dir}")

        # Generate violations (proof of concept - 3 violations)
        print("\n🔨 Generating violations...")
        print("\n  Category 1: Clock Domain Violations")
        self.generate_variant(self.inject_cat1_violation1, "cat1-violation1")

        print("\n  Category 3: Signal Integrity Issues")
        self.generate_variant(self.inject_cat3_violation1, "cat3-violation1")

        print("\n  Category 7: Functional Errors")
        self.generate_variant(self.inject_cat7_violation1, "cat7-violation1")

        # Generate documentation
        print("\n📝 Generating documentation...")
        self.generate_manifest()
        self.generate_readme()

        print(f"\n✅ Generation complete!")
        print(f"   Total violations: {len(self.violations)}")
        print(f"   Files created: {len(self.violations)} variants + manifest + README")
        print(f"\nNote: This is a proof-of-concept with {len(self.violations)} violations.")
        print("Full implementation should generate 21+ violations (3 per category).")

def main():
    parser = argparse.ArgumentParser(
        description='Generate VHDL chaos monkey test variants (POC implementation)'
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
║          Proof of Concept Implementation                      ║
║                 Contract: spellcraft-adc-011                  ║
╚═══════════════════════════════════════════════════════════════╝
""")

    monkey = VHDLChaosMonkey(args.source, args.output)
    monkey.generate()

    return 0

if __name__ == '__main__':
    sys.exit(main())
