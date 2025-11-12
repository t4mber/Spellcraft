#!/usr/bin/env python3
import subprocess
import glob
import os

def test_file(filepath):
    """Test a single VHDL file and return status."""
    try:
        result = subprocess.run(
            ['stack', 'exec', 'spellcraft', '--', filepath],
            capture_output=True,
            text=True,
            timeout=10
        )
        output = result.stdout + result.stderr

        if 'Analysis complete' in output:
            return 'pass'
        elif 'Found' in output and 'error' in output:
            return 'violations'
        else:
            return 'failed'
    except subprocess.TimeoutExpired:
        return 'timeout'
    except Exception as e:
        return 'error'

def test_corpus(name, path):
    """Test all files in a corpus."""
    print(f"\n{'='*70}")
    print(f"  {name} CORPUS")
    print(f"{'='*70}\n")

    files = sorted(glob.glob(f"{path}/*.vhd"))
    results = {'pass': [], 'violations': [], 'failed': [], 'timeout': [], 'error': []}

    for i, filepath in enumerate(files, 1):
        filename = os.path.basename(filepath)
        status = test_file(filepath)
        results[status].append(filename)

        status_icon = {
            'pass': '✅',
            'violations': '⚠️ ',
            'failed': '❌',
            'timeout': '⏱️ ',
            'error': '🔥'
        }

        print(f"  [{i:2d}] {filename:40s} {status_icon[status]} {status.upper()}")

    total = len(files)
    passed = len(results['pass'])
    violations = len(results['violations'])
    failed = len(results['failed']) + len(results['timeout']) + len(results['error'])

    print(f"\n  Summary:")
    print(f"    Total files:      {total}")
    print(f"    ✅ Passed:         {passed} ({passed*100//total}%)")
    print(f"    ⚠️  Violations:     {violations} ({violations*100//total}%)")
    print(f"    ❌ Failed:         {failed} ({failed*100//total}%)")
    print(f"    Parse success:    {(total-failed)*100//total}%")

    return total, passed, violations, failed

if __name__ == '__main__':
    print("\n")
    print("="*70)
    print("  SPELLCRAFT 0.4.0 - COMPREHENSIVE CORPUS TEST")
    print("="*70)

    # Test each corpus
    lum_total, lum_pass, lum_viol, lum_fail = test_corpus(
        "LUMARIAN", "contrib/lzx/lumarian"
    )

    mir_total, mir_pass, mir_viol, mir_fail = test_corpus(
        "MIRRORBOUND", "contrib/lzx/mirrorbound"
    )

    kaos_total, kaos_pass, kaos_viol, kaos_fail = test_corpus(
        "KAOS", "contrib/lzx-kaos-levels"
    )

    # Overall summary
    total = lum_total + mir_total + kaos_total
    passed = lum_pass + mir_pass + kaos_pass
    violations = lum_viol + mir_viol + kaos_viol
    failed = lum_fail + mir_fail + kaos_fail

    print(f"\n{'='*70}")
    print(f"  OVERALL RESULTS")
    print(f"{'='*70}\n")
    print(f"  Corpus Breakdown:")
    print(f"    Lumarian:      {lum_total} files ({lum_pass} pass, {lum_viol} viol, {lum_fail} fail)")
    print(f"    Mirrorbound:   {mir_total} files ({mir_pass} pass, {mir_viol} viol, {mir_fail} fail)")
    print(f"    Kaos:          {kaos_total} files ({kaos_pass} pass, {kaos_viol} viol, {kaos_fail} fail)")
    print(f"\n  Combined Metrics:")
    print(f"    Total files:         {total}")
    print(f"    ✅ Clean pass:        {passed} ({passed*100//total}%)")
    print(f"    ⚠️  With violations:   {violations} ({violations*100//total}%)")
    print(f"    ❌ Parse failures:    {failed} ({failed*100//total}%)")
    print(f"\n  Key Metrics:")
    print(f"    Parse success rate:  {(total-failed)*100//total}%")
    print(f"    Clean file ratio:    {passed*100//total}%")
    print(f"\n{'='*70}\n")

    # Kaos validation
    print(f"  Kaos Validation:")
    if kaos_viol > 0:
        print(f"    ✅ Violation detection working: {kaos_viol}/{kaos_total} files correctly flagged")
    else:
        print(f"    ⚠️  No violations detected (expected violations)")
    print()
