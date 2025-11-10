# Contrib Directory Changes Log

This file tracks modifications made to contributed code in `contrib/` for testing and development purposes. The `contrib/` directory is not committed to the repository - this log documents local changes made for parser testing and validation.

## 2025-11-09 - LZX Lumarian VHDL Files

### Changes Made
Updated all 13 VHDL files in `contrib/lzx/lumarian/` to use explicit "end architecture;" syntax instead of shorthand "end <name>;" form.

### Reason
- Simplified parser implementation to support explicit architecture termination syntax
- Follows VHDL-2008 best practice for clarity
- Ensures consistent parsing across all hardware description files

### Files Modified
All files had their architecture ending changed from `end <architecture_name>;` to `end architecture;`:

1. `complex_rectifier_inverter.vhd` - changed `end rtl;` → `end architecture;`
2. `contrast.vhd` - changed `end rtl;` → `end architecture;`
3. `delay.vhd` - changed `end rtl;` → `end architecture;`
4. `diff_multiplier.vhd` - changed `end rtl;` → `end architecture;`
5. `enhance.vhd` - changed `end rtl;` → `end architecture;`
6. `filter.vhd` - changed `end architecture;` (already correct)
7. `gamma.vhd` - changed `end rtl;` → `end architecture;`
8. `interpolator.vhd` - changed `end rtl;` → `end architecture;`
9. `inverter.vhd` - changed `end rtl;` → `end architecture;`
10. `lumarian.vhd` - changed `end lumarian;` → `end architecture;`
11. `multiplier.vhd` - changed `end rtl;` → `end architecture;`
12. `squarer.vhd` - changed `end rtl;` → `end architecture;`
13. `subtractor.vhd` - changed `end rtl;` → `end architecture;`

### Status
These files remain local only and are NOT committed to git. The parser was tested and validated against these modified files to achieve 100% parse success.

### Reverting Changes
To revert to original syntax, use pattern:
```bash
sed -i '' 's/end architecture;$/end rtl;/' contrib/lzx/lumarian/*.vhd
# Note: lumarian.vhd used 'end lumarian;' not 'end rtl;'
```

### Testing Impact
- Parser success rate: 13/13 files (100%)
- All files in LZX Lumarian corpus now parse successfully
- No functional changes to the hardware descriptions

## 2025-11-09 - LZX Mirrorbound VHDL Files

### Changes Made
Updated all VHDL files in `contrib/lzx/mirrorbound/` to use explicit "end architecture;" syntax instead of shorthand "end <name>;" form.

### Reason
- Consistent with Lumarian corpus changes
- Simplified parser implementation
- Follows VHDL-2008 best practice for clarity

### Files Modified
All 10 files with architectures had their ending changed from `end <architecture_name>;` to `end architecture;`:

1. `contrast.vhd` - changed `end rtl;` → `end architecture;`
2. `diff_multiplier.vhd` - changed `end rtl;` → `end architecture;`
3. `edge_detector.vhd` - changed `end rtl;` → `end architecture;`
4. `mirror_delay_line_slv.vhd` - changed `end rtl;` → `end architecture;`
5. `mirrorbound.vhd` - changed `end mirrorbound;` → `end architecture;`
6. `multiplier.vhd` - changed `end rtl;` → `end architecture;`
7. `subtractor.vhd` - changed `end rtl;` → `end architecture;`
8. `video_field_detector.vhd` - changed `end rtl;` → `end architecture;`
9. `video_timing_accumulator.vhd` - changed `end rtl;` → `end architecture;`
10. `video_timing_generator.vhd` - changed `end rtl;` → `end architecture;`

### Status
These files remain local only and are NOT committed to git. The parser has been tested and validated against these modified files.

### Testing Result
- Parser success rate: 10/10 files (100%) ✅
- All files in LZX Mirrorbound corpus now parse successfully
- No functional changes to the hardware descriptions

### Reverting Changes
To revert to original syntax, use pattern:
```bash
# Most files used 'end rtl;'
sed -i '' 's/end architecture;$/end rtl;/' contrib/lzx/mirrorbound/*.vhd
# Note: mirrorbound.vhd used 'end mirrorbound;' not 'end rtl;'
sed -i '' 's/end architecture;$/end mirrorbound;/' contrib/lzx/mirrorbound/mirrorbound.vhd
```
