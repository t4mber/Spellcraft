#!/bin/bash
echo "Analyzing parse failures..."
for f in $(find contrib/lzx -name "*.vhd"); do
  fname=$(basename "$f")
  result=$(stack exec spellcraft "$f" 2>&1)
  if ! echo "$result" | grep -q "✓"; then
    echo "=== FAIL: $fname ==="
    echo "$result" | grep -A3 "Parse error" | head -5
    echo ""
  fi
done
