#!/bin/bash
total=0
pass=0
for f in $(find contrib/lzx -name "*.vhd"); do
  total=$((total+1))
  if stack exec spellcraft "$f" 2>&1 | grep -q "✓"; then
    pass=$((pass+1))
    echo "PASS: $(basename "$f")"
  else
    echo "FAIL: $(basename "$f")"
  fi
done
echo ""
echo "=== Parse Rate: $pass/$total ($((pass*100/total))%) ==="
