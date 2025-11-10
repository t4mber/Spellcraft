#!/bin/bash
cd /Users/tad/t4mber/spellcraft
for f in contrib/lzx/lumarian/*.vhd contrib/lzx/mirrorbound/*.vhd; do
  result=$(stack exec spellcraft -- "$f" 2>&1 | grep "Parse results")
  if echo "$result" | grep -q "errors: 0"; then
    echo "✅ $(basename $f): PASS"
  else
    echo "❌ $(basename $f): FAIL"
  fi
done
