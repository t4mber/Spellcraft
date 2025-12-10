# Spellcraft CI/CD Integration

This directory contains GitHub Actions workflows for continuous integration and automated analysis of VHDL designs.

## Workflows

### `ci.yml` - Main CI Pipeline

Runs on:
- Push to `main`, `master`, or `release/*` branches
- Pull requests to `main` or `master`
- Manual trigger via `workflow_dispatch`

#### Jobs

| Job | Description |
|-----|-------------|
| **build** | Builds Spellcraft and runs the test suite |
| **analyze-contrib** | Analyzes all VHDL files in `contrib/` and generates JSON reports |
| **sarif-upload** | Generates SARIF report and uploads to GitHub Code Scanning |

## Output Artifacts

The `analyze-contrib` job produces the following artifacts:

```
reports/
├── lumarian.json      # LZX Lumarian analysis
├── mirrorbound.json   # LZX Mirrorbound analysis
├── lzx-kaos.json      # LZX KAOS analysis
├── codeglow.json      # Codeglow analysis (if VHDL files exist)
└── combined.json      # All reports combined with timestamp
```

Reports are retained for 30 days.

## Using Spellcraft in Your CI

### Basic JSON Export

```yaml
- name: Analyze VHDL
  run: |
    stack exec spellcraft -- --format=json src/*.vhd > report.json
```

### SARIF for GitHub Code Scanning

```yaml
- name: Generate SARIF
  run: |
    stack exec spellcraft -- --format=sarif src/*.vhd > results.sarif

- name: Upload SARIF
  uses: github/codeql-action/upload-sarif@v3
  with:
    sarif_file: results.sarif
```

### Fail on Errors

```yaml
- name: Check for violations
  run: |
    stack exec spellcraft -- --strict src/*.vhd
    # Exit code 1 if any violations found
```

### Videomancer Mode

```yaml
- name: Analyze video hardware
  run: |
    stack exec spellcraft -- --videomancer --config config.json --format=json src/*.vhd > report.json
```

## Command Line Options

| Option | Description |
|--------|-------------|
| `--format=json` | Output JSON format (machine-readable) |
| `--format=sarif` | Output SARIF format (GitHub Code Scanning) |
| `--format=text` | Output text format (default, human-readable) |
| `--strict` | Treat warnings as errors |
| `--suppress-warnings` | Only show errors |
| `--videomancer` | Enable video hardware validation mode |
| `--config FILE` | Load Videomancer configuration from JSON file |

## JSON Report Schema

```json
{
  "summary": {
    "files_analyzed": 13,
    "total_errors": 2,
    "total_warnings": 5
  },
  "violations": [
    {
      "file": "design.vhd",
      "line": 44,
      "column": 3,
      "severity": "error",
      "message": "Signal declared but never assigned (undriven)",
      "signal": "some_signal"
    }
  ]
}
```

## SARIF Integration

When SARIF reports are uploaded, violations appear in:
- **Security tab** > Code scanning alerts
- **Pull request** > Checks > Code scanning results

This enables:
- Automatic PR annotations
- Historical tracking of violations
- Integration with GitHub's security dashboard

## Local Testing

Run the same analysis locally:

```bash
# Build
stack build

# Analyze contrib examples
stack exec spellcraft -- --format=json contrib/lzx/lumarian/*.vhd

# Run full test suite
stack test
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | No errors found |
| 1 | Errors found (or warnings in `--strict` mode) |
| 2 | Parse errors or invalid input |
