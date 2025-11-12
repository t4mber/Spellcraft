# Test Fixtures

This directory contains VHDL test files used to validate parser features.

## Passing Tests (Parser Works)

### Basic Features
- `simple.vhd` - Basic entity/architecture
- `entity_generic.vhd` - Entity with generics
- `work_library.vhd` - Library and use clauses

### Expression Features (ADC-016, ADC-017, ADC-019)
- `attribute.vhd` - Attribute access (`signal'event`, `array'length`)
- `char_literal.vhd` - Character literals vs attributes
- `test_attr_expr.vhd` - Attribute expressions in assignments
- `power_op.vhd` - Power operator (`**`)
- `power_paren.vhd` - Power with parentheses
- `concat_op.vhd` - Concatenation operator
- `test_concat.vhd` - Concatenation in assignments

### Slice Features (ADC-017, ADC-023)
- `simple_slice.vhd` - Basic slice syntax
- `downto_slice.vhd` - Slice with expressions
- `test_slice_simple.vhd` - Slice on RHS of assignment
- `test_slice_to_failing.vhd` - Slice with TO direction + concatenation (fixed in ADC-023)

### Indexed Assignments (ADC-022)
- `test_indexed_assign.vhd` - Array indexing on LHS: `s_array(0) <= value;`

### Signal Features (ADC-018, ADC-020, ADC-021)
- `signal_init.vhd` - Signal initialization
- `test_entity_inst.vhd` - Entity instantiation with generic/port maps
- Generic map expressions: `G_MAX => (2 ** C_WIDTH) - 1`
- Port map expressions: `port => to_signed(0, 8)`

### Process Features
- `test_process_var.vhd` - Variable declarations in process
- `test_var_assign.vhd` - Variable assignments (`:=`)
- `test_for_loop.vhd` - For loops

### Statement Features (ADC-024)
- `inline_comment.vhd` - Inline comments after statements
- `test_case.vhd` - Case statements with multiple when clauses

### Complex Expressions
- `constant_in_expr.vhd` - Constants in expressions
- `constant_decl.vhd` - Constant declarations
- `complex_expr.vhd` - Complex nested expressions

## Known Failing Tests (Parser Issues)

None currently - all test files parse successfully!

## Test Coverage Summary

**Working Features:**
- ✅ Entity/architecture parsing
- ✅ Generic and port declarations
- ✅ Signal declarations with initialization
- ✅ Component instantiation (all 3 forms)
- ✅ Generic map expressions
- ✅ Port map expressions
- ✅ Indexed signal assignments
- ✅ Slice indexing (downto and to directions)
- ✅ Attribute access
- ✅ Power operator
- ✅ Concatenation
- ✅ Process with variables
- ✅ For loops
- ✅ Variable assignments
- ✅ Case statements with literal and 'others' choices
- ✅ Inline comments

**Known Issues:**
- ❌ Chained indexing: `array(i)(j)` (not yet tested)

## Adding New Tests

When adding test files:
1. Use descriptive names: `test_<feature>.vhd`
2. Keep tests minimal (focused on one feature)
3. Add comments explaining what's being tested
4. Update this README with the test category
5. Mark as passing/failing with explanation
