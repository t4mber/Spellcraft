# Parsing Anti-Patterns

**Status**: Active Policy
**Date**: 2025-11-12
**Context**: Discovered during ADC-013 Process Body Parsing implementation

## Overview

This document records anti-patterns discovered during parser development and provides recommended alternatives. Following these guidelines will prevent infinite loops, hanging parsers, and subtle backtracking bugs.

## Critical Anti-Patterns

### ❌ AP-001: `manyTill` with Consuming Parsers

**Problem**: Using `manyTill p end` where `p` consumes input (like whitespace) even when it fails will cause infinite loops.

**Symptom**: Parser hangs indefinitely, repeatedly trying to parse at the same or advancing position.

**Example (BAD)**:
```haskell
parseSequentialStatements :: Parser [Statement]
parseSequentialStatements = do
  stmts <- manyTill parseSequentialStatement (lookAhead $ keyword "end")
  pure stmts
  where
    parseSequentialStatement = do
      sc  -- ⚠️ Consumes whitespace BEFORE trying alternatives
      choice
        [ try parseSignalAssignment
        , try parseIfStatement
        ...
        ]
```

**Why It Fails**:
1. `parseSequentialStatement` calls `sc`, consuming whitespace
2. All alternatives in `choice` fail
3. `manyTill` checks `lookAhead $ keyword "end"`, which also fails
4. `manyTill` tries `parseSequentialStatement` again at new position (after whitespace)
5. Infinite loop

**Solution (GOOD)**:
```haskell
parseSequentialStatements :: Parser [Statement]
parseSequentialStatements = do
  stmts <- many (try (notFollowedBy endMarker >> parseSequentialStatement))
  pure stmts
  where
    endMarker = keyword "end" >> keyword "process"

    parseSequentialStatement = do
      sc  -- ✅ OK here because we check endMarker first
      choice [...]
```

**Rationale**: `many` with `notFollowedBy` explicitly checks for termination BEFORE trying to parse. The `try` wrapper ensures backtracking if `endMarker` succeeds.

### ❌ AP-002: `choice` Without `try` Wrappers

**Problem**: Using `choice` with alternatives that consume input before failing prevents later alternatives from being tried.

**Symptom**: Parser fails even when a valid alternative exists later in the choice list.

**Example (BAD)**:
```haskell
parseUnaryExpr :: Parser Expression
parseUnaryExpr = choice
  [ UnaryExpr Not <$> (keyword "not" >> parseUnaryExpr)  -- ⚠️ No try
  , UnaryExpr Negate <$> (symbol "-" >> parseUnaryExpr)  -- ⚠️ No try
  , parsePrimaryExpr
  ]
```

**Why It Fails**:
1. First alternative calls `keyword "not"`, which consumes whitespace via lexeme
2. If input is not "not", the keyword fails but whitespace is consumed
3. `choice` doesn't backtrack without `try`, so remaining alternatives fail

**Solution (GOOD)**:
```haskell
parseUnaryExpr :: Parser Expression
parseUnaryExpr = choice
  [ try $ UnaryExpr Not <$> (keyword "not" >> parseUnaryExpr)  -- ✅
  , try $ UnaryExpr Negate <$> (symbol "-" >> parseUnaryExpr)  -- ✅
  , parsePrimaryExpr  -- Last alternative doesn't need try
  ]
```

**Rationale**: `try` restores parser state on failure, allowing subsequent alternatives to be attempted.

### ❌ AP-003: Consuming Whitespace Before All Alternatives

**Problem**: Calling `sc` before a choice of parsers that might fail causes consumption without progress.

**Symptom**: Parser loops or fails unexpectedly when input doesn't match first alternative.

**Example (BAD)**:
```haskell
parseStatement = do
  sc  -- ⚠️ Always consumes, even if no statement follows
  choice
    [ try parseAssignment
    , try parseIf
    ]
```

**Solution (GOOD)**:
```haskell
-- Option 1: Let lexemes handle whitespace
parseStatement = choice
  [ try parseAssignment  -- Each parser's lexemes handle whitespace
  , try parseIf
  ]

-- Option 2: Check for valid start first
parseStatement = do
  lookAhead $ choice [symbol "<=", keyword "if"]  -- Verify valid start
  sc  -- Now safe to consume
  choice [...]
```

### ❌ AP-004: `manyTill` with Ambiguous Terminator

**Problem**: Using `manyTill p (lookAhead end)` where `end` might never succeed, even with valid input.

**Symptom**: Parser hangs, never finding termination condition.

**Example (BAD)**:
```haskell
-- Parsing "begin ... end process" but checking only for "end"
parseStatements = manyTill parseStatement (lookAhead $ keyword "end")
```

**Why It Fails**: If `keyword "end"` is too generic and consumed by a nested construct, the terminator is never found at the top level.

**Solution (GOOD)**:
```haskell
-- Option 1: More specific terminator
parseStatements = manyTill parseStatement
  (lookAhead $ try $ keyword "end" >> keyword "process")

-- Option 2: Use many with explicit guards (PREFERRED)
parseStatements = many (try (notFollowedBy endMarker >> parseStatement))
  where endMarker = keyword "end" >> keyword "process"
```

## Recommended Patterns

### ✅ RP-001: `many` with `notFollowedBy` Guards

**Best for**: Parsing sequences with clear termination markers.

```haskell
parseStatements :: Parser [Statement]
parseStatements = many (try (notFollowedBy endMarker >> parseStatement))
  where
    endMarker = choice [keyword "end", keyword "else", keyword "elsif"]
```

**Advantages**:
- Explicit termination check
- Clear intent
- Fails fast when terminator found
- No risk of infinite loops

### ✅ RP-002: Lexeme-Based Whitespace Handling

**Best for**: Token parsers in languages with flexible whitespace.

```haskell
-- Define lexeme once
lexeme :: Parser a -> Parser a
lexeme p = p <* sc

-- Use in all tokens
keyword :: Text -> Parser ()
keyword kw = lexeme $ string kw >> notFollowedBy alphaNumChar

-- Parsers automatically handle whitespace
parseAssignment = do
  target <- identifier  -- Automatically skips trailing whitespace
  void $ symbol "<="    -- Automatically skips trailing whitespace
  expr <- parseExpression
  void semi
  pure $ Assignment target expr
```

**Advantages**:
- Consistent whitespace handling
- No manual `sc` calls in parser combinators
- Reduces bugs from forgetting `sc`

### ✅ RP-003: Top-Level `try` in `choice`

**Best for**: Alternatives that may consume input before failing.

```haskell
parseExpr = choice
  [ try parseFunctionCall  -- May parse identifier before seeing no '('
  , try parseIndexed       -- May parse identifier before seeing no '['
  , parseIdentifier        -- Last alternative, no try needed
  ]
```

**Advantages**:
- Proper backtracking
- Predictable behavior
- Clear intent

### ✅ RP-004: Lookahead for Disambiguation

**Best for**: Avoiding unnecessary consumption when checking conditions.

```haskell
parseFunctionCallOrIndexed = try $ do
  name <- identifier
  -- Use lookahead to check for '(' without consuming it yet
  lookAhead $ symbol "("
  -- Only if '(' exists, parse the arguments
  args <- parens $ parseExpression `sepBy` comma
  pure $ FunctionCall name args
```

**Advantages**:
- Minimal consumption before verification
- Better error messages
- Enables better alternative ordering

## Historical Context

### ADC-013 Implementation Journey

During the implementation of process body parsing, we encountered a critical infinite loop caused by AP-001 (`manyTill` with consuming parsers).

**Timeline**:
1. **Initial Implementation**: Used `manyTill parseSequentialStatement (lookAhead $ keyword "end")`
2. **Bug Discovered**: Parser hung indefinitely on all process bodies
3. **Root Cause**: `parseSequentialStatement` called `sc` before trying alternatives, consuming whitespace even when no statement followed
4. **Failed Fix Attempt**: Changed to `many parseSequentialStatement` (still hung)
5. **Final Solution**: Changed to `many (try (notFollowedBy endMarker >> parseSequentialStatement))` with proper guards

**Key Insight**: The user identified that "manyTill has been an anti-pattern" after experiencing this issue firsthand. This document codifies that insight for future development.

## Testing for Anti-Patterns

### Test Case Template

When implementing new parsers, include these test cases:

1. **Termination Test**: Verify parser stops at expected terminators
2. **Whitespace Test**: Verify correct behavior with various whitespace patterns
3. **Timeout Test**: Verify no infinite loops (use short timeout in tests)
4. **Backtracking Test**: Verify alternatives are tried when early ones fail

### Example Test

```haskell
it "should not hang on empty process body" $ do
  let input = "process begin end process;"
  parseTest processParser input `shouldTimeoutWithin` 1000  -- 1 second

it "should backtrack correctly on identifier vs function call" $ do
  let input = "x"  -- Not a function call
  parseTest parseExpression input `shouldParse` IdentifierExpr "x"
```

## Policy Enforcement

1. **Code Review**: Check for anti-patterns in all parser PRs
2. **Linting**: Consider adding custom linter rules for common anti-patterns
3. **Documentation**: Reference this document in parser module comments
4. **Training**: Onboard new contributors with anti-pattern examples

## References

- **Megaparsec Documentation**: https://hackage.haskell.org/package/megaparsec
- **Parsec Design Patterns**: "Parsing with Haskell" by Alex Ionescu
- **ADC-013**: Process Body Parsing Implementation Contract
- **Parser.hs**: `/src/VHDL/Parser.hs` (refactored 2025-11-12)

## Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2025-11-12 | 1.0 | Initial creation based on ADC-013 debugging experience |

---

**Note**: This is a living document. Add new anti-patterns as they are discovered, with concrete examples and solutions.
