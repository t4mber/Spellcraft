{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-012
-- Priority 3: Arithmetic Bounds Checker for overflow detection
module VHDL.Analysis.ArithmeticBounds
  ( -- * Violation Types
    ArithmeticViolation(..)
    -- * Analysis Functions
  , checkArithmeticBounds
  , detectCounters
  , detectUnboundedCounters
  , calcExpressionWidth
    -- * Bit Width Types
  , BitWidth(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (mapMaybe, catMaybes, listToMaybe)
import VHDL.AST
import VHDL.SourceLocation (SourceLocation(..))

-- | Bit width information for expressions
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
data BitWidth = BitWidth
  { bwMin :: Integer      -- Minimum value representable
  , bwMax :: Integer      -- Maximum value representable
  , bwActual :: Int       -- Declared bit width
  , bwSigned :: Bool      -- Whether signed
  } deriving (Show, Eq)

-- | Arithmetic violations
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
data ArithmeticViolation
  = PotentialOverflow
      { ovSignal :: Identifier
      , ovOperation :: Text
      , ovLocation :: SourceLocation
      , ovDeclaredWidth :: Int
      , ovRequiredWidth :: Int
      , ovDescription :: Text
      }
  | UnboundedCounter
      { ucSignal :: Identifier
      , ucLocation :: SourceLocation
      , ucDescription :: Text
      }
  deriving (Show, Eq)

-- | Counter pattern information
data CounterPattern = CounterPattern
  { cpSignal :: Identifier
  , cpIncrement :: Integer
  , cpLocation :: SourceLocation
  , cpHasBoundsCheck :: Bool
  , cpBitWidth :: Maybe Int
  } deriving (Show, Eq)

-- | Check for arithmetic bounds violations in an architecture
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
checkArithmeticBounds :: Architecture -> [ArithmeticViolation]
checkArithmeticBounds arch =
  let -- Get signal declarations for bit width lookup
      signalWidths = collectSignalWidths arch
      -- Detect counter patterns
      counters = detectCounters arch
      -- Find unbounded counters
      unbounded = detectUnboundedCounters arch counters signalWidths
  in unbounded

-- | Collect signal bit widths from declarations
collectSignalWidths :: Architecture -> Map Identifier Int
collectSignalWidths arch = Map.fromList $ mapMaybe extractWidth (archSignals arch)
  where
    extractWidth decl =
      let typeName = sigDeclType decl
          width = parseTypeWidth typeName
      in case width of
           Just w -> Just (sigDeclName decl, w)
           Nothing -> Nothing

-- | Parse bit width from type name (e.g., "unsigned(7 downto 0)" -> 8)
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
parseTypeWidth :: TypeName -> Maybe Int
parseTypeWidth typeName
  | "std_logic_vector" `Text.isInfixOf` typeName = parseRangeWidth typeName
  | "unsigned" `Text.isInfixOf` typeName = parseRangeWidth typeName
  | "signed" `Text.isInfixOf` typeName = parseRangeWidth typeName
  | "std_logic" `Text.isSuffixOf` typeName = Just 1
  | otherwise = Nothing
  where
    -- Parse width from "(N-1 downto 0)" or "(0 to N-1)" patterns
    parseRangeWidth t =
      let -- Look for common patterns in type declarations
          -- Handle expressions like "G_WIDTH - 1 downto 0" by looking for "downto 0"
          hasDowntoZero = "downto 0" `Text.isInfixOf` t
          hasToPattern = " to " `Text.isInfixOf` t
      in if hasDowntoZero
         then extractDowntoWidth t
         else if hasToPattern
              then extractToWidth t
              else Nothing

    -- Extract width from "... N downto 0" -> N+1
    extractDowntoWidth t =
      let parts = Text.splitOn "(" t
      in case parts of
           (_:rangePart:_) ->
             let rangeParts = Text.splitOn " downto " rangePart
             in case rangeParts of
                  (highStr:_) -> parseHighBound highStr
                  _ -> Nothing
           _ -> Nothing

    -- Parse high bound, handling expressions like "G_WIDTH - 1"
    parseHighBound s =
      let stripped = Text.strip s
      in case Text.splitOn " - 1" stripped of
           [_] -> -- No " - 1", try direct parse
             case reads (Text.unpack stripped) :: [(Int, String)] of
               [(n, "")] -> Just (n + 1)
               _ -> Nothing
           _ -> Nothing  -- Has " - 1" but we can't evaluate generics

    extractToWidth t =
      let parts = Text.splitOn " to " t
      in case parts of
           (_:highPart:_) ->
             let numStr = Text.takeWhile (\c -> c >= '0' && c <= '9') (Text.strip highPart)
             in case reads (Text.unpack numStr) :: [(Int, String)] of
                  [(n, _)] -> Just (n + 1)
                  _ -> Nothing
           _ -> Nothing

-- | Detect counter patterns in architecture
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
detectCounters :: Architecture -> [CounterPattern]
detectCounters arch =
  concatMap detectInArchStmt (archStatements arch)
  where
    detectInArchStmt (ProcessStmt _ _ stmts _) =
      concatMap detectInStmt stmts
    detectInArchStmt _ = []

    detectInStmt :: Statement -> [CounterPattern]
    detectInStmt (SignalAssignment target expr loc) =
      case isCounterPattern target expr of
        Just (signal, increment) ->
          [CounterPattern
            { cpSignal = signal
            , cpIncrement = increment
            , cpLocation = loc
            , cpHasBoundsCheck = False  -- Will be updated later
            , cpBitWidth = Nothing
            }]
        Nothing -> []
    detectInStmt (IfStatement _ thenStmts elsifs elseStmts _) =
      concatMap detectInStmt thenStmts ++
      concatMap (concatMap detectInStmt . snd) elsifs ++
      concatMap detectInStmt elseStmts
    detectInStmt (CaseStatement _ whenClauses _) =
      concatMap (concatMap detectInStmt . snd) whenClauses
    detectInStmt (LoopStatement _ _ body _) =
      concatMap detectInStmt body
    detectInStmt _ = []

-- | Check if an assignment is a counter pattern: signal <= signal + N
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
isCounterPattern :: Expression -> Expression -> Maybe (Identifier, Integer)
isCounterPattern target expr =
  let targetName = extractTargetName target
  in case expr of
       -- signal <= signal + 1
       BinaryExpr Add left right
         | isSignalRef targetName left ->
             case getConstantValue right of
               Just n -> Just (targetName, n)
               Nothing -> Nothing
         | isSignalRef targetName right ->
             case getConstantValue left of
               Just n -> Just (targetName, n)
               Nothing -> Nothing
       -- signal <= signal - 1 (decrementing counter)
       BinaryExpr Sub left right
         | isSignalRef targetName left ->
             case getConstantValue right of
               Just n -> Just (targetName, -n)
               Nothing -> Nothing
       _ -> Nothing

-- | Extract target name from expression
extractTargetName :: Expression -> Identifier
extractTargetName (IdentifierExpr name) = name
extractTargetName (IndexedName base _) = extractTargetName base
extractTargetName (SliceExpr base _ _ _) = extractTargetName base
extractTargetName _ = "<complex>"

-- | Check if expression is a reference to the given signal
isSignalRef :: Identifier -> Expression -> Bool
isSignalRef name (IdentifierExpr n) = n == name
isSignalRef name (IndexedName base _) = isSignalRef name base
isSignalRef name (SliceExpr base _ _ _) = isSignalRef name base
isSignalRef _ _ = False

-- | Get constant value from expression
getConstantValue :: Expression -> Maybe Integer
getConstantValue (LiteralExpr (IntLiteral n)) = Just n
getConstantValue _ = Nothing

-- | Detect unbounded counters (counters without overflow protection)
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
detectUnboundedCounters :: Architecture -> [CounterPattern] -> Map Identifier Int -> [ArithmeticViolation]
detectUnboundedCounters arch counters signalWidths =
  let -- Check each counter for bounds protection
      unboundedCounters = filter (not . hasOverflowProtection arch) counters
  in map toViolation unboundedCounters
  where
    toViolation cp =
      UnboundedCounter
        { ucSignal = cpSignal cp
        , ucLocation = cpLocation cp
        , ucDescription = Text.pack $
            "Counter '" ++ Text.unpack (cpSignal cp) ++
            "' increments without overflow protection - will silently wrap" ++
            case Map.lookup (cpSignal cp) signalWidths of
              Just w -> " at " ++ show (2 ^ w :: Integer) ++ " (bit width: " ++ show w ++ ")"
              Nothing -> ""
        }

-- | Check if a counter has overflow protection
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
hasOverflowProtection :: Architecture -> CounterPattern -> Bool
hasOverflowProtection arch counter =
  let signal = cpSignal counter
  in any (checksSignalBounds signal) (archStatements arch)
  where
    checksSignalBounds :: Identifier -> ArchStatement -> Bool
    checksSignalBounds sig (ProcessStmt _ _ stmts _) =
      any (stmtChecksBounds sig) stmts
    checksSignalBounds _ _ = False

    -- Check if a statement contains bounds checking for the signal
    stmtChecksBounds :: Identifier -> Statement -> Bool
    stmtChecksBounds sig (IfStatement cond thenStmts elsifs elseStmts _) =
      exprComparesSig sig cond ||
      any (stmtChecksBounds sig) thenStmts ||
      any (\(c, s) -> exprComparesSig sig c || any (stmtChecksBounds sig) s) elsifs ||
      any (stmtChecksBounds sig) elseStmts
    stmtChecksBounds sig (CaseStatement _ whenClauses _) =
      any (\(_, s) -> any (stmtChecksBounds sig) s) whenClauses
    stmtChecksBounds sig (LoopStatement _ _ body _) =
      any (stmtChecksBounds sig) body
    stmtChecksBounds _ _ = False

    -- Check if expression compares the signal (bounds check)
    exprComparesSig :: Identifier -> Expression -> Bool
    exprComparesSig sig (BinaryExpr op left right)
      | op `elem` [Lt, Gt, LEq, GEq, Eq, NEq] =
          isSignalRef sig left || isSignalRef sig right
      | otherwise =
          exprComparesSig sig left || exprComparesSig sig right
    exprComparesSig sig (UnaryExpr _ e) = exprComparesSig sig e
    exprComparesSig _ _ = False

-- | Calculate expression bit width requirements
-- ADC-IMPLEMENTS: spellcraft-adc-012 Section: Arithmetic Bounds Checker
calcExpressionWidth :: Expression -> Maybe BitWidth
calcExpressionWidth (LiteralExpr (IntLiteral n)) =
  let width = ceiling (logBase 2 (fromIntegral (abs n + 1) :: Double)) :: Int
      actualWidth = max 1 width
  in Just $ BitWidth
       { bwMin = 0
       , bwMax = n
       , bwActual = actualWidth
       , bwSigned = n < 0
       }
calcExpressionWidth (BinaryExpr Add left right) = do
  leftW <- calcExpressionWidth left
  rightW <- calcExpressionWidth right
  let newMax = bwMax leftW + bwMax rightW
      -- Addition may need one extra bit
      newWidth = max (bwActual leftW) (bwActual rightW) + 1
  pure $ BitWidth
    { bwMin = bwMin leftW + bwMin rightW
    , bwMax = newMax
    , bwActual = newWidth
    , bwSigned = bwSigned leftW || bwSigned rightW
    }
calcExpressionWidth (BinaryExpr Mul left right) = do
  leftW <- calcExpressionWidth left
  rightW <- calcExpressionWidth right
  let newMax = bwMax leftW * bwMax rightW
      -- Multiplication needs sum of bit widths
      newWidth = bwActual leftW + bwActual rightW
  pure $ BitWidth
    { bwMin = bwMin leftW * bwMin rightW
    , bwMax = newMax
    , bwActual = newWidth
    , bwSigned = bwSigned leftW || bwSigned rightW
    }
calcExpressionWidth _ = Nothing  -- Complex expressions not analyzed
