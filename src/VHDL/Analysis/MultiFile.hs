{-# LANGUAGE OverloadedStrings #-}

-- ADC-IMPLEMENTS: spellcraft-adc-029
-- | Multi-file analysis context building and entity resolution
--
-- This module provides functionality to build a shared analysis context
-- from multiple parsed VHDL files, enabling resolution of cross-file
-- entity references and accurate component output detection.
module VHDL.Analysis.MultiFile
  ( -- * Context Building
    buildContext
  , mergeContexts
    -- * Entity Resolution
  , lookupEntity
  , getPortDirection
  , isOutputPort
    -- * Component Output Detection
  , getComponentOutputPorts
  , isKnownComponentOutput
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import VHDL.AST

-- =============================================================================
-- Context Building
-- =============================================================================

-- | Build analysis context from multiple parsed designs
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- Collects all entities and architectures from the designs and indexes
-- them for quick lookup during signal usage analysis.
buildContext :: [VHDLDesign] -> AnalysisContext
buildContext designs = AnalysisContext
  { ctxEntities = buildEntityMap designs
  , ctxArchitectures = buildArchMap designs
  , ctxDesigns = designs
  , ctxSourceFiles = map designSourceFile designs
  }

-- | Build entity map from designs
-- Entities are indexed by name (without library prefix)
buildEntityMap :: [VHDLDesign] -> Map Identifier Entity
buildEntityMap designs =
  Map.fromList
    [ (entityName ent, ent)
    | design <- designs
    , ent <- designEntities design
    ]

-- | Build architecture map from designs
-- Architectures are indexed by (archName, entityName)
buildArchMap :: [VHDLDesign] -> Map (Identifier, Identifier) Architecture
buildArchMap designs =
  Map.fromList
    [ ((archName arch, archEntityName arch), arch)
    | design <- designs
    , arch <- designArchitectures design
    ]

-- | Merge two analysis contexts
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- Combines entities and architectures from both contexts.
-- If the same entity appears in both, the second context's version wins.
mergeContexts :: AnalysisContext -> AnalysisContext -> AnalysisContext
mergeContexts ctx1 ctx2 = AnalysisContext
  { ctxEntities = Map.union (ctxEntities ctx2) (ctxEntities ctx1)
  , ctxArchitectures = Map.union (ctxArchitectures ctx2) (ctxArchitectures ctx1)
  , ctxDesigns = ctxDesigns ctx1 ++ ctxDesigns ctx2
  , ctxSourceFiles = ctxSourceFiles ctx1 ++ ctxSourceFiles ctx2
  }

-- =============================================================================
-- Entity Resolution
-- =============================================================================

-- | Look up entity by name in context
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- Handles both bare names ("contrast_u") and qualified names ("work.contrast_u").
-- For qualified names, the library prefix is stripped before lookup.
lookupEntity :: AnalysisContext -> Identifier -> Maybe Entity
lookupEntity ctx name =
  let normalizedName = stripLibraryPrefix name
  in Map.lookup normalizedName (ctxEntities ctx)

-- | Strip library prefix from entity name
-- "work.contrast_u" -> "contrast_u"
-- "contrast_u" -> "contrast_u"
stripLibraryPrefix :: Identifier -> Identifier
stripLibraryPrefix name =
  case T.breakOn "." name of
    (_, rest) | T.null rest -> name  -- No dot, return as-is
    (_, rest) -> T.drop 1 rest       -- Drop the dot and return the rest

-- | Get port direction for a given entity and port name
-- ADC-IMPLEMENTS: spellcraft-adc-029
getPortDirection :: AnalysisContext -> Identifier -> Identifier -> Maybe PortDirection
getPortDirection ctx entName prtName = do
  entity <- lookupEntity ctx entName
  port <- findPort (entityPorts entity) prtName
  pure (portDirection port)

-- | Find a port by name in a list of port declarations
findPort :: [PortDecl] -> Identifier -> Maybe PortDecl
findPort ports name =
  case filter (\p -> portName p == name) ports of
    [port] -> Just port
    _      -> Nothing  -- Not found or ambiguous

-- | Check if a port is an output port (out or inout)
-- ADC-IMPLEMENTS: spellcraft-adc-029
isOutputPort :: AnalysisContext -> Identifier -> Identifier -> Bool
isOutputPort ctx entName prtName =
  case getPortDirection ctx entName prtName of
    Just Output -> True
    Just InOut  -> True
    _           -> False

-- =============================================================================
-- Component Output Detection
-- =============================================================================

-- | Get all output port names for a component
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- Returns a list of (portName, actualSignal) pairs for all output ports
-- of the component instantiation that could be resolved.
getComponentOutputPorts :: AnalysisContext -> ComponentInst -> [(Identifier, Identifier)]
getComponentOutputPorts ctx inst =
  case lookupEntity ctx (compComponentName inst) of
    Nothing -> []  -- Entity not found, can't determine outputs
    Just entity ->
      let outputPorts = filter isOutput (entityPorts entity)
          outputNames = map portName outputPorts
      in mapMaybe (matchPortToActual outputNames (compPortMap inst)) outputNames
  where
    isOutput p = portDirection p == Output || portDirection p == InOut

-- | Match a formal port name to its actual signal in the port map
matchPortToActual :: [Identifier] -> [(Expression, Expression)] -> Identifier -> Maybe (Identifier, Identifier)
matchPortToActual _outputNames portMap formalName =
  -- Look for a port map entry where the formal matches
  case filter (matchesFormal formalName) portMap of
    [(_, actual)] ->
      -- Extract the signal name from the actual expression
      case extractSignalName actual of
        Just sigName -> Just (formalName, sigName)
        Nothing -> Nothing
    _ -> Nothing  -- Not found or multiple matches

-- | Check if a port map entry's formal matches the given name
matchesFormal :: Identifier -> (Expression, Expression) -> Bool
matchesFormal name (formal, _) =
  case extractSignalName formal of
    Just formalName -> formalName == name
    Nothing -> False

-- | Extract signal name from an expression
-- Handles simple identifiers and indexed expressions
extractSignalName :: Expression -> Maybe Identifier
extractSignalName (IdentifierExpr name) = Just name
extractSignalName (IndexedName base _) = extractSignalName base
extractSignalName (SliceExpr base _ _ _) = extractSignalName base
extractSignalName (FunctionCall name _) = Just name  -- Array indexing parses as function call
extractSignalName _ = Nothing

-- | Check if a signal is driven by a known component output
-- ADC-IMPLEMENTS: spellcraft-adc-029
--
-- Given a component instantiation and a signal name, determines if that
-- signal is connected to an output port of the component (where the
-- component's entity is known in the context).
isKnownComponentOutput :: AnalysisContext -> ComponentInst -> Identifier -> Bool
isKnownComponentOutput ctx inst signalName =
  signalName `elem` map snd (getComponentOutputPorts ctx inst)
