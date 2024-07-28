{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SrcSpan
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module provides the 'SrcSpan' type along with operations for working
-- with source spans.
--
-- @since 1.0.0
module Data.SrcSpan
  ( -- * SrcSpan
    SrcSpan (..)
    -- ** Basic Operations
  , fromSrcLoc
  , diffSrcSpan
  , defaultSrcSpan
    -- ** Show
  , showSrcSpan
  , showsSrcSpan
  ) where

import Data.SrcLoc qualified as SrcLoc
import Data.SrcSpan.Core (SrcSpan (..), defaultSrcSpan, fromSrcLoc)

import Prelude hiding (span)

-- SrcSpan - Basic Operations --------------------------------------------------

-- | Similar to 'SrcLoc.diff' defined by "Data.SrcLoc", but uses the 'begin' and
-- 'end' source locations of the given source span when calculating the
-- difference in the source locations 'SrcLoc.posn' component.
--
-- @
-- 'diffSrcSpan' ('SrcSpan' a b) == 'SrcLoc.posn' b - 'SrcLoc.posn' a
-- @
--
-- @since 1.0.0
diffSrcSpan :: SrcSpan -> Int
diffSrcSpan (SrcSpan loc0 loc1) = SrcLoc.diff loc0 loc1
{-# INLINE diffSrcSpan #-}

-- SrcSpan - Show --------------------------------------------------------------

-- | The implementation of 'show' for 'SrcSpan'. Converts a 'SrcSpan' to a
-- 'String'.
--
-- >>> format (SrcSpan (SrcLoc 5 2 8) (SrcSpan 10 3 10))
-- "5:2:8-10:3:10"
--
-- @since 1.0.0
showSrcSpan :: SrcSpan -> String
showSrcSpan span = showsSrcSpan span ""

-- | Produce a 'ShowS' printer function for a given 'SrcSpan'.
--
-- @since 1.0.0
showsSrcSpan :: SrcSpan -> ShowS
showsSrcSpan (SrcSpan loc0 loc1) rest = SrcLoc.showsSrcLoc loc0 ('-' : SrcLoc.showsSrcLoc loc1 rest)
