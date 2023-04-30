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
-- This module provides the 'SrcSpan' type.
--
-- @since 1.0.0
module Data.SrcSpan
  ( module Data.SrcSpan.Core
    -- * Construction
  , defaultSrcSpan
  , fromSrcLoc
    -- * Basic Operations
  , diff
    -- * Show
  , format
  , formats
  ) where

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.SrcSpan.Core

import Prelude hiding (span)

-- SrcLoc - Construction -------------------------------------------------------

-- | The default source span.
--
-- @
-- 'defaultSrcSpan' == 'SrcSpan' 'SrcLoc.defaultSrcLoc' 'SrcLoc.defaultSrcLoc'
-- @
--
-- @since 1.0.0
defaultSrcSpan :: SrcSpan
defaultSrcSpan = fromSrcLoc SrcLoc.defaultSrcLoc
{-# INLINE CONLIKE defaultSrcSpan #-}

-- | Converts a given source location @loc@ into a source span. The resulting
-- source span will 'begin' and 'end' at the location @loc@.
--
-- @
-- 'fromSrcLoc' loc == 'SrcSpan' loc loc
-- @
--
-- @since 1.0.0
fromSrcLoc :: SrcLoc -> SrcSpan
fromSrcLoc loc = SrcSpan loc loc
{-# INLINE CONLIKE fromSrcLoc #-}

-- Basic Operations ------------------------------------------------------------

-- | Similar to 'SrcLoc.diff' defined by "Data.SrcLoc", but uses the 'begin' and
-- 'end' source locations of the given source span when calculating the
-- difference in the source locations 'SrcLoc.posn' component.
--
-- @
-- 'diff' ('SrcSpan' a b) == 'SrcLoc.posn' b - 'SrcLoc.posn' a
-- @
--
-- @since 1.0.0
diff :: SrcSpan -> Int
diff (SrcSpan loc0 loc1) = SrcLoc.diff loc0 loc1
{-# INLINE diff #-}

-- Basic Operations ------------------------------------------------------------

-- Show ------------------------------------------------------------------------

-- | TODO
--
-- >>> format (SrcSpan (SrcLoc 5 2 8) (SrcSpan 10 3 10))
-- "5:2:8-10:3:10"
--
-- @since 1.0.0
format :: SrcSpan -> String
format span = formats span ""

-- | TODO
--
-- @since 1.0.0
formats :: SrcSpan -> ShowS
formats (SrcSpan loc0 loc1) rest = SrcLoc.formats loc0 ('-' : SrcLoc.formats loc1 rest)
