{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SrcLoc
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module provides the 'SrcLoc' type.
--
-- @since 1.0.0
module Data.SrcLoc
  ( -- * Source Locations
    SrcLoc (SrcLoc),

    -- * Construction
    empty,

    -- * Basic Operations
    feed,
    diff,

#ifdef LENS
    -- * Lenses
    module Data.SrcLoc.Lens
#endif
  )
where

import GHC.Exts (Char (C#))

--------------------------------------------------------------------------------

import Data.SrcLoc.Core (SrcLoc (SL, SrcLoc))
import Data.SrcLoc.Prim qualified as Prim

#ifdef LENS
import Data.SrcLoc.Lens (posn, line, coln)
#endif

-- Construction ----------------------------------------------------------------

-- | The empty source location, equivalent to:
--
-- @
-- 'empty' == 'SrcLoc' 0 1 1 
-- @
--
-- @since 1.0.0
empty :: SrcLoc
empty = SrcLoc 0 1 1
{-# INLINE CONLIKE empty #-}

-- Basic Operations ------------------------------------------------------------

-- | "Feeds" a character to a 'SrcLoc'. This produces a new 'SrcLoc' with
-- fields incremented according to the kind character the source location was
-- fed.
--
-- * For a newline character (i.e. @'\\n'@ or @'\\r'@) the position and line
--   number fields are incremented. The columnn field is set to @1@.
--
-- * For any character that is not a newline character, the position and 
--   column number fields are increment. The line number is left unmodified.
--
-- >>> foldl feed empty "abc \n xyz"
-- SrcLoc 9 2 5
--
-- @since 1.0.0
feed :: SrcLoc -> Char -> SrcLoc
feed (SL loc#) (C# chr#) = SL (Prim.feed# loc# chr#)
{-# INLINE feed #-}

-- | Take the difference of two 'SrcLoc' source positions.
--
-- @since 1.0.0
diff :: SrcLoc -> SrcLoc -> Int
diff (SrcLoc p0 _ _) (SrcLoc p1 _ _) = p1 - p0
{-# INLINE diff #-}
