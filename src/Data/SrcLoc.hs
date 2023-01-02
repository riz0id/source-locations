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
-- This module provides the t'SrcLoc' type.
--
-- @since 1.0.0
module Data.SrcLoc
  ( module Data.SrcLoc.Core,

    -- * Construction
    empty,

    -- * Basic Operations
    diff,

    -- * Modification
    nextColn,
    nextLine,

    -- * Feed 
    feed,
    feeds,
    feedsText,

    -- * Show
    format,
    formats,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text

import GHC.Exts (Char (C#), Int (I#))

--------------------------------------------------------------------------------

import Data.SrcLoc.Core
import Data.SrcLoc.Prim qualified as Prim
import Data.Foldable (foldl')

-- SrcLoc - Construction -------------------------------------------------------

-- | The empty source location, equivalent to:
--
-- @
-- 'empty' == v'SrcLoc' 0 1 1
-- @
--
-- @since 1.0.0
empty :: SrcLoc
empty = SrcLoc 0 1 1
{-# INLINE CONLIKE empty #-}

-- Basic Operations ------------------------------------------------------------

infixl 6 `diff`

-- | Calculate the difference between the 'posn' component of two source 
-- locations.
--
-- @since 1.0.0
diff :: SrcLoc -> SrcLoc -> Int
diff loc0 loc1 = I# (Prim.diff# (unbox loc0) (unbox loc1))
{-# INLINE diff #-}

-- Modification ----------------------------------------------------------------

-- | Advances the given source location to the next column. The resulting source
-- location will have:
--
-- * The 'posn' and 'coln' fields incremented by @1@.
--
-- * The same 'line' field as the original source location.
--
-- @since 1.0.0
nextColn :: SrcLoc -> SrcLoc
nextColn loc = box (Prim.nextColn# (unbox loc))
{-# INLINE nextColn #-}

-- | Advances the given source location to the next line. The resulting source 
-- location will have:
--
--   * The 'posn' and 'line' fields incremented by @1@.
--
--   * The 'coln' field reset to column @1@.
--
-- @since 1.0.0
nextLine :: SrcLoc -> SrcLoc
nextLine loc = box (Prim.nextLine# (unbox loc))
{-# INLINE nextLine #-}

-- Feed ------------------------------------------------------------------------

-- | "Feeds" a character to a t'SrcLoc'. This produces a new t'SrcLoc' with
-- fields incremented according to the kind character the source location was
-- fed.
--
-- * For a newline character the position and line number fields are
--   incremented. The columnn field is set to @1@.
--
-- * For any character that is not a newline character, the position and
--   column number fields are increment. The line number is left unmodified.
--
-- >>> foldl feed empty "abc \n xyz"
-- SrcLoc 9 2 5
--
-- @since 1.0.0
feed :: SrcLoc -> Char -> SrcLoc
feed loc (C# chr#) = box (Prim.feed# (unbox loc) chr#)
{-# INLINE feed #-}

-- | The 'feeds' function updates a 'SrcLoc' according to the characters in a 
-- given string by strictly folding 'feed' over the input string:
--
-- prop> feeds loc xs == foldl' feed loc xs
--
-- @since 1.0.0
feeds :: SrcLoc -> String -> SrcLoc
feeds = foldl' feed
{-# INLINE feeds #-}

-- | Similar to 'feeds', but updates the fields of 'SrcLoc' by folding a 'Text'
-- rather than a 'String'.
--
-- prop> feedsText loc xs == Text.foldl' feed loc xs
--
-- @since 1.0.0
feedsText :: SrcLoc -> Text -> SrcLoc
feedsText = Text.foldl' feed

-- Show ------------------------------------------------------------------------

-- | TODO
--
-- >>> format (SrcLoc 5 2 8)
-- "5:2:8"
--
-- @since 1.0.0
format :: SrcLoc -> String
format loc = formats loc ""

-- | TODO 
--
-- @since 1.0.0
formats :: SrcLoc -> ShowS
formats (SrcLoc ps ln cn) rest = shows ps (':' : shows ln (':' : shows cn rest))