{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
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
  ( -- * Source Locations
    SrcLoc (SrcLoc, posn, line, coln),

    -- * Construction
    empty,
    box,
    unbox,

    -- * Basic Operations
    feed,
    diff,
  )
where

import Data.Bool.Prim qualified as Bool
import Data.Data (Data)
import Data.Ord.Prim (Eq# (..), Ord# (..), toOrdering)

import GHC.Exts (Char (C#), Int (I#))

import Text.Printf (PrintfArg, formatArg)
import Text.Printf qualified as Text

--------------------------------------------------------------------------------

import Data.SrcLoc.Prim (SrcLoc# (SrcLoc#))
import Data.SrcLoc.Prim qualified as Prim

--------------------------------------------------------------------------------

-- | t'SrcLoc' is a location in a source file. Source locations are identified
-- by:
--
-- * The position (via 'Data.SrcLoc.posn') of the source location relative to
--   the beginning of the file.
--
-- * The line number (via 'Data.SrcLoc.line') at the source location position.
--
-- * The column (via 'Data.SrcLoc.coln') at the source location position.
--
-- @since 1.0.0
data SrcLoc = SrcLoc
  { posn :: {-# UNPACK #-} !Int
  , line :: {-# UNPACK #-} !Int
  , coln :: {-# UNPACK #-} !Int
  }
  deriving (Data, Show)

-- | @since 1.0.0
instance Eq SrcLoc where
  loc0 == loc1 = Bool.toBool (unbox loc0 ==# unbox loc1)
  {-# INLINE (==) #-}

  loc0 /= loc1 = Bool.toBool (unbox loc0 /=# unbox loc1)
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance Ord SrcLoc where
  compare loc0 loc1 = toOrdering (compare# (unbox loc0) (unbox loc1))
  {-# INLINE compare #-}

  loc0 > loc1 = Bool.toBool (unbox loc0 ># unbox loc1)
  {-# INLINE (>) #-}

  loc0 >= loc1 = Bool.toBool (unbox loc0 >=# unbox loc1)
  {-# INLINE (>=) #-}

  loc0 < loc1 = Bool.toBool (unbox loc0 <# unbox loc1)
  {-# INLINE (<) #-}

  loc0 <= loc1 = Bool.toBool (unbox loc0 <=# unbox loc1)
  {-# INLINE (<=) #-}

-- | @since 1.0.0
instance PrintfArg SrcLoc where
  formatArg loc fmt
    | 's' == fmtChar = shows loc
    | otherwise = Text.errorBadFormat fmtChar
    where
      fmtChar :: Char
      fmtChar = Text.fmtChar fmt
  {-# INLINE formatArg #-}

-- Construction ----------------------------------------------------------------

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

-- | TODO 
-- 
-- @since 1.0.0
box :: SrcLoc# -> SrcLoc
box (SrcLoc# x# y# z#) = SrcLoc (I# x#) (I# y#) (I# z#)
{-# INLINE CONLIKE box #-}

-- | TODO 
-- 
-- @since 1.0.0
unbox :: SrcLoc -> SrcLoc#
unbox (SrcLoc (I# x#) (I# y#) (I# z#)) = SrcLoc# x# y# z#
{-# INLINE CONLIKE unbox #-}

-- Basic Operations ------------------------------------------------------------

infixl 6 `diff`

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

-- | Take the difference of two t'SrcLoc' source positions.
--
-- @since 1.0.0
diff :: SrcLoc -> SrcLoc -> Int
diff loc0 loc1 = I# (Prim.diff# (unbox loc0) (unbox loc1))
{-# INLINE diff #-}