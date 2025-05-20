{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}

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
  ( -- * SrcLoc
    SrcLoc (..)
    -- ** Basic Operations
  , defaultSrcLoc
  , diff
  , boxSrcLoc
  , unboxSrcLoc
    -- ** Modification
  , nextColn
  , nextColns
  , nextLine
  , nextLines
    -- ** Feed
  , feed
  , feeds
  , feedsText
  , feedsByteString
    -- ** Show
  , showSrcLoc
  , showsSrcLoc
  ) where

import Control.DeepSeq (NFData)

import Data.Bool.Prim qualified as Bool
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.Ord.Prim qualified as Ord
import Data.SrcLoc.Prim (SrcLoc# (..))
import Data.SrcLoc.Prim qualified as Prim
import Data.Text (Text)
import Data.Text qualified as Text

import GHC.Exts (Char (..), Int (..))
import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Text.Printf (PrintfArg)
import Text.Printf qualified as Printf

-- SrcLoc ----------------------------------------------------------------------

-- | The 'SrcLoc' record is a location inside some file. Source locations are
-- represented as a position, line number, and column number triple.
--
-- @since 1.0.0
data SrcLoc = SrcLoc
  { posn :: {-# UNPACK #-} !Int
    -- ^ The offset of the 'SrcLoc'.
  , line :: {-# UNPACK #-} !Int
    -- ^ The line number that the offset of the 'SrcLoc' is on.
  , coln :: {-# UNPACK #-} !Int
    -- ^ The column that the offset of the 'SrcLoc' is on.
  }
  deriving (Data, Generic, Lift, Show)

-- | @since 1.0.0
instance Eq SrcLoc where
  loc0 == loc1 = Bool.toBool (unboxSrcLoc loc0 Ord.==# unboxSrcLoc loc1)
  {-# INLINE (==) #-}

  loc0 /= loc1 = Bool.toBool (unboxSrcLoc loc0 Ord./=# unboxSrcLoc loc1)
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance Ord SrcLoc where
  compare loc0 loc1 = Ord.toOrdering (Ord.compare# (unboxSrcLoc loc0) (unboxSrcLoc loc1))
  {-# INLINE compare #-}

  loc0 > loc1 = Bool.toBool (unboxSrcLoc loc0 Ord.># unboxSrcLoc loc1)
  {-# INLINE (>) #-}

  loc0 >= loc1 = Bool.toBool (unboxSrcLoc loc0 Ord.>=# unboxSrcLoc loc1)
  {-# INLINE (>=) #-}

  loc0 < loc1 = Bool.toBool (unboxSrcLoc loc0 Ord.<# unboxSrcLoc loc1)
  {-# INLINE (<) #-}

  loc0 <= loc1 = Bool.toBool (unboxSrcLoc loc0 Ord.<=# unboxSrcLoc loc1)
  {-# INLINE (<=) #-}

-- | @since 1.0.0
instance NFData SrcLoc

-- | @since 1.0.0
instance PrintfArg SrcLoc where
  formatArg loc fmt
    | 's' == fmtChar = shows loc
    | otherwise      = Printf.errorBadFormat fmtChar
    where
      fmtChar :: Char
      fmtChar = Printf.fmtChar fmt
  {-# INLINE formatArg #-}

-- SrcLoc - Basic Operations ---------------------------------------------------

-- | The default source location, equivalent to:
--
-- @
-- 'defaultSrcLoc' == v'SrcLoc' 0 1 1
-- @
--
-- @since 1.0.0
defaultSrcLoc :: SrcLoc
defaultSrcLoc = SrcLoc 0 1 1
{-# INLINE CONLIKE defaultSrcLoc #-}

infixl 6 `diff`

-- | Calculate the difference between the 'posn' component of two source
-- locations.
--
-- @since 1.0.0
diff :: SrcLoc -> SrcLoc -> Int
diff loc0 loc1 = I# (Prim.diff# (unboxSrcLoc loc0) (unboxSrcLoc loc1))
{-# INLINE diff #-}

-- | Box a 'SrcLoc'. Convert an unboxed 'SrcLoc#' to a boxed 'SrcLoc'.
--
-- @since 1.0.0
boxSrcLoc :: SrcLoc# -> SrcLoc
boxSrcLoc (SrcLoc# x# y# z#) = SrcLoc (I# x#) (I# y#) (I# z#)
{-# INLINE CONLIKE boxSrcLoc #-}

-- | Unbox a 'SrcLoc'. Convert an boxed 'SrcLoc#' to a boxed 'SrcLoc'.
--
-- @since 1.0.0
unboxSrcLoc :: SrcLoc -> SrcLoc#
unboxSrcLoc (SrcLoc (I# x#) (I# y#) (I# z#)) = SrcLoc# x# y# z#
{-# INLINE CONLIKE unboxSrcLoc #-}

-- SrcLoc - Feed ---------------------------------------------------------------

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
feed loc (C# chr#) = boxSrcLoc (Prim.feed# (unboxSrcLoc loc) chr#)
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

-- | Similar to 'feeds', but updates the fields of 'SrcLoc' by folding over a
-- 'Text' rather than a 'String'.
--
-- prop> feedsText loc xs == Text.foldl' feed loc xs
--
-- @since 1.0.0
feedsText :: SrcLoc -> Text -> SrcLoc
feedsText = Text.foldl' feed
{-# INLINE feedsText #-}

-- | Similar to 'feeds', but updates the fields of 'SrcLoc' by folding over a
-- 'ByteString' rather than a 'String'.
--
-- @since 1.0.0
feedsByteString :: SrcLoc -> ByteString -> SrcLoc
feedsByteString = ByteString.foldl' feed
{-# INLINE feedsByteString #-}

-- SrcLoc - Modification -------------------------------------------------------

-- | Advances the given source location to the next column. The resulting
-- source location will have:
--
-- * The 'posn' and 'coln' fields incremented by @1@.
--
-- * The same 'line' field as the original source location.
--
-- @since 1.0.0
nextColn :: SrcLoc -> SrcLoc
nextColn loc = boxSrcLoc (Prim.nextColn# (unboxSrcLoc loc))
{-# INLINE nextColn #-}

-- | Advances the given source location's column by @n@. The resulting source
-- location will have:
--
-- * The 'posn' and 'coln' fields incremented by @n@.
--
-- * The same 'line' field as the original source location.
--
-- @since 1.0.0
nextColns :: SrcLoc -> Int -> SrcLoc
nextColns loc (I# n#) = boxSrcLoc (Prim.nextColns# (unboxSrcLoc loc) n#)
{-# INLINE nextColns #-}

-- | Advances the given source location to the next line. The resulting source
-- location will have:
--
--   * The 'posn' and 'line' fields incremented by @1@.
--
--   * The 'coln' field reset to column @1@.
--
-- @since 1.0.0
nextLine :: SrcLoc -> SrcLoc
nextLine loc = boxSrcLoc (Prim.nextLine# (unboxSrcLoc loc))
{-# INLINE nextLine #-}

-- | Advances the given source location's line by @n@. The resulting source
-- location will have:
--
--   * The 'posn' and 'line' fields incremented by @n@.
--
--   * The 'coln' field reset to column @1@.
--
-- @since 1.0.0
nextLines :: SrcLoc -> Int -> SrcLoc
nextLines loc (I# n#) = boxSrcLoc (Prim.nextLines# (unboxSrcLoc loc) n#)
{-# INLINE nextLines #-}

-- SrcLoc - Show ---------------------------------------------------------------

-- | The implementation of 'show' for 'SrcLoc'. Converts a 'SrcLoc' to a
-- 'String'.
--
-- >>> format (SrcLoc 5 2 8)
-- "5:2:8"
--
-- @since 1.0.0
showSrcLoc :: SrcLoc -> String
showSrcLoc loc = showsSrcLoc loc ""

-- | Produce a 'ShowS' printer function for a given 'SrcLoc'.
--
-- @since 1.0.0
showsSrcLoc :: SrcLoc -> ShowS
showsSrcLoc (SrcLoc ps ln cn) rest = shows ps (':' : shows ln (':' : shows cn rest))
