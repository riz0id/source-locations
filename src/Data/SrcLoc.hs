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
    -- ** Construction
  , defaultSrcLoc
    -- ** Basic Operations
  , diff
    -- ** Lenses
  , srcLocPosn
  , srcLocLine
  , srcLocColn
    -- ** Modification
  , nextColn
  , nextColns
  , nextLine
  , nextLines
    -- ** Feed
  , feed
  , feeds
  , feedsText
    -- ** Show
  , format
  , formats
  ) where

import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens)

import Data.Bool.Prim qualified as Bool
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
    -- ^ TODO: docs
  , line :: {-# UNPACK #-} !Int
    -- ^ TODO: docs
  , coln :: {-# UNPACK #-} !Int
    -- ^ TODO: docs
  }
  deriving (Data, Generic, Lift, Show)

-- | @since 1.0.0
instance Eq SrcLoc where
  loc0 == loc1 = Bool.toBool (unbox loc0 Ord.==# unbox loc1)
  {-# INLINE (==) #-}

  loc0 /= loc1 = Bool.toBool (unbox loc0 Ord./=# unbox loc1)
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance Ord SrcLoc where
  compare loc0 loc1 = Ord.toOrdering (Ord.compare# (unbox loc0) (unbox loc1))
  {-# INLINE compare #-}

  loc0 > loc1 = Bool.toBool (unbox loc0 Ord.># unbox loc1)
  {-# INLINE (>) #-}

  loc0 >= loc1 = Bool.toBool (unbox loc0 Ord.>=# unbox loc1)
  {-# INLINE (>=) #-}

  loc0 < loc1 = Bool.toBool (unbox loc0 Ord.<# unbox loc1)
  {-# INLINE (<) #-}

  loc0 <= loc1 = Bool.toBool (unbox loc0 Ord.<=# unbox loc1)
  {-# INLINE (<=) #-}

-- | @since 1.0.0
instance NFData SrcLoc

-- | @since 1.0.0
instance PrintfArg SrcLoc where
  formatArg loc fmt
    | 's' == fmtChar = shows loc
    | otherwise = Printf.errorBadFormat fmtChar
    where
      fmtChar :: Char
      fmtChar = Printf.fmtChar fmt
  {-# INLINE formatArg #-}

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

-- SrcLoc - Construction -------------------------------------------------------

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

-- SrcLoc - Basic Operations ---------------------------------------------------

infixl 6 `diff`

-- | Calculate the difference between the 'posn' component of two source
-- locations.
--
-- @since 1.0.0
diff :: SrcLoc -> SrcLoc -> Int
diff loc0 loc1 = I# (Prim.diff# (unbox loc0) (unbox loc1))
{-# INLINE diff #-}

-- srcloc - feed ---------------------------------------------------------------

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

-- SrcLoc - Lenses -------------------------------------------------------------

-- | Lens focusing on the 'posn' field of 'SrcLoc'.
--
-- @since 1.0.0
srcLocPosn :: Lens' SrcLoc Int
srcLocPosn = lens posn \s x -> s { posn = x }
{-# INLINE srcLocPosn #-}

-- | Lens focusing on the 'line' field of 'SrcLoc'.
--
-- @since 1.0.0
srcLocLine :: Lens' SrcLoc Int
srcLocLine = lens line \s x -> s { line = x }
{-# INLINE srcLocLine #-}

-- | Lens focusing on the 'coln' field of 'SrcLoc'.
--
-- @since 1.0.0
srcLocColn :: Lens' SrcLoc Int
srcLocColn = lens coln \s x -> s { coln = x }
{-# INLINE srcLocColn #-}

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
nextColn loc = box (Prim.nextColn# (unbox loc))
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
nextColns loc (I# n#) = box (Prim.nextColns# (unbox loc) n#)
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
nextLine loc = box (Prim.nextLine# (unbox loc))
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
nextLines loc (I# n#) = box (Prim.nextLines# (unbox loc) n#)
{-# INLINE nextLines #-}

-- SrcLoc - Show ---------------------------------------------------------------

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
