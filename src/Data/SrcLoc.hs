{-# LANGUAGE DataKinds #-}
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
    SrcLoc (SL), 
    pattern SrcLoc, posn, line, coln,

    -- * Construction
    empty,

    -- * Basic Operations
    feed,
    diff,
  )
where

import Data.Bool.Prim qualified as Bool
import Data.Data (Constr, Data, DataType)
import Data.Data qualified as Data
import Data.Int.Prim (Int#)
import Data.Ord.Prim (Eq# (..), Ord# (..), toOrdering)

import GHC.Exts (Char (C#), Int (I#))
import GHC.Records (HasField, getField)

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift, lift, liftTyped)

import Text.Printf (PrintfArg, formatArg)
import Text.Printf qualified as Text

--------------------------------------------------------------------------------

import Data.SrcLoc.Prim (SrcLoc# (SL#, SrcLoc#))
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
data SrcLoc = SL SrcLoc#

-- | @since 1.0.0
instance Data SrcLoc where
  toConstr _ = constr
  {-# INLINE CONLIKE toConstr #-}

  dataTypeOf _ = datatype
  {-# INLINE CONLIKE dataTypeOf #-}

  gfoldl k f (SrcLoc x y z) = k (k (k (f SrcLoc) x) y) z
  {-# INLINE gfoldl #-}

  gunfold k f _ = k (k (k (f SrcLoc)))
  {-# INLINE gunfold #-}

-- | @since 1.0.0
instance Eq SrcLoc where
  SL loc0# == SL loc1# = Bool.toBool (loc0# ==# loc1#)
  {-# INLINE (==) #-}

  SL loc0# /= SL loc1# = Bool.toBool (loc0# /=# loc1#)
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance Lift SrcLoc where
  lift (SL loc#) = fmap (TH.AppE (TH.ConE 'SL)) (lift loc#)
  {-# INLINE lift #-}

  liftTyped loc = TH.unsafeCodeCoerce (lift loc)
  {-# INLINE liftTyped #-}

-- | @since 1.0.0
instance Ord SrcLoc where
  compare (SL loc0#) (SL loc1#) = toOrdering (compare# loc0# loc1#)
  {-# INLINE compare #-}

  SL loc0# > SL loc1# = Bool.toBool (loc0# ># loc1#)
  {-# INLINE (>) #-}

  SL loc0# >= SL loc1# = Bool.toBool (loc0# >=# loc1#)
  {-# INLINE (>=) #-}

  SL loc0# < SL loc1# = Bool.toBool (loc0# <# loc1#)
  {-# INLINE (<) #-}

  SL loc0# <= SL loc1# = Bool.toBool (loc0# <=# loc1#)
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

-- | @since 1.0.0
instance Show SrcLoc where
  show (SL (SrcLoc# p# l# c#)) = "SrcLoc " ++ shows (I# p#) " " ++ shows (I# l#) " " ++ show (I# c#)
  {-# INLINE show #-}

-- Instances - Has Field -------------------------------------------------------

-- | @since 1.0.0
instance HasField "posn" SrcLoc Int where
  getField (SL (SrcLoc# x# _ _)) = I# x#
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "line" SrcLoc Int where
  getField (SL (SrcLoc# _ x# _)) = I# x#
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "coln" SrcLoc Int where
  getField (SL (SrcLoc# _ _ x#)) = I# x#
  {-# INLINE getField #-}

-- Construction ----------------------------------------------------------------

-- | Constructs a source location give:
--
--   1. The position of the source location,
--
--   2. The line number of the source location, and;
--
--   3. The column of the source location.
--
-- @since 1.0.0
pattern SrcLoc :: Int -> Int -> Int -> SrcLoc
pattern SrcLoc {posn, line, coln} = SL (SL# (# UnI# posn, UnI# line, UnI# coln #))

{-# COMPLETE SrcLoc #-}

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
feed (SL loc#) (C# chr#) = SL (Prim.feed# loc# chr#)
{-# INLINE feed #-}

-- | Take the difference of two t'SrcLoc' source positions.
--
-- @since 1.0.0
diff :: SrcLoc -> SrcLoc -> Int
diff (SrcLoc p0 _ _) (SrcLoc p1 _ _) = p1 - p0
{-# INLINE diff #-}

--------------------------------------------------------------------------------

pattern UnI# :: Int -> Int#
pattern UnI# x <-
  (I# -> x)
  where
    UnI# (I# x#) = x#

{-# COMPLETE UnI# #-}

constr :: Constr
constr = Data.mkConstrTag datatype "SrcLoc" 1 [] Data.Prefix
{-# INLINE CONLIKE constr #-}

datatype :: DataType
datatype = Data.mkDataType "SrcLoc" [constr]
{-# INLINE CONLIKE datatype #-}
