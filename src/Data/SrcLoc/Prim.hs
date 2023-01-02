{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTSyntax               #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedNewtypes         #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SrcLoc.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module provides the unboxed source location type t'SrcLoc#' and a
-- standard set of primitive operations on t'SrcLoc#'.
--
-- @since 1.0.0
module Data.SrcLoc.Prim
  ( -- * Source Locations
    SrcLoc# (SL#, SrcLoc#, posn#, line#, coln#)
    -- * Basic Operations
  , feed#
  , nextColn#
  , nextLine#
  , diff#
  ) where

import Data.Bool.Prim (Bool# (F#, T#), and#, or#)
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int
import Data.Ord.Prim (Eq# (..), Ord# (..), Ordering# (EQ#, GT#, LT#))

import GHC.Exts (Char#, Int (I#), RuntimeRep (..), TYPE)
import GHC.Exts qualified as GHC

import Language.Haskell.TH.Syntax (Exp, Lift, lift, liftTyped)
import Language.Haskell.TH.Syntax qualified as TH

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
type SrcLocRep :: RuntimeRep 
type SrcLocRep = 'TupleRep '[ 'IntRep, 'IntRep, 'IntRep ]

-- | t'SrcLoc#' is an unboxed source location.
--
-- @since 1.0.0
newtype SrcLoc# :: TYPE SrcLocRep where 
  SL# :: (# Int#, Int#, Int# #) -> SrcLoc#

-- | TODO
--
-- @since 1.0.0
pattern SrcLoc# :: Int# -> Int# -> Int# -> SrcLoc#
pattern SrcLoc# {posn#, line#, coln#} = SL# (# posn#, line#, coln# #)

{-# COMPLETE SrcLoc# #-}

-- | @since 1.0.0
instance Eq# SrcLoc# where
  SL# (# x0#, y0#, z0# #) ==# SL# (# x1#, y1#, z1# #) =
    (x0# ==# x1#) `and#` (y0# ==# y1#) `and#` (z0# ==# z1#)
  {-# INLINE (==#) #-}

  SL# (# x0#, y0#, z0# #) /=# SL# (# x1#, y1#, z1# #) =
    (x0# /=# x1#) `or#` (y0# /=# y1#) `or#` (z0# /=# z1#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Ord# SrcLoc# where
  compare# (SL# (# x0#, y0#, z0# #)) (SL# (# x1#, y1#, z1# #)) =
    case compare# x0# x1# of
      EQ# -> case compare# y0# y1# of
        EQ#  -> compare# z0# z1#
        ord# -> ord#
      ord# -> ord#
  {-# INLINE compare# #-}

  SL# (# x0#, y0#, z0# #) ># SL# (# x1#, y1#, z1# #) =
    case compare# x0# x1# of
      EQ# -> case compare# y0# y1# of
        EQ# -> z0# ># z1#
        GT# -> T#
        LT# -> F#
      GT# -> T#
      LT# -> F#
  {-# INLINE (>#) #-}

  loc0# >=# loc1# = (loc0# ==# loc1#) `or#` (loc0# ># loc1#)
  {-# INLINE (>=#) #-}

  SL# (# x0#, y0#, z0# #) <# SL# (# x1#, y1#, z1# #) =
    case compare# x0# x1# of
      EQ# -> case compare# y0# y1# of
        EQ# -> z0# <# z1#
        GT# -> F#
        LT# -> T#
      GT# -> F#
      LT# -> T#
  {-# INLINE (<#) #-}

  loc0# <=# loc1# = (loc0# <# loc1#) `or#` (loc0# ==# loc1#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Lift SrcLoc# where
  lift (SL# (# x#, y#, z# #)) =
    let conE = TH.ConE 'SL#
        tupE = TH.UnboxedTupE [quoteI# x#, quoteI# y#, quoteI# z#]
     in pure (TH.AppE conE tupE)
    where
      quoteI# :: Int# -> Maybe Exp
      quoteI# i# = Just (TH.LitE (TH.IntPrimL (toInteger (I# i#))))
  {-# INLINE lift #-}

  liftTyped loc# = TH.unsafeCodeCoerce (lift loc#)
  {-# INLINE liftTyped #-}

-- Basic Operations ------------------------------------------------------------

-- | "Feeds" a character to a t'SrcLoc#'. This produces a new t'SrcLoc#' with
-- fields incremented according to the kind character the source location was
-- fed.
--
-- * For a newline character the position and line number fields are
--   incremented. The columnn field is set to @1#@.
--
-- * For any character that is not a newline character, the 'posn#' and 'coln#'
--   fields are increment while 'line#' is left unmodified.
--
-- @since 1.0.0
feed# :: SrcLoc# -> Char# -> SrcLoc#
feed# loc# chr# =
  case '\n'# ==# chr# of
    T# -> nextLine# loc#
    F# -> nextColn# loc#

-- | Take the difference of two 'SrcLoc#' source positions.
--
-- @since 1.0.0
diff# :: SrcLoc# -> SrcLoc# -> Int#
diff# (SrcLoc# x0# _ _) (SrcLoc# x1# _ _) = x1# GHC.-# x0#
{-# INLINE diff# #-}

-- Modification ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
nextColn# :: SrcLoc# -> SrcLoc#
nextColn# (SrcLoc# p# l# c#) = SrcLoc# (Int.addInt# 1# p#) l# (Int.addInt# 1# c#)

-- | TODO
--
-- @since 1.0.0
nextLine# :: SrcLoc# -> SrcLoc#
nextLine# (SrcLoc# p# l# _) = SrcLoc# (Int.addInt# 1# p#) (Int.addInt# 1# l#) 1#
