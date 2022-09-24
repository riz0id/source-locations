{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
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
    SrcLoc# (SL#, SrcLoc#),
    posn#,
    line#,
    coln#,

    -- * Comparison
    gt#,
    ge#,
    eq#,
    ne#,
    lt#,
    le#,

    -- * Basic Operations
    feed#,
  )
where

import Data.Bool.Prim (Bool# (True#, False#))
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int
import Data.Char.Prim qualified as Char

import GHC.Exts (Int (I#), Char#)

import Language.Haskell.TH.Syntax (Exp, Lift, lift, liftTyped)
import Language.Haskell.TH.Syntax qualified as TH

--------------------------------------------------------------------------------

-- | t'SrcLoc#' is an unboxed source location.
--
-- @since 1.0.0
newtype SrcLoc# = 
  SL# (# Int#, Int#, Int# #)

-- | TODO
--
-- @since 1.0.0
pattern SrcLoc# :: Int# -> Int# -> Int# -> SrcLoc#
pattern SrcLoc# x# y# z# = SL# (# x#, y#, z# #)

{-# COMPLETE SrcLoc# #-}

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

-- | Obtain the source position field from a t'SrcLoc#'.
--
-- @since 1.0.0
posn# :: SrcLoc# -> Int#
posn# (SrcLoc# x# _ _) = x#

-- | Obtain the line number field from a t'SrcLoc#'.
--
-- @since 1.0.0
line# :: SrcLoc# -> Int#
line# (SrcLoc# _ x# _) = x#

-- | Obtain the column number field from a t'SrcLoc#'.
--
-- @since 1.0.0
coln# :: SrcLoc# -> Int#
coln# (SrcLoc# _ _ x#) = x#

-- Comparison ------------------------------------------------------------------

infix 4 `gt#`, `ge#`, `eq#`, `ne#`

-- | "Greater than" comparison on two t'SrcLoc#' values.
--
-- @since 1.0.0
gt# :: SrcLoc# -> SrcLoc# -> Bool#
gt# (SrcLoc# x0# y0# z0#) (SrcLoc# x1# y1# z1#) =
  let cmp0# = Int.gtInt# x0# x1#
      cmp1# = Int.gtInt# y0# y1#
      cmp2# = Int.gtInt# z0# z1#
   in cmp0# `Bool.and#` cmp1# `Bool.and#` cmp2#

-- | "Greater than or equal to" comparison on two t'SrcLoc#' values.
--
-- @since 1.0.0
ge# :: SrcLoc# -> SrcLoc# -> Bool#
ge# (SrcLoc# x0# y0# z0#) (SrcLoc# x1# y1# z1#) =
  let cmp0# = Int.geInt# x0# x1#
      cmp1# = Int.geInt# y0# y1#
      cmp2# = Int.geInt# z0# z1#
   in cmp0# `Bool.and#` cmp1# `Bool.and#` cmp2#

-- | "Equal to" comparison on two t'SrcLoc#' values.
--
-- @since 1.0.0
eq# :: SrcLoc# -> SrcLoc# -> Bool#
eq# (SrcLoc# x0# y0# z0#) (SrcLoc# x1# y1# z1#) =
  let cmp0# = Int.eqInt# x0# x1#
      cmp1# = Int.eqInt# y0# y1#
      cmp2# = Int.eqInt# z0# z1#
   in cmp0# `Bool.and#` cmp1# `Bool.and#` cmp2#

-- | "Not equal to" comparison on two t'SrcLoc#' values.
--
-- @since 1.0.0
ne# :: SrcLoc# -> SrcLoc# -> Bool#
ne# (SrcLoc# x0# y0# z0#) (SrcLoc# x1# y1# z1#) =
  let cmp0# = Int.neInt# x0# x1#
      cmp1# = Int.neInt# y0# y1#
      cmp2# = Int.neInt# z0# z1#
   in cmp0# `Bool.or#` cmp1# `Bool.or#` cmp2#

-- | "Less than" comparison on two t'SrcLoc#' values.
--
-- @since 1.0.0
lt# :: SrcLoc# -> SrcLoc# -> Bool#
lt# (SrcLoc# x0# y0# z0#) (SrcLoc# x1# y1# z1#) =
  let cmp0# = Int.ltInt# x0# x1#
      cmp1# = Int.ltInt# y0# y1#
      cmp2# = Int.ltInt# z0# z1#
   in cmp0# `Bool.and#` cmp1# `Bool.and#` cmp2#

-- | "Less than or equal to" comparison on two t'SrcLoc#' values.
--
-- @since 1.0.0
le# :: SrcLoc# -> SrcLoc# -> Bool#
le# (SrcLoc# x0# y0# z0#) (SrcLoc# x1# y1# z1#) =
  let cmp0# = Int.leInt# x0# x1#
      cmp1# = Int.leInt# y0# y1#
      cmp2# = Int.leInt# z0# z1#
   in cmp0# `Bool.and#` cmp1# `Bool.and#` cmp2#

-- Basic Operations ------------------------------------------------------------

-- | "Feeds" a character to a t'SrcLoc#'. This produces a new t'SrcLoc#' with 
-- fields incremented according to the kind character the source location was
-- fed. 
--
-- * For a newline character (i.e. @'\\n'#@ or @'\\r'#@) the 'posn#' and 'line#' 
--   fields are incremented. The 'coln#' field is set to @1#@.
--
-- * For any character that is not a newline character, the 'posn#' and 'coln#'
--   fields are increment while 'line#' is left unmodified.
--
-- @since 1.0.0
feed# :: SrcLoc# -> Char# -> SrcLoc# 
feed# (SrcLoc# p# l# c#) chr# = 
  case isNewline# chr# of 
    True# -> SrcLoc# (Int.addInt# 1# p#) (Int.addInt# 1# l#) 1#
    False# -> SrcLoc# (Int.addInt# 1# p#) l# (Int.addInt# 1# c#)

isNewline# :: Char# -> Bool# 
isNewline# chr# = Bool.or# (Char.eq# '\n'# chr#) (Char.eq# '\r'# chr#) 
{-# INLINE isNewline# #-}