{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
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
-- TODO
--
-- @since 1.0.0
module Data.SrcLoc.Core
  ( -- * Source Locations
    SrcLoc (SL, SrcLoc),
  )
where

import Data.Bool.Prim qualified as Bool
import Data.Data (Constr, Data, DataType)
import Data.Data qualified as Data
import Data.Int.Prim (Int#)

import GHC.Exts (Int (I#))
import GHC.Records (HasField, getField)

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift, lift, liftTyped)

import Text.Printf (PrintfArg, formatArg)
import Text.Printf qualified as Text

--------------------------------------------------------------------------------

import Data.SrcLoc.Prim (SrcLoc# (SL#))
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

-- | The source location constructor.
pattern SrcLoc ::
  -- | The position of the source location.
  Int ->
  -- | The line number of the source location.
  Int ->
  -- | The column of the source location.
  Int ->
  SrcLoc
pattern SrcLoc posn line coln =
  SL (SL# (# UnI# posn, UnI# line, UnI# coln #))

{-# COMPLETE SrcLoc #-}

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
  SL l0# == SL l1# = Bool.toBool (Prim.eq# l0# l1#)
  {-# INLINE (==) #-}

  SL l0# /= SL l1# = Bool.toBool (Prim.ne# l0# l1#)
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance Lift SrcLoc where
  lift (SL loc#) = fmap (TH.AppE (TH.ConE 'SL)) (lift loc#)
  {-# INLINE lift #-}

  liftTyped loc = TH.unsafeCodeCoerce (lift loc)
  {-# INLINE liftTyped #-}

-- | @since 1.0.0
instance Ord SrcLoc where
  SL l0# > SL l1# = Bool.toBool (Prim.gt# l0# l1#)
  {-# INLINE (>) #-}

  SL l0# >= SL l1# = Bool.toBool (Prim.ge# l0# l1#)
  {-# INLINE (>=) #-}

  SL l0# < SL l1# = Bool.toBool (Prim.lt# l0# l1#)
  {-# INLINE (<) #-}

  SL l0# <= SL l1# = Bool.toBool (Prim.le# l0# l1#)
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
  show (SrcLoc p l c) = "SrcLoc " ++ shows p " " ++ shows l " " ++ show c
  {-# INLINE show #-}

-- Instances - Has Field -------------------------------------------------------

-- | 'HasField' instance selecting the 'Data.SrcLoc.posn' field getter.
--
-- @since 1.0.0
instance HasField "posn" SrcLoc Int where
  getField (SrcLoc x _ _) = x
  {-# INLINE getField #-}

-- | 'HasField' instance selecting the 'Data.SrcLoc.line' field getter.
--
-- @since 1.0.0
instance HasField "line" SrcLoc Int where
  getField (SrcLoc _ x _) = x
  {-# INLINE getField #-}

-- | 'HasField' instance selecting the 'Data.SrcLoc.coln' field getter.
--
-- @since 1.0.0
instance HasField "coln" SrcLoc Int where
  getField (SrcLoc _ _ x) = x
  {-# INLINE getField #-}

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
