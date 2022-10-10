{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SrcLoc.Core
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
    SrcLoc (SrcLoc, posn, line, coln),
    box,
    unbox,
  )
where

import Control.DeepSeq (NFData)

import Data.Bool.Prim qualified as Bool
import Data.Data (Data)
import Data.Ord.Prim (Eq# (..), Ord# (..), toOrdering)

import GHC.Exts (Int (I#))
import GHC.Generics (Generic)

import Text.Printf (PrintfArg, formatArg)
import Text.Printf qualified as Text

--------------------------------------------------------------------------------

import Data.SrcLoc.Prim (SrcLoc# (SrcLoc#))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data SrcLoc = SrcLoc
  { posn :: {-# UNPACK #-} !Int
  -- ^ TODO
  , line :: {-# UNPACK #-} !Int
  -- ^ TODO
  , coln :: {-# UNPACK #-} !Int
  -- ^ TODO
  }
  deriving (Data, Generic, NFData, Show)

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