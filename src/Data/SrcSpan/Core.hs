{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SrcSpan.Core
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'Data.SrcSpan.Core' module provides the 'SrcSpan' type.
--
-- @since 1.0.0
module Data.SrcSpan.Core
  ( -- * Source Spans
    SrcSpan (..)
    -- ** Basic Operations
  , defaultSrcSpan
  , fromSrcLoc
  ) where

import Control.DeepSeq (NFData)

import Data.Data (Data)
import Data.Default (Default (..))
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

-- SrcSpan ---------------------------------------------------------------------

-- | The 'SrcSpan' type represents a source span in a source file.
--
-- @since 1.0.0
data SrcSpan = SrcSpan
  { begin :: {-# UNPACK #-} !SrcLoc
    -- The 'SrcLoc' where the source span begins.
  , end   :: {-# UNPACK #-} !SrcLoc
    -- The 'SrcLoc' where the source span ends.
  }
  deriving (Data, Eq, Generic, Lift, NFData, Ord, Show)

-- | @since 1.0.0
instance Default SrcSpan where
  def = defaultSrcSpan
  {-# INLINE CONLIKE def #-}

-- SrcSpan - Basic Operations --------------------------------------------------

-- | The default source span.
--
-- @
-- 'defaultSrcSpan' == 'SrcSpan' 'SrcLoc.defaultSrcLoc' 'SrcLoc.defaultSrcLoc'
-- @
--
-- @since 1.0.0
defaultSrcSpan :: SrcSpan
defaultSrcSpan = fromSrcLoc SrcLoc.defaultSrcLoc
{-# INLINE CONLIKE defaultSrcSpan #-}

-- | Converts a given source location @loc@ into a source span. The resulting
-- source span will 'begin' and 'end' at the location @loc@.
--
-- @
-- 'fromSrcLoc' loc == 'SrcSpan' loc loc
-- @
--
-- @since 1.0.0
fromSrcLoc :: SrcLoc -> SrcSpan
fromSrcLoc loc = SrcSpan loc loc
{-# INLINE CONLIKE fromSrcLoc #-}