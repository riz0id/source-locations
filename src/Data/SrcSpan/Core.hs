{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
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
-- TODO
--
-- @since 1.0.0
module Data.SrcSpan.Core
  ( -- * Source Spans
    SrcSpan (SrcSpan, begin, end),
  )
where

import Control.DeepSeq (NFData)

import Data.Data (Data)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

import Data.SrcLoc.Core (SrcLoc)

-- Source Spans ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data SrcSpan = SrcSpan
  { begin :: {-# UNPACK #-} !SrcLoc
  -- ^ TODO
  , end :: {-# UNPACK #-} !SrcLoc
  -- ^ TODO
  }
  deriving (Data, Eq, Generic, NFData, Ord, Show, Lift)