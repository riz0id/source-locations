{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.SrcLoc.Lens
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the lenses on the 'SrcLoc' type. This module is only
-- avaliable when the "source-locations" package has been compile with the 
-- @-flens@ cabal flag enabled.
--
-- @since 1.0.0
module Data.SrcLoc.Lens
  ( -- * Lenses 
    posn,
    line,
    coln,
  )
where

import Lens.Micro (Lens', lens)

--------------------------------------------------------------------------------

import Data.SrcLoc.Core (SrcLoc (SrcLoc))

--------------------------------------------------------------------------------

-- | Lens focusing on the source position of a 'SrcLoc'.
--
-- @since 1.0.0
posn :: Lens' SrcLoc Int
posn = lens (\(SrcLoc x _ _) -> x) (\(SrcLoc _ y z) x -> SrcLoc x y z)
{-# INLINE CONLIKE posn #-}

-- | Lens focusing on the line number of a 'SrcLoc'.
--
-- @since 1.0.0
line :: Lens' SrcLoc Int
line = lens (\(SrcLoc _ x _) -> x) (\(SrcLoc x _ z) y -> SrcLoc x y z)
{-# INLINE CONLIKE line #-}

-- | Lens focusing on the column number of a 'SrcLoc'.
--
-- @since 1.0.0
coln :: Lens' SrcLoc Int
coln = lens (\(SrcLoc _ _ x) -> x) (\(SrcLoc x y _) z -> SrcLoc x y z)
{-# INLINE CONLIKE coln #-}