{-|
Module      : Graphics.Asciiart.Type
Description : Common type collections for Asciiart
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module provides common data types and classes for 'Graphics.Asciiart'
-}
module Graphics.Asciiart.Type
( -- * Common data types
  Coord
  -- * Classes
, ToImage(..)
) where

import Graphics.Vty.Image (Image)
-- | Represent Coordinate
type Coord = ( Int -- ^ X
             , Int -- ^ Y
             )

-- | Convert to 'Graphics.Vty.Image.Image'
class ToImage a where
    -- | Convert to 'Graphics.Vty.Image.Image'
    toImage :: a -> Image



