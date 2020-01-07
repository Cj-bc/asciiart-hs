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
, ToWidget(..)
) where

import Graphics.Vty.Image (Image)
import Brick.Types (Widget)
import Brick.Widgets.Core (raw)
-- | Represent Coordinate
type Coord = ( Int -- ^ X
             , Int -- ^ Y
             )

-- | Convert to 'Graphics.Vty.Image.Image'
class ToImage a where
    -- | Convert to 'Graphics.Vty.Image.Image'
    toImage :: a -> Image



-- | Convert to 'Brick.Types.Widget'
class ToImage a => ToWidget a where
    -- | Convert to 'Brick.Types.Widget'
    toWidget :: a -> Widget n
    toWidget = raw . toImage
