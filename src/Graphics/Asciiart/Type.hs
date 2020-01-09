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
, IsAsciiart(..)
) where

-- import Control.Monad.Trans.State
import Graphics.Vty.Image (Image)
import Brick.Types (Widget)
import Brick.Widgets.Core (raw)

-- | Represent Coordinate
type Coord = (Int, Int) -- ^ (x, y)


-- | Represent 'a' is ASCII art
class IsAsciiart a where
    -- | Import from plain text
    --
    -- this is for reading data generated by 'toData'
    --
    -- Use 'generateFrom' to generate Asciiart data from Plain text
    --
    -- prop> fromData (toData a) == a
    fromData :: [String] -> a

    -- | Export to plain text in specific style
    --
    -- This is for saving value.
    -- This won't convert to human-readable style.
    -- (i.e. metadata, Attr values will be saved as is)
    --
    -- Use 'render' to generate human-readable output.
    toData :: a -> [String]

    -- | Render to actual letters
    --
    -- This will generate human-readable output.
    --
    -- To save Asciiart data as is, please use 'toData' instead.
    -- render :: a -> [String]
    -- render = undefined

    -- | Render without attributes
    renderMono :: a -> [String]

    -- | Generate Asciiart data from Plain Text
    --
    -- To generate Asciiart data from Plain-text-ASCIIart
    generateFrom :: [String] -> a

    -- | Convert to 'Graphics.Vty.Image.Image'
    toImage :: a -> Image

    -- | Convert to 'Brick.Types.Widget'
    toWidget :: a -> Widget n
    toWidget = raw . toImage
