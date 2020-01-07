{-|
Module      : Graphics.Asciiart.Data.Raster
Description : Raster style ASCII art
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental


-}
module Graphics.Asciiart.Data.Raster
(

) where
import Data.Vector
import Graphics.Vty


-- | Raster Style Ascii art
data Raster = Raster { displayText :: Vector (Char, Attr) -- ^ Vector of data
                     , width       :: Int -- ^ Width of the ASCII art
                     }

