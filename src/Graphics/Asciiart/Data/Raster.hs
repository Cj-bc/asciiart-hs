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
import qualified Data.Vector as V
import Data.List.Split (chunksOf)
import Graphics.Vty
import Graphics.Asciiart.Type

-- | Raster Style Ascii art
data Raster = Raster { displayText :: V.Vector (Char, Attr) -- ^ Vector of data
                     , width       :: Int -- ^ Width of the ASCII art
                     }

instance ToImage Raster where
    toImage (Raster txt w)= foldl1 (<->) rows
        where
            chars = map (\(c, a) -> char a c) $ V.toList txt
            rows  = map mkrow $ chunksOf w chars
            mkrow = foldl1 (<|>)
