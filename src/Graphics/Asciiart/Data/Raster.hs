{-|
Module      : Graphics.Asciiart.Data.Raster
Description : Raster style ASCII art
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Raster Data format is:

@
Asciiart Raster data
<Char><separator><Attr>;...
<Char><separator><Attr>;...
...
@

where '<char>' is one character to show and
'<Attr>' is the attributes to apply (e.g. background color,
foreground color, bold, italic, etc.)
'<separator>' is one character.
It'll just ignored.

No space are allowed around <separator> and ';'.
If space exists before <separator> or after ';',
it'll be recognized as '<Char>'
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

instance IsAsciiart Raster where
    fromData rows = Raster (V.fromList $ mconcat parsedRows) maxWidth
      where
        parsedRows :: [[(Char, Attr)]]
        parsedRows  = map parseRow rows

        maxWidth    = maximum $ map length parsedRows

        -- | Parse data format and generate (Char, Attr) pair
        parseRow :: String -> [(Char, Attr)]
        parseRow xs  = map createSet (splitAtChar ';' xs)

        createSet :: String -> (Char, Attr)
        createSet (c:_:attr) = (c, (read attr :: Attr))

        -- | Split string at given Char
        splitAtChar :: Char -> String -> [String]
        splitAtChar c (x:[]) | x == c    = [[x]]
                             | otherwise = []
        splitAtChar c (x:xs) | x == c    = splitAtChar c xs
                             | otherwise = let (h:t) = splitAtChar c xs
                                           in (x : h) : t

    toData (Raster txt w) = "Asciiart Raster data" : map encodeRow rows
      where
          rows = chunksOf w (V.toList txt)
          encodeRow = mconcat . map encodePair
          encodePair (c, attr) = c : ',' : show attr ++ ";"

    renderMono (Raster txt w) = map (map pickChar) rows
        where
            rows = chunksOf w (V.toList txt)
            pickChar (c, _) = c

    generateFrom rows = Raster (V.fromList $ mconcat $ map genRow rows) maxWidth
        where
            maxWidth = maximum $ map length rows
            genRow   = map (\c -> (c, defAttr))


    toImage (Raster txt w)= foldl1 (<->) rows
        where
            chars = map (\(c, a) -> char a c) $ V.toList txt
            rows  = map mkrow $ chunksOf w chars
            mkrow = foldl1 (<|>)
