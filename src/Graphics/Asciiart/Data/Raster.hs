{-|
Module      : Graphics.Asciiart.Data.Raster
Description : Raster style ASCII art
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

Raster Data format is:

@
exif:
  title: <title>
  author: <author>
  createdAt: YYYY-MM-DDThh:mm:ss<TIMEZONE>
width: <width>
data:
 - c: <char>
   attr: <Attr>
 - c: <char>
   attr: <Attr>
 ...
@

where '<char>' is one character to show and

'<Attr>' is the attributes to apply (e.g. background color,
foreground color, bold, italic, etc.)

`exif` is optional. Each of child dictionary of `exif` is
also optional(i.e. only title is allowed)

Exif data is still not implemented.
-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Asciiart.Data.Raster
(
  Raster
) where
import qualified Data.Vector as V
import Data.Yaml
import Data.List.Split (chunksOf)
import Data.HashMap.Lazy ((!))
import Graphics.Vty
import Graphics.Asciiart.Type
import Graphics.Asciiart.Data.Raster.Internal

-- | Raster Style Ascii art
data Raster = Raster { displayText :: V.Vector (Char, Attr) -- ^ Vector of data
                     , width       :: Int -- ^ Width of the ASCII art
                     }

instance FromJSON Raster where
    parseJSON (Object v) = Raster <$> parseData (v ! "data")
                                    <*> v .: "width"
        where
            parseData = withArray "data" $ \a -> sequence $ V.map parseChar a
            parseChar (Object v) = (,) <$> v .: "c" <*> (read <$> v .: "attr")


instance ToJSON Raster where
    toJSON (Raster txt w) = object ["width" .= w
                                   , ("data", encTxt txt)
                                   ]
        where
            encTxt :: V.Vector (Char, Attr) -> Value
            encTxt = toJSON . V.map (\(c, a) -> object ["c" .= c, "attr" .= show a])
-- }}}

instance IsAsciiart Raster where
    fromData yaml = case decodeEither' yaml of
                        (Right a) -> Just a
                        (Left _)  -> Nothing

    toData = encode

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


instance Scalable Raster where
    scaleX amount center (Raster txt w) = Raster newTxt newWidth
       where
           newWidth = round $ (toRational w) * (toRational amount)
           newTxt = V.fromList $ concat newRows
           oldRows = chunksOf w (V.toList txt)
           newRows = map (addPadding (newWidth - w)) oldRows

