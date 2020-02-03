{-|
Module      : Graphics.Asciiart.Data.Raster.Internal
Description : Internal use codes for Raster
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental
-}
module Graphics.Asciiart.Data.Raster.Internal where

import Control.Arrow ((***))
import Graphics.Vty.Attributes (Attr, defAttr)


-- For Scalable {{{
-- Add padding based on scaling
--
-- I use String instead of [(Char, Attr)], for example.
--
-- >> addPadding 5 "_,.-~=\"'\"=~-.._"
-- "_,.-~ = \" ' \" =~-.._"
--
-- >> addPadding 4 "_,.-~=\"'\"=~-.._"
-- "_,.-~= \" ' \" =~-.._"
addPadding :: Int -> [(Char, Attr)] -> [(Char, Attr)]
addPadding padAmount original =
     let _halfLength xs = round $ (toRational $ length xs) / 2
         splitHalf xs = splitAt (_halfLength xs) xs
         halfAmountOfPadding = round $ (toRational padAmount) / 2

         -- Insert blank character at one character interval
         --
         -- >> addPadFromFront 2 "abcdef"
         -- "a b cdef"
         addPadFromFront _ [] = []
         addPadFromFront i (head':rest)
             | i < 0     = (head':rest)
             | otherwise = head' : [(' ', defAttr)] ++ (addPadFromFront (i-1) rest)

         -- Almost the same as 'addPadFromFront', but insert from last
         --
         -- >> addPadFromLast 2 "abcdef"
         -- "abcd e f"
         addPadFromLast i = reverse . addPadFromFront i . reverse
         concatTuple (a, b) = a ++ b

     in concatTuple . (addPadFromLast halfAmountOfPadding *** addPadFromFront halfAmountOfPadding)
             $ splitHalf original


-- }}}

