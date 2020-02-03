{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Data.Vector as V
import Graphics.Asciiart.Data.Raster
import Graphics.Asciiart.Type
import Graphics.Vty.Attributes

-- testRasterData {{{
testRasterData = unlines $ [ "width: 6"
                           , "data:"
                           , "  - c: a"
                           , "    attr: defAttr"
                           , "  - c: b"
                           , "    attr: defAttr"
                           , "  - c: c"
                           , "    attr: defAttr"
                           , "  - c: d"
                           , "    attr: defAttr"
                           , "  - c: e"
                           , "    attr: defAttr"
                           , "  - c: f"
                           , "    attr: defAttr"
                           , "  - c: 1"
                           , "    attr: defAttr"
                           , "  - c: 2"
                           , "    attr: defAttr"
                           , "  - c: 3"
                           , "    attr: defAttr"
                           , "  - c: 4"
                           , "    attr: defAttr"
                           , "  - c: 5"
                           , "    attr: defAttr"
                           , "  - c: 6"
                           ]
-- }}}

-- testRaster {{{
testRaster = Raster (V.fromList [('a', defAttr), ('b', defAttr), ('c', defAttr)
                                , ('d', defAttr), ('e', defAttr), ('f', defAttr)
                                , ('1', defAttr), ('2', defAttr), ('3', defAttr)
                                , ('4', defAttr), ('5', defAttr), ('6', defAttr)
                                ]) 6
-- }}}


main :: IO ()
main = hspec $ do
    describe "Graphics.Asciiart.Data.Raster" $ do
        describe "IsAsciiart instance of Raster" $ do

            describe "renderMono" $ do
                it "should output mono AA" $ do
                    renderMono testRaster `shouldBe` ["abcdef", "123456"]

            describe "fromData" $ do
                it "should parse Save-file-format and generate Raster" $ do
                    let (Raster original_txt original_w) = testRaster
                        (Raster txt w) = fromData testRasterData
                    txt `shouldBe` original_txt
                    w `shouldBe` original_w
