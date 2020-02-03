{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Data.Vector as V
import Data.Maybe (isNothing)
import Data.Either (isRight)
import Data.ByteString (ByteString, append)
import Data.Yaml (encode, decodeEither', ParseException)
import Graphics.Asciiart.Data.Raster
import Graphics.Asciiart.Type
import Graphics.Vty.Attributes

-- testRasterData {{{
defAttrTxt = "- attr: Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default,\n    attrURL = Default}"
testRasterData :: ByteString
testRasterData = mconcat $ map (flip append "\n")
                         $ [ "data:"
                           , defAttrTxt
                           , "  c: a"
                           , defAttrTxt
                           , "  c: b"
                           , defAttrTxt
                           , "  c: c"
                           , defAttrTxt
                           , "  c: d"
                           , defAttrTxt
                           , "  c: e"
                           , defAttrTxt
                           , "  c: f"
                           , defAttrTxt
                           , "  c: '1'"
                           , defAttrTxt
                           , "  c: '2'"
                           , defAttrTxt
                           , "  c: '3'"
                           , defAttrTxt
                           , "  c: '4'"
                           , defAttrTxt
                           , "  c: '5'"
                           , defAttrTxt
                           , "  c: '6'"
                           , "width: 6"
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
        describe "FromJSON" $ do
            it "should parse correct YAML file safely" $ do
                isRight (decodeEither' testRasterData :: Either ParseException Raster)
                    `shouldBe` True

        describe "ToJSON" $ do
            it "should encode Raster correctly" $ do
                encode testRaster `shouldBe` testRasterData


        describe "IsAsciiart instance of Raster" $ do

            describe "renderMono" $ do
                it "should output mono AA" $ do
                    renderMono testRaster `shouldBe` ["abcdef", "123456"]

            describe "fromData" $ do
                it "should parse correct YAML file safely" $ do
                    not (isNothing (fromData testRasterData :: Maybe Raster))
                      `shouldBe` True

                it "should parse Save-file-format and generate Raster" $ do
                    let (Raster original_txt original_w) = testRaster
                        (Raster txt w) = fromData testRasterData
                    txt `shouldBe` original_txt
                    w `shouldBe` original_w
