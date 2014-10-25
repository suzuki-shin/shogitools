module Game.Shogi.ParserSpec where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
-- import Game.Shogi.Parser

spec :: Spec
spec = do
  describe "hoge" $ do
    it "return hoge" $ do
      "hoge" `shouldBe` "hoge"
--   describe "piece" $ do
--     it "piece 歩" $ do
--       parseTest "歩" `shouldBe` (Right "歩")
--   describe "SimpleHtmlParser" $ do
--       context "with one tag" $ do
--           it "parses" $ do
--               parseTest "<html></html>" `shouldBe` (Right $ Tag "html" "" [])
