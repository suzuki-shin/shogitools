module Game.Shogi.Shogiwars (
) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List (nub, sort, isPrefixOf, isSuffixOf)

-- | 
downloadKifs :: String -> IO ()
downloadKifs user = undefined

-- | userIdを受け取ってkifファイルの URLのリストを返す
kifUrls :: String -> IO [String]
kifUrls user = do
--   c <- simpleHttp url
  c <- readFile "test/sample.html"
--   let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack c
  let doc = readString [withParseHTML yes, withWarnings no] c
  links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "wrap3") >>> css "a" ! "href"
  return $ sort $ nub $ filter (isSuffixOf ".kif") links

