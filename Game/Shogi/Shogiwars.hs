module Game.Shogi.Shogiwars (
) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.FilePath.Posix ((</>))
import Data.List (nub, sort, isPrefixOf, isSuffixOf)
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad (forM_, mapM_)
import Data.ByteString.Lazy.Char8 as BL8 (writeFile)

baseUrl = "http://swks.sakura.ne.jp/wars/"
kifuSearchUrl = baseUrl </> "kifusearch/"

-- | userIdを受け取って、そのユーザーのkifファイルをダウンロードして、DBにurl等を保存する
downloadKifs :: String -> IO ()
downloadKifs user = do
  urls <- kifUrls user
  forM_ (zip urls [0..]) $ \(u, i) -> do
    kif <- simpleHttp u
    print kif
    BL8.writeFile ("k" ++ show i) kif
  return ()


-- | userIdを受け取ってkifファイルの URLのリストを返す
kifUrls :: String -> IO [String]
kifUrls user = do
--   c <- simpleHttp url
  c <- readFile "test/sample.html"
--   let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack c
  let doc = readString [withParseHTML yes, withWarnings no] c
  links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "wrap3") >>> css "a" ! "href"
  return $ map ((baseUrl ++) . dropWhile (=='.')) $ sort $ nub $ filter (isSuffixOf ".kif") links

