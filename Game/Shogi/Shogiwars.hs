{-# LANGUAGE OverloadedStrings #-}

module Game.Shogi.Shogiwars (
) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.FilePath.Posix ((</>))
import Data.List (nub, sort, isPrefixOf, isSuffixOf)
import Network.HTTP.Conduit
import Control.Monad (forM_, mapM_)
import Data.ByteString.Lazy.Char8 as BL8 (writeFile, unpack)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import System.IO (stdout)

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


-- | 将棋ウォーズ棋譜検索ベータの検索結果ページを返す
-- kifusearchResultPage :: String -> IO String
kifusearchResultPage user = runResourceT $ do
--   manager <- liftIO $ newManager def
  manager <- liftIO $ newManager conduitManagerSettings
  req <- liftIO $ parseUrl kifuSearchUrl
  let postRequest = urlEncodedBody [("keyword1", "soradayo"),("csrfmiddlewaretoken","ubK8vuZNfZbBsBvlIOpFrDsBcRIGHtwg")] req
  response <- http postRequest manager
  responseBody response $$+- CB.sinkHandle stdout
--   responseBody response $$ CB.sinkHandle stdout

kifusearchPageInputParams :: IO [(String,String)]
kifusearchPageInputParams = do
  page <- simpleHttp kifuSearchUrl
--   let doc = readString [withParseHTML yes, withWarnings no] page
--   links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "inner1") >>> css "input" ! "value"
--   return $ map ((baseUrl ++) . dropWhile (=='.')) $ sort $ nub $ filter (isSuffixOf ".kif") links
  let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack page
--   links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "inner1") >>> css "input" ! "name" &&& css "input" ! "value"
--   links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "inner1") >>> $ css "input" ! "name" &&& css "input" ! "value"
--   links <- runX $ doc >>> css "input" ! "name" &&& css "input" ! "value"
  runX $ doc >>> css "input" >>>  (getAttrValue "name" &&& getAttrValue "value")
