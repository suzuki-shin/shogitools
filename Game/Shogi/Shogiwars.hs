{-# LANGUAGE OverloadedStrings #-}

module Game.Shogi.Shogiwars (
) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.FilePath.Posix ((</>))
import Data.List (nub, sort, isPrefixOf, isSuffixOf)
import Network.HTTP.Conduit
import Control.Monad (forM_, mapM_)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 as BL8 (writeFile, unpack, pack)
import Data.ByteString.Char8 as B8 (writeFile, unpack, pack)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import System.IO (stdout)
import Data.Maybe (fromJust)
import Data.Time.Clock
import Data.Time.Calendar

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
  kifusearchResultPage user
  c <- readFile $ "DL" </> "result.html"
--   let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack c
  let doc = readString [withParseHTML yes, withWarnings no] c
  print c
  links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "wrap3") >>> css "a" ! "href"
  return $ map ((baseUrl ++) . dropWhile (=='.')) $ sort $ nub $ filter (isSuffixOf ".kif") links


-- | 将棋ウォーズ棋譜検索ベータの検索結果ページを返す
-- kifusearchResultPage :: String -> IO String
kifusearchResultPage user = runResourceT $ do
--   manager <- liftIO $ newManager def
  manager <- liftIO $ newManager conduitManagerSettings
  req' <- liftIO $ parseUrl kifuSearchUrl
--   let postRequest = urlEncodedBody [("keyword1", "soradayo"),("csrfmiddlewaretoken","ubK8vuZNfZbBsBvlIOpFrDsBcRIGHtwg")] req
  params <- liftIO $ kifusearchPageInputParams
  liftIO $ print params
  let params2 = map ((\(a,b) -> (B8.pack a, B8.pack b)) . (\(k,v) -> if k == "name1" then (k,user) else (k,v))) $ params
      req = req' { cookieJar = Just $ createCookieJar [cookie params2] }
      postRequest = urlEncodedBody params2 req
--   let params2 = map (\(k,v) -> if k == "name1" then (k,user) else (k,v)) params
--       req = req' { cookieJar = Just $ createCookieJar [cookie params] }
--       postRequest = urlEncodedBody (map (\(a,b) -> (B8.pack a, B8.pack b)) params2) req
  liftIO $ print params2
  response <- http postRequest manager
  responseBody response $$+- CB.sinkFile $ "DL" </> "result.html"
--   responseBody response $$ CB.sinkHandle stdout
  where
    cookie_value params = fromJust $ lookup "csrfmiddlewaretoken" params
--     cookie :: Cookie
    cookie params = Cookie { cookie_name = "csrftoken"
                     , cookie_value = cookie_value params
                     , cookie_path = "/"
                 , cookie_expiry_time = future
                 , cookie_domain = "swks.sakura.ne.jp"
                 , cookie_creation_time = past
                 , cookie_last_access_time = past
                 , cookie_persistent = False
                 , cookie_host_only = False
                 , cookie_secure_only = False
                 , cookie_http_only = False
                 }

past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

kifusearchPageInputParams :: IO [(String,String)]
kifusearchPageInputParams = do
  page <- simpleHttp kifuSearchUrl
  let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack page
  runX $ doc >>> css "input" >>>  (getAttrValue "name" &&& getAttrValue "value")
