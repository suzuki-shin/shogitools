{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls    #-}

module Game.Shogi.Shogiwars (
   downloadKifFilesAndSaveKifuInfo
 , makeKifuListJs
) where

import Data.Text (Text)
import Data.Text as T (pack)
import Data.Text.IO as T (writeFile)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.FilePath.Posix ((</>), takeBaseName, takeFileName)
import Data.List (nub, sort, isPrefixOf, isSuffixOf)
import Network.HTTP.Conduit
import Control.Monad (forM_, mapM_)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 as BL8 (writeFile, unpack, pack)
import Data.ByteString.Char8 as B8 (writeFile, unpack, pack, ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import System.IO (stdout)
import Data.Maybe (fromJust)
import Data.Time.Clock
import Data.Time.Calendar
import Network.URI
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.Process (readProcess)
import Control.Exception as E
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

baseUrl :: String
baseUrl = "http://swks.sakura.ne.jp/wars"
kifuSearchUrl :: String
kifuSearchUrl = baseUrl </> "kifusearch"
downloadDir :: FilePath
downloadDir = "DL"
htmlDir :: FilePath
htmlDir = "public_html"
resultPageFile :: FilePath
resultPageFile = downloadDir </> "result.html"
dbname :: String
dbname = "test.db"

type Pos = (Int, Int)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
KifuInfo
  url String Maybe
  player1 Text Maybe
  player2 Text Maybe
  player1Class Int Maybe
  player2Class Int Maybe
  winner Text Maybe
  datetime UTCTime Maybe
  deriving Show
KifuDetail
  kifInfoId Int
  turn Int
--     player Player
--     piece Piece Maybe
  pos Pos Maybe
  lastPos Pos Maybe
--     reason Reason Maybe
  second Int Maybe
  deriving Show
|]


-- | userIdを受け取って、そのユーザーのkifファイルをダウンロードして、DBにurl等を保存する
downloadKifFilesAndSaveKifuInfo :: String -> String -> IO ()
downloadKifFilesAndSaveKifuInfo user gtype = do
  downloadResultPage user gtype
  urls <- kifURIs resultPageFile
  forM_ urls $ \u -> do
    let url = show u
    kif <- simpleHttp url
    saveKifuInfo (KifuInfo (Just url) Nothing Nothing Nothing Nothing Nothing Nothing) `catch` ignore
    BL8.writeFile ("DL" </> takeFileName url) kif
  return ()
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()

-- | DBにKifuInfoを保存する（すでに存在する場合はエラーを返す）
saveKifuInfo :: KifuInfo -> IO ()
saveKifuInfo kInfo = runSqlite "test.db" $ do
    runMigration migrateAll
    entries <- selectList ([KifuInfoUrl ==. kifuInfoUrl kInfo]::[Filter KifuInfo]) [LimitTo 1]
    if null entries
      then do
        insert kInfo
        liftIO $ print kInfo
      else error "already saved."
    return ()



-- | 検索結果ページのhtmlファイルを受け取ってKifuInfoのリストを返す(未完成）
kifuInfoFromResultPage :: FilePath -> IO [String]
kifuInfoFromResultPage file = do
  let wrap3 = readDocument [withParseHTML yes, withWarnings no] file //> css "div" >>> hasAttrValue "id" (== "wrap3")
  links <- runX (wrap3
                 //>
                 (hasAttrValue "href" (isSuffixOf ".kif") ! "href")
                 >>.
                 map ((baseUrl ++) . dropWhile (=='.'))
    )
  return links


-- | 検索結果ページのhtmlファイル名を受け取ってkifファイルの URLのリストを返す
kifURIs :: String -> IO [URI]
kifURIs file = do
  c <- readFile file
  let doc = readString [withParseHTML yes, withWarnings no] c
  links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "wrap3") >>> css "a" ! "href"
  return $ linksToURIs links
  where
    filepathToURI :: String -> URI
    filepathToURI = fromJust . parseURI . (baseUrl ++) . dropWhile (=='.')
    linksToURIs :: [String] -> [URI]
    linksToURIs = (map filepathToURI) . sort . nub . (filter (isSuffixOf ".kif"))

-- 将棋ウォーズ棋譜検索ベータの検索結果ページをDLする
downloadResultPage :: String -> String -> IO ()
downloadResultPage user gtype = runResourceT $ do
  manager <- liftIO $ newManager conduitManagerSettings
  req' <- liftIO $ parseUrl kifuSearchUrl
  params <- liftIO $ kifusearchPageInputParams
  liftIO $ print params
  let params2 :: [(B8.ByteString, B8.ByteString)]
      params2 = [("name1", B8.pack user), ("csrfmiddlewaretoken", B8.pack $ fromJust $ lookup "csrfmiddlewaretoken" params), ("gtype", B8.pack gtype)]
      req = req' { cookieJar = Just $ createCookieJar [cookie (fromJust $ lookup "csrfmiddlewaretoken" params2)] }
      postRequest = urlEncodedBody params2 req
  liftIO $ print params2
  response <- http postRequest manager
  responseBody response $$+- CB.sinkFile resultPageFile
  where
--     cookie :: Cookie
--     cookie params = Cookie {
    cookie value = Cookie {
        cookie_name = "csrftoken"
      , cookie_value = value
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

-- | 棋譜検索ページのinputパラメタを返す
kifusearchPageInputParams :: IO [(String,String)]
kifusearchPageInputParams = do
  page <- simpleHttp kifuSearchUrl
  let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack page
  runX $ doc >>> css "input" >>>  (getAttrValue "name" &&& getAttrValue "value")


-- | 棋譜リストjsを作る
makeKifuListJs :: FilePath -> FilePath -> IO ()
makeKifuListJs outputPath kifuDir = do
  kifulist <- filter (`notElem` [".",".."]) <$> getDirectoryContents kifuDir
  T.writeFile outputPath $ T.pack $ "kifulist = " ++ show kifulist

sample user gtype = do
--   downloadKifFilesAndSaveKifuInfo user gtype -- 棋譜をダウンロードして、kifInfoをDBに保存
  s <- copyToDropbox  -- 保存した棋譜をDropboxへコピー
  print s
  makeKifuListJs kifulistJsPath dropboxKifuPath
  where
    dropboxKifuPath = "/Users/sshin/Dropbox/Public/shogi/kifu/"
    kifulistJsPath = htmlDir </> "kifulist.js"
    copyToDropbox = do
      currentDir <- getCurrentDirectory
      readProcess "/bin/cp" [currentDir </> downloadDir </> "*.kif", dropboxKifuPath] []
