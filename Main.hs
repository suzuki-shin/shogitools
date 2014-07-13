{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls    #-}

-- import Web.Blog.Type (Url)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Control.Monad (mzero)
import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>),(<*>))
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment (getEnv)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.ByteString.Lazy.Char8 as BL8 (unpack)
import Data.List (nub, sort, isPrefixOf, isSuffixOf)

-- baseUrl :: Url
-- baseUrl = "https://readability.com/api/content/v1/parser"
-- dbname :: Text
-- dbname = "entry.sqlite3"

-- getToken :: IO String
-- getToken = getEnv "READABILITY_API_TOKEN"

data Player = P1 | P2 deriving (Show, Read, Eq)
derivePersistField "Player"

data GameResult = P1Lose | Draw | P1Win deriving (Show, Read, Eq)
derivePersistField "GameResult"

-- type Nari = Right
-- type Normal = Left
data Piece = Fu | Kyo | Kei | Gin | Kin | Kaku | Hi | Gyoku
           | Tokin | NariKyo | NariKei | NariGin | Uma | Ryu deriving (Show, Read, Eq)
derivePersistField "Piece"

type Pos = (Int, Int)
data Reason = Resign            -- 投了
            | Timeup            -- 時間切れ
            | LostConnection    -- 接続切れ
            | OtherReason
            deriving (Show, Read)
derivePersistField "Reason"


-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- KifuInfo
--     url String Maybe
--     player1 Text Maybe
--     player2 Text Maybe
-- --     result GameResult Maybe
-- --     reason Reason Maybe
--     deriving Show
-- KifuDetail
--     kifInfoId Int
--     turn Int
-- --     player Player
-- --     piece Piece Maybe
--     pos Pos Maybe
--     lastPos Pos Maybe
-- --     reason Reason Maybe
--     second Int Maybe
--     deriving Show
-- |]

-- instance FromJSON ReaderApiResponse where
--   parseJSON (Object v) = ReaderApiResponse <$>
--                          v .: "content" <*>
--                          v .: "domain" <*>
--                          v .:? "author" <*>
--                          v .: "url" <*>
--                          v .: "short_url" <*>
--                          v .: "title" <*>
--                          v .: "excerpt" <*>
--                          v .: "direction" <*>
--                          v .: "word_count" <*>
--                          v .: "total_pages" <*>
--                          v .:? "date_published" <*>
--                          v .:? "dek" <*>
--                          v .:? "lead_image_url" <*>
--                          v .:? "next_page_id" <*>
--                          v .: "rendered_pages"
--   parseJSON _          = mzero


-- saveKifu :: [String] -> IO ()
-- saveKifu kifu = 
--   runSqlite "test.db" $ do
--     runMigration migrateAll
--     entries <- selectList ([KifuInfo ==. url]::[Filter KifuInfo]) [LimitTo 1]
--     if null entries
--       then do
--         b <- liftIO $ httpGetAndJson url
--         insert b
--         liftIO $ putStrLn $ kifuInfo b
--       else error "already saved."
--     return ()


-- httpGetAndJson :: Url -> IO ReaderApiResponse
-- httpGetAndJson entry_url = do
--   token <- getToken
--   a <- simpleHttp $ baseUrl ++ "?url=" ++ entry_url ++ "&token=" ++ token
--   let b = fromJust $ decode a :: ReaderApiResponse
--   return b

main = do
--   c <- simpleHttp url
  c <- readFile "test/sample.html"
--   let doc = readString [withParseHTML yes, withWarnings no] $ BL8.unpack c
  let doc = readString [withParseHTML yes, withWarnings no] c
  links <- runX $ doc //> css "div" >>> hasAttrValue "id" (== "wrap3") >>> css "a" ! "href"
  return $ sort $ nub $ filter (isSuffixOf ".kif") links

