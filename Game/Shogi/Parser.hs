{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.String
import Data.List
import Control.Applicative ((<*>), (<$>))
import Data.Char (digitToInt)
import Codec.Binary.UTF8.String (decodeString)
import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as BS
-- import Control.Monad

pieces :: [String]
pieces = ["歩","香","桂","銀","金","角","飛","玉","と","馬","竜","龍"]

-- 板上の位置
data Pos = Pos {col :: Int, row ::  Int} | Dou | Uchi deriving (Eq)

instance Show Pos where
  show (Pos c r) = show c ++ show r
  show Dou = "同"
  show Uchi = "打"

data Action = Action {
    getPiece :: String
  , getFromPos :: Pos
  , getToPos :: Pos
  , getNari :: Bool
  } | Tohryo deriving (Eq)

instance Show Action where
  show (Action p f t n) = show t ++ p ++ if n then "成" else "" ++ "(" ++ show f ++ ")"
  show Tohryo = "投了"

data Kif = Kif {
    getHeaders :: [String]
  , getKifLines :: [KifLine]
  } deriving (Show)


data KifLine = KifLine { getNumber :: String , getAction :: Action , getTime :: Maybe String } deriving (Show, Eq)
type StartDateLine = String
type KisenLine = String
type TimeLine = String
type TeaiwariLine = String
type SenteLine = String
type GoteLine = String
type CommentLine = String

lexer  = P.makeTokenParser emptyDef
parens = P.parens lexer

toInt :: String -> Int
toInt "１" = 1
toInt "２" = 2
toInt "３" = 3
toInt "４" = 4
toInt "５" = 5
toInt "６" = 6
toInt "７" = 7
toInt "８" = 8
toInt "９" = 9
toInt "一" = 1
toInt "二" = 2
toInt "三" = 3
toInt "四" = 4
toInt "五" = 5
toInt "六" = 6
toInt "七" = 7
toInt "八" = 8
toInt "九" = 9
toInt s = read s


-- 駒
piece :: Parser String
piece = do
  nari <- string "成" <|> string ""
  p <- foldl1' (<|>)  $ map string pieces
  return $ nari ++ p

toPos :: Parser Pos
toPos = Pos <$> col <*> row
  where
    col :: Parser Int
    col = do
      c <- many1 $ oneOf "１２３４５６７８９"
      return $ toInt c
    row :: Parser Int
    row = do
      r <- many1 $ oneOf "一二三四五六七八九"
      return $ toInt r


fromPos :: Parser Pos
fromPos = do
  c:[r] <- parens $ many1 $ oneOf "123456789"
  return $ Pos (digitToInt c) (digitToInt r)

dou :: Parser Pos
dou = string "同　" >> return Dou

uchi :: Parser Pos
uchi = string "打" >> return Uchi

-- tohryo 投了 みたいなやつをparseする
tohryo :: Parser Action
tohryo = string "投了" >> return Tohryo

-- ９八玉(97)や同　桂(89)をparseする
-- action :: Parser String
action :: Parser Action
action = do
  toPos_ <- toPos <|> dou
  p <- piece
  nari <- string "成" <|> string ""
  fromPos_ <- fromPos <|> uchi
  return $ Action p fromPos_ toPos_ (nari == "成")

kifLine :: Parser KifLine
kifLine = do
  number <- many1 digit
  space
  a <- action <|> tohryo
  x <- try $ do
--     many space
--     char '('
--     many space
--     mm1 <- many1 digit
--     char ':'
--     ss1 <- many1 digit
--     char '/'
--     hh2 <- many1 digit
--     char ':'
--     mm2 <- many1 digit
--     char ':'
--     ss2 <- many1 digit
--     char ')'
--     return $ mm1 ++ ":" ++ ss1 ++ "[" ++ hh2 ++ ":" ++ mm2 ++ ":" ++ ss2 ++ "]"
    time <-  many (noneOf "\n") <|> string ""
    return time
  return $ KifLine number a (if x == "" then Nothing else Just x)

kifLines :: Parser [KifLine]
kifLines = endBy1 kifLine eol

headers :: Parser [String]
headers = endBy header eol

kif :: Parser Kif
kif = do
  hs <- try (headers)
  kls <- try (kifLines)
  return $ Kif hs kls

header :: Parser String
header = (header_ "開始日時：")
         <|>
         (header_ "棋戦：")
         <|>
         (header_ "持ち時間：")
         <|>
         (header_ "先手：")
         <|>
         (header_ "後手：")
         <|>
         (many (noneOf "1234567890\n") >>= return)

header_ :: String -> Parser String
header_ h = do
  string h
  b <- many (noneOf "\n")
  return $ h ++ b

eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"

-- main :: IO ()
-- main = do
--   c <- getContents
--   hoge c

-- main1 = hoge "85 ５八玉(68)   ( 00:04/00:04:38)"
-- main2 = hoge "8 同　歩(23)   ( 00:01/00:00:11)"
-- main3 = hoge "25 ７七銀打   ( 00:02/00:00:16)"
-- main4 = hoge "93 同　歩成(24)   ( 00:01/00:02:00)"
-- main5 = hoge "65 投了"

-- hoge c1 = do
--   case parse kifLine "(kifLine)" c1 of
--     Left err -> print err
--     Right res -> print res

parseKif s = do
  case parse kif "(kif)" s of
    Left err -> error $ show err
    Right res -> res

-- convertEncoding :: EncodingName -> EncodingName -> BS.ByteString -> BS.ByteString
convertEncoding fromEnc toEnc = decodeString . BS.unpack . convert fromEnc toEnc . BS.pack

readAndConvert :: FilePath -> IO Kif
readAndConvert filePath = do
  sjisbs <- BS.readFile filePath
  let utf8bs = convert "SJIS" "UTF-8" sjisbs
  return $ parseKif $ decodeString $ BS.unpack utf8bs
