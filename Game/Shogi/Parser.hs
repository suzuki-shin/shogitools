{-# LANGUAGE OverloadedStrings #-}

module Game.Shogi.Parser (
    module X
  , parseKif
  ) where

import Text.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.String
import Data.List
import Control.Applicative ((<*>), (<$>))
import Data.Char (digitToInt)
import Game.Shogi.Type as X

lexer  = P.makeTokenParser emptyDef
parens = P.parens lexer

-- | piece parser
-- >>> parseTest piece "歩"
-- "\27497"
-- >>> parseTest piece "成桂"
-- "\25104\26690"
piece :: Parser String
piece = do
  nari <- string "成" <|> string ""
  p <- foldl1' (<|>)  $ map string pieces
  return $ nari ++ p

-- | 移動後の位置 parser
-- >>> parseTest toPos "８六"
-- 86
toPos :: Parser Pos
toPos = toPos_ <|> dou

-- | 移動前の位置 parser
-- >>> parseTest fromPos "(32)"
-- 32
-- >>> parseTest fromPos "打"
-- 打
fromPos :: Parser Pos
fromPos = fromPos_ <|> uchi

fromPos_ :: Parser Pos
fromPos_ = do
  c:[r] <- parens $ many1 $ oneOf "123456789"
  return $ Pos (digitToInt c) (digitToInt r)

toPos_ :: Parser Pos
toPos_ = Pos <$> col <*> row
  where
    toInt_ lis = toInt <$> (many1 $ oneOf lis)
    col = toInt_ "１２３４５６７８９"
    row = toInt_ "一二三四五六七八九"

-- | "同銀"とかの"同"をparseするparser
-- >>> parseTest dou "同"
-- 同
dou :: Parser Pos
dou = do
  string "同"
  string "　" <|> string ""
  return Dou

uchi :: Parser Pos
uchi = string "打" >> return Uchi

-- tohryo 投了 みたいなやつをparseする
tohryo :: Parser Action
tohryo = string "投了" >> return Tohryo

-- | ９八玉(97)や同　桂(89)をparseする
-- >>> parseTest action "９八玉(97)"
-- 98玉(97)
-- >>> parseTest action "同　桂(89)"
-- 同桂(89)
action :: Parser Action
action = do
  toP <- toPos
  p <- piece
  nari <- string "成" <|> string ""
  fromP <- fromPos
  return $ Action p fromP toP (nari == "成")
  <|>
  tohryo

-- | 棋譜の行parser
-- >>> parseTest kifLine "20 ３八馬(29)   ( 00:03/00:00:53)"
-- KifLine {getNumber = "20", getAction = 38馬(29), getTime = Just "( 00:03/00:00:53)"}
-- >>> parseTest kifLine "31 １一角成(66)   ( 00:04/00:02:12)"
-- KifLine {getNumber = "31", getAction = 11角成(66), getTime = Just "( 00:04/00:02:12)"}
-- >>> parseTest kifLine "115 投了"
-- KifLine {getNumber = "115", getAction = 投了, getTime = Nothing}
kifLine :: Parser KifLine
kifLine = do
  number <- many1 digit
  space
  a <- action
  x <- many (noneOf "\n") <|> string ""
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

parseKif s = do
  case parse kif "(kif)" s of
    Left err -> error $ show err
    Right res -> res
