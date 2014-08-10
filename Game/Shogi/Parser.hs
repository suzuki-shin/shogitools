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
pieces = ["歩","香","桂","銀","金","角","飛","玉","と","成香","成桂","成銀","馬","竜","龍"]

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

data Kif = Kif {
    getHeader :: [String]
  , getKifLines :: [KifLine]
  } deriving (Show)

data KifLine = KifLine {
    getNumber :: String
  , getAction :: Action
  , getTime :: Maybe String
  } deriving (Eq, Show)

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
piece = foldl1' (<|>)  $ map string pieces

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
  x <- try (do
          time <- many (noneOf "\n") <|> string ""
          return time
      )
  return $ KifLine number a (if x == "" then Nothing else Just x)

kifLines :: Parser [KifLine]
kifLines = endBy1 kifLine eol

headers :: Parser [String]
headers = endBy header eol

kif :: Parser Kif
kif = do
  hs <- headers <|> return []
  kls <- kifLines
  return $ Kif hs kls


header :: Parser String
header =    startDatetime
         <|> kisen
         <|> time
         <|> teaiwari
         <|> sente
         <|> gote
         <|> comment

header_ :: String -> Parser String
-- header_ h = return <$> string h <*> many (noneOf "\n")
header_ h = do
  string h
  b <- many (noneOf "\n")
  return $ h ++ b

startDatetime, kisen, time, teaiwari, sente, gote, comment :: Parser String
startDatetime = header_ "開始日時："
kisen = header_ "棋戦："
time = header_ "持ち時間："
teaiwari = header_ "手合割："
sente = header_ "先手："
gote = header_ "後手："
-- comment = try (string "手数----指手---------消費時間--")
comment = many (noneOf "1234567890\n")

eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"

main :: IO ()
main = do
  c <- getContents
  hoge c

main1 = hoge "85 ５八玉(68)   ( 00:04/00:04:38)"
main2 = hoge "8 同　歩(23)   ( 00:01/00:00:11)"
main3 = hoge "25 ７七銀打   ( 00:02/00:00:16)"
main4 = hoge "93 同　歩成(24)   ( 00:01/00:02:00)"
main5 = hoge "65 投了"

hoge c1 = do
  case parse kifLine "(kifLine)" c1 of
    Left err -> print err
    Right res -> print res

fuga s = do
  case parse kif "(kif)" s of
    Left err -> error $ show err
    Right res -> res

-- convertEncoding :: EncodingName -> EncodingName -> BS.ByteString -> BS.ByteString
convertEncoding fromEnc toEnc = decodeString . BS.unpack . convert fromEnc toEnc . BS.pack
-- convertEncoding fromEnc toEnc = decodeString . convert fromEnc toEnc

readAndConvert :: FilePath -> IO Kif
readAndConvert filePath = do
  sjisbs <- BS.readFile filePath
  let utf8bs = convert "SJIS" "UTF-8" sjisbs
  return $ fuga $ decodeString $ BS.unpack utf8bs

{--
開始日時：2014/06/30 10:05:29
棋戦：将棋ウォーズ(10切)
持ち時間：10分切れ負け
手合割：平手
先手：tooowaaa
後手：Hascurry
手数----指手---------消費時間--
1 ２六歩(27)   ( 00:00/00:00:00)
2 ８四歩(83)   ( 00:03/00:00:03)
3 ２五歩(26)   ( 00:01/00:00:01)
4 ８五歩(84)   ( 00:05/00:00:08)
5 ７八金(69)   ( 00:02/00:00:03)
6 ３二金(41)   ( 00:02/00:00:10)
7 ２四歩(25)   ( 00:02/00:00:05)
8 同　歩(23)   ( 00:01/00:00:11)
9 同　飛(28)   ( 00:00/00:00:05)
10 ２三歩打   ( 00:01/00:00:12)
11 ２六飛(24)   ( 00:01/00:00:06)
12 ７二銀(71)   ( 00:03/00:00:15)
13 ３八銀(39)   ( 00:02/00:00:08)
14 ８三銀(72)   ( 00:02/00:00:17)
15 ６八銀(79)   ( 00:00/00:00:08)
16 ８四銀(83)   ( 00:01/00:00:18)
17 ７六歩(77)   ( 00:01/00:00:09)
18 ９五銀(84)   ( 00:09/00:00:27)
19 ７七銀(68)   ( 00:04/00:00:13)
20 ８六歩(85)   ( 00:08/00:00:35)
21 同　歩(87)   ( 00:01/00:00:14)
22 同　銀(95)   ( 00:01/00:00:36)
23 同　銀(77)   ( 00:00/00:00:14)
24 同　飛(82)   ( 00:01/00:00:37)
25 ７七銀打   ( 00:02/00:00:16)
26 ８二飛(86)   ( 00:02/00:00:39)
27 ８七歩打   ( 00:01/00:00:17)
28 ４二銀(31)   ( 00:15/00:00:54)
29 ５八金(49)   ( 00:03/00:00:20)
30 ５二金(61)   ( 00:02/00:00:56)
31 ７九角(88)   ( 00:01/00:00:21)
32 ３四歩(33)   ( 00:01/00:00:57)
33 ６六歩(67)   ( 00:02/00:00:23)
34 ３三銀(42)   ( 00:04/00:01:01)
35 ９六歩(97)   ( 00:01/00:00:24)
36 ９四歩(93)   ( 00:01/00:01:02)
37 ６七金(58)   ( 00:01/00:00:25)
38 ４四歩(43)   ( 00:02/00:01:04)
39 １六歩(17)   ( 00:01/00:00:26)
40 １四歩(13)   ( 00:02/00:01:06)
41 ６八角(79)   ( 00:00/00:00:26)
42 ４三金(52)   ( 00:01/00:01:07)
43 ２八飛(26)   ( 00:01/00:00:27)
44 ３一角(22)   ( 00:03/00:01:10)
45 ５六歩(57)   ( 00:01/00:00:28)
46 ５四歩(53)   ( 00:01/00:01:11)
47 ６九玉(59)   ( 00:00/00:00:28)
48 ４二角(31)   ( 00:02/00:01:13)
49 ７九玉(69)   ( 00:01/00:00:29)
50 ４一玉(51)   ( 00:01/00:01:14)
51 ２七銀(38)   ( 00:01/00:00:30)
52 ３一玉(41)   ( 00:02/00:01:16)
53 ２六銀(27)   ( 00:00/00:00:30)
54 ２二玉(31)   ( 00:08/00:01:24)
55 ８八玉(79)   ( 00:02/00:00:32)
56 ７四歩(73)   ( 00:04/00:01:28)
57 ２五銀(26)   ( 00:02/00:00:34)
58 ６四歩(63)   ( 00:19/00:01:47)
59 ２四歩打   ( 00:01/00:00:35)
60 同　歩(23)   ( 00:01/00:01:48)
61 同　銀(25)   ( 00:01/00:00:36)
62 同　銀(33)   ( 00:01/00:01:49)
63 同　角(68)   ( 00:00/00:00:36)
64 同　角(42)   ( 00:03/00:01:52)
65 同　飛(28)   ( 00:01/00:00:37)
66 ２三銀打   ( 00:04/00:01:56)
67 ２八飛(24)   ( 00:01/00:00:38)
68 ２四歩打   ( 00:03/00:01:59)
69 ７一角打   ( 00:31/00:01:09)
70 ７二飛(82)   ( 00:27/00:02:26)
71 ８二銀打   ( 00:01/00:01:10)
72 ６二銀打   ( 00:15/00:02:41)
73 同　角成(71)   ( 00:06/00:01:16)
74 同　飛(72)   ( 00:02/00:02:43)
75 ８一銀成(82)   ( 00:02/00:01:18)
76 ９三香(91)   ( 00:08/00:02:51)
77 ３六桂打   ( 00:05/00:01:23)
78 ３三金(43)   ( 00:27/00:03:18)
79 ２五歩打   ( 00:20/00:01:43)
80 ４二角打   ( 00:30/00:03:48)
81 ２四歩(25)   ( 00:05/00:01:48)
82 同　銀(23)   ( 00:03/00:03:51)
83 同　桂(36)   ( 00:02/00:01:50)
84 同　金(33)   ( 00:02/00:03:53)
85 ２五歩打   ( 00:02/00:01:52)
86 ２三金(24)   ( 00:03/00:03:56)
87 ２四銀打   ( 00:01/00:01:53)
88 同　金(23)   ( 00:04/00:04:00)
89 同　歩(25)   ( 00:01/00:01:54)
90 １二銀打   ( 00:08/00:04:08)
91 ２三銀打   ( 00:05/00:01:59)
92 同　銀(12)   ( 00:01/00:04:09)
93 同　歩成(24)   ( 00:01/00:02:00)
94 同　金(32)   ( 00:01/00:04:10)
95 ７一銀打   ( 00:07/00:02:07)
96 ６三飛(62)   ( 00:06/00:04:16)
97 ５二金打   ( 00:06/00:02:13)
98 ３三角(42)   ( 00:07/00:04:23)
99 ６二銀成(71)   ( 00:04/00:02:17)
100 ８三飛(63)   ( 00:01/00:04:24)
101 ７一成銀(81)   ( 00:04/00:02:21)
102 ３二銀打   ( 00:31/00:04:55)
103 ５一成銀(62)   ( 00:12/00:02:33)
104 ６五歩(64)   ( 00:22/00:05:17)
105 同　歩(66)   ( 00:06/00:02:39)
106 ３九角打   ( 00:01/00:05:18)
107 ２五飛(28)   ( 00:18/00:02:57)
108 ２四歩打   ( 00:05/00:05:23)
109 ２六飛(25)   ( 00:02/00:02:59)
110 ８四角成(39)   ( 00:17/00:05:40)
111 ４一成銀(51)   ( 00:04/00:03:03)
112 ８二飛(83)   ( 00:33/00:06:13)
113 ５三金(52)   ( 00:29/00:03:32)
114 ４一銀(32)   ( 00:05/00:06:18)
115 ４三金(53)   ( 00:00/00:03:32)
116 ３二銀(41)   ( 00:11/00:06:29)
117 ３三金(43)   ( 00:02/00:03:34)
118 同　銀(32)   ( 00:01/00:06:30)
119 ６四歩(65)   ( 00:07/00:03:41)
120 ６二歩打   ( 00:11/00:06:41)
121 ２五歩打   ( 00:17/00:03:58)
122 同　歩(24)   ( 00:02/00:06:43)
123 同　飛(26)   ( 00:01/00:03:59)
124 ７三馬(84)   ( 00:25/00:07:08)
125 ９一角打   ( 00:05/00:04:04)
126 ２四歩打   ( 00:07/00:07:15)
127 ８二角成(91)   ( 00:02/00:04:06)
128 同　馬(73)   ( 00:02/00:07:17)
129 ８五飛(25)   ( 00:01/00:04:07)
130 ８四歩打   ( 00:07/00:07:24)
131 ６五飛(85)   ( 00:06/00:04:13)
132 ５三桂打   ( 00:14/00:07:38)
133 ５二飛打   ( 00:05/00:04:18)
134 ４二銀打   ( 00:18/00:07:56)
135 ６二飛成(52)   ( 00:01/00:04:19)
136 ６五桂(53)   ( 00:16/00:08:12)
137 ８二龍(62)   ( 00:05/00:04:24)
138 ７七桂成(65)   ( 00:02/00:08:14)
139 同　桂(89)   ( 00:05/00:04:29)
140 ２八飛打   ( 00:13/00:08:27)
141 ６三歩成(64)   ( 00:03/00:04:32)
142 ２九飛成(28)   ( 00:03/00:08:30)
143 ５三と(63)   ( 00:01/00:04:33)
144 ８九金打   ( 00:05/00:08:35)
145 ９七玉(88)   ( 00:02/00:04:35)
146 ６四角打   ( 00:01/00:08:36)
147 ８六角打   ( 00:07/00:04:42)
148 ８八銀打   ( 00:27/00:09:03)
149 ９八玉(97)   ( 00:06/00:04:48)
150 ９九金(89)   ( 00:06/00:09:09)
151 投了
--}
