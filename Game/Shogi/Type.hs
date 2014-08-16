{-# LANGUAGE OverloadedStrings #-}

module Game.Shogi.Type where

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
  show (Action p f t n) = show t ++ p ++ (if n then "成" else "") ++ "(" ++ show f ++ ")"
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
