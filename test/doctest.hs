module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Game/Shogi/Parser.hs"]
