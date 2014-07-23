{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import qualified Data.Text as T
import System.Directory (getDirectoryContents)
import Data.List
import qualified System.FilePath as FP ((</>))
default (T.Text)

main :: IO ()
main = do
  files <- getDirectoryContents "DL"
  print files
  mapM_ (shelly . (`copy` "/tmp") . T.pack . ("DL" FP.</>)) $ onlyKif files
  where
    onlyKif = filter (isPrefixOf "Hascurry")

copy :: T.Text -> T.Text -> Sh ()
copy from to = cp (fromText from) (fromText to)
