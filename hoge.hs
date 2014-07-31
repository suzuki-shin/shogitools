{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import qualified Data.Text as T
import System.Directory (getDirectoryContents)
import Data.List
import qualified System.FilePath as FP ((</>), FilePath)
default (T.Text)

copyKif :: FP.FilePath -> FP.FilePath -> IO ()
copyKif from to = do
  files <- getDirectoryContents "DL"
  print files
  mapM_ (shelly . (`copy` to) . T.pack . ("DL" FP.</>)) $ onlyKif files
  where
    onlyKif = filter (isSuffixOf ".kif")

copy :: T.Text -> T.Text -> Sh ()
copy from to = cp (fromText from) (fromText to)
