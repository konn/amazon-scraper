{-# LANGUAGE RecordWildCards #-}
module Main where
import Lib

import           Control.Lens
import           Control.Monad
import           Data.List               (find)
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Network.Wreq
import           System.IO

main :: IO ()
main = do
  books <- parseBooks <$> readFile "data/bought.tsv"
  forM_ books $ \book -> do
    src <- LT.unpack . LT.decodeUtf8 . view responseBody
      <$> get (toSearchUrl book)
    case find ((== 0) . itemRank) $ parseItems src of
      Nothing       -> hPutStrLn stderr $ "Book not found: " <> show book
      Just Item{..} -> putStrLn itemASIN
