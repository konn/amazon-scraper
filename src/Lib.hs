{-# LANGUAGE RecordWildCards #-}
module Lib
    ( parseItems
    , toSearchUrl
    , Book(..)
    , Item(..)
    , parseBooks
    ) where
import Control.Arrow
import Control.Arrow.ListArrow
import Data.List.Split
import Text.HandsomeSoup
import Text.XML.HXT.Core

data Item  = Item { itemASIN :: String
                  , itemRank :: Int
                  }
            deriving (Read, Show, Eq, Ord)

data Book = Book { author :: String
                 , title  :: String
                 }
            deriving (Read, Show, Eq, Ord)

parseItems :: String -> [Item]
parseItems = runLA $
  hread    >>>
  css "li" >>> hasAttr "data-asin"
           >>> hasAttr "data-result-rank"
           >>> getAttrValue "data-asin" &&& getAttrValue "data-result-rank"
           >>> second (arr read)
           >>> arr (uncurry Item)

toSearchUrl :: Book -> String
toSearchUrl Book{..} =
  let keyword = escapeURI $ unwords [title, author]
  in mconcat [ "https://www.amazon.co.jp/s/?_encoding=UTF-8&field-keywords="
             , keyword
             ]

parseBooks :: String -> [Book]
parseBooks =
  map ((flip Book <$> head <*> (!! 1)) . splitOn "\t") . lines
