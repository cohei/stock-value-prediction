{-# LANGUAGE OverloadedStrings #-}
module DailyChart
       ( fromFile
       , DailyChart(..)
       ) where

import           Control.Applicative       ((<$>), (<*>))
import qualified Data.ByteString.Lazy as L (readFile)
import           Data.Csv                  (FromNamedRecord(..), decodeByName, (.:))
import           Data.Time.Calendar        (Day)
import           Data.Time.Format          (readTime)
import           Data.Vector               (Vector)
import           System.Locale             (defaultTimeLocale)

data DailyChart = DailyChart { date :: Day, open, close :: Int } deriving Show

instance FromNamedRecord DailyChart where
  parseNamedRecord m =
    dailyChart <$> m .: "date" <*> m .: "open" <*> m .: "close"
    where
      dailyChart d o c = DailyChart (readDate d) (readPrice o) (readPrice c)

readDate :: String -> Day
readDate = readTime defaultTimeLocale "%-m/%-d/%y"

readPrice :: String -> Int
readPrice = read . filter (','/=)

fromFile :: FilePath -> IO (Vector DailyChart)
fromFile path = do
  file <- L.readFile path
  case decodeByName file of
   Left e -> error e
   Right (_header, v) -> return v
