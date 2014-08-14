module Main where

import AI.SVM.Simple
import Control.Monad.Random
import Data.Time.Format (readTime)
import Data.Time.Calendar (Day)
import qualified Data.Vector.Storable as V
import System.Locale (defaultTimeLocale)
import Data.CSV.Conduit
import Control.Applicative ((<$>))

learningData = "225.csv"

data DailyChart   = DailyChart { date :: Day, open, close :: Int }
type TrainingData = [DailyChart]
type Input        = (DailyChart, DailyChart, DailyChart, DailyChart, DailyChart, Int)
type Identifier   = Input -> Bool

learn :: TrainingData -> Identifier
learn = undefined

randomIdentifier :: IO Identifier
randomIdentifier = const <$> getRandom

readDate :: String -> Day
readDate = readTime defaultTimeLocale "%-m/%-d/%y"

main = do
  let
    trainingData =
      [ ('r', V.fromList [0,0])
      , ('r', V.fromList [1,1])
      , ('b', V.fromList [0,1])
      , ('b', V.fromList [1,0])
      , ('i', V.fromList [0.5,0.5::Double])
      ]
    (m, svm) = trainClassifier (C 1) (RBF 4) trainingData

  print $ classify svm [0.2, 0.8::Double]
