module Input where

import Data.Vector (Vector)
import qualified Data.Vector as V

import DailyChart

type Input = Vector Double

type SixDailyCharts = (DailyChart, DailyChart, DailyChart, DailyChart, DailyChart, DailyChart)

fromDailyCharts :: SixDailyCharts -> (Bool, Input)
fromDailyCharts (d1,d2,d3,d4,d5,d6) = (answer, input)
  where
    input = V.fromList $ init [ fromIntegral (f d) | f <- [open, close], d <- [d1,d2,d3,d4,d5,d6] ]
    answer = close d5 < close d6


makeInputs :: Vector DailyChart -> Vector (Bool, Input)
makeInputs ds = V.map fromDailyCharts $ V.zip6 ds (V.drop 1 ds) (V.drop 2 ds) (V.drop 3 ds) (V.drop 4 ds) (V.drop 5 ds)
