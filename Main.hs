module Main where

import AI.SVM.Simple
import qualified Data.Vector.Storable as V
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.Cairo.Internal hiding (C)

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

  let
    plot :: Diagram Cairo R2
    plot =
      (circle 1 # fc green # scale 5)
      `atop`
      (circle 1 # fc green # scale 5 # translateX 200 # translateY 200)
      `atop`
      (circle 1 # fc green # scale 5 # translateX 400 # translateY 400)
      `atop`
      foldl1 atop [
        circle 1 # scale 5 # translateX (400 * x) # translateY (400 * y) # fc (color svm (x,y))
          | x <- [0, 0.025 .. 1], y <- [0, 0.025 .. 1] ]

  fst $ renderDia Cairo (CairoOptions "test.png" (Width 400) PNG True) plot

color svm (x,y) =
  case classify svm [x,y] of
    'r' -> red
    'b' -> blue
    'i' -> indigo
