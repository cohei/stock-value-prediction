{-# LANGUAGE OverloadedStrings #-}
module Main where

import AI.SVM.Base
import AI.SVM.Simple
import Control.Applicative ((<$>))
import Control.Monad.Random
import Data.Foldable (Foldable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (liftM)

import DailyChart
import Input
import System.Environment (getArgs)

trainingDataFile = "225.csv"

type TrainingData = [DailyChart]
type Classifier   = Input -> Bool

toInput :: Vector DailyChart -> [Input]
toInput = undefined

teacher :: [DailyChart] -> (Input, Bool)
teacher = undefined

train :: TrainingData -> Classifier
train = undefined

tr :: (Foldable f, SVMVector a) => f (Bool, a) -> (String, SVMClassifier Bool)
tr = trainClassifier (C 1) (RBF 4)

randomClassifier :: MonadRandom m => m Classifier
randomClassifier = liftM const getRandom

main = do
  input <- V.fromList . map read . take 11 <$> getArgs

  putStrLn "random"
  randomClassifier' <- randomClassifier
  print $ randomClassifier' input

  putStrLn ""

  putStrLn "svm"
  trainingData <- fromFile trainingDataFile
  let (str, svm) = tr (makeInputs trainingData)
  putStr str
  print $ classify svm input
