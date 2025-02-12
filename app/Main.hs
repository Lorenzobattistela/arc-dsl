{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Type
import Functions

import System.Directory (listDirectory)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Example = Example
  { input  :: Grid
  , output :: Grid
  } deriving (Show, Eq, Generic)

instance FromJSON Example
instance ToJSON Example

type Task = [Example]
type Dataset = Map.Map String Task

getData :: Bool -> IO (Map.Map String (Map.Map String Task))
getData train = do
  let path = "../ARC-AGI/data/" ++ if train then "training" else "evaluation"
  files <- listDirectory path
  rawData <- traverse (\fn -> do
                          content <- decodeFileStrict (path ++ "/" ++ fn) :: IO (Maybe (Map.Map String [Example]))
                          pure (takeWhile (/= '.') fn, content)) files
  let parsedData = Map.fromList [(k, fromMaybe [] $ do
                                      v <- d
                                      examples <- Map.lookup (if train then "train" else "test") v
                                      pure examples)
                                | (k, d) <- rawData]
  pure $ Map.singleton (if train then "train" else "test") parsedData

solve :: String -> (Grid -> Grid) -> IO ()
solve taskName solvingFunction = do
  trainData <- getData True
  case Map.lookup "train" trainData >>= Map.lookup taskName of
    Just task -> do
      putStrLn $ "Solving task " ++ taskName ++ ":"
      mapM_ (\(i, ex) -> do
               let solvedOutput = solvingFunction (input ex)
               putStrLn $ "Example " ++ show i ++ ":"
               putStrLn "Input:"
               mapM_ print (input ex)
               putStrLn "Expected Output:"
               mapM_ print (output ex)
               putStrLn "Solved Output:"
               mapM_ print solvedOutput
               putStrLn $ "Correct: " ++ show (solvedOutput == output ex)
               putStrLn ""
            ) (zip [1..] task)
    Nothing -> putStrLn $ "Task " ++ taskName ++ " not found."

solve_a416b8f3 :: Grid -> Grid
solve_a416b8f3 inputGrid = hconcat inputGrid inputGrid

main :: IO ()
main = do
  -- let taskName = "a416b8f3"
  -- solve taskName solve_a416b8f3
  putStrLn "compiled"


