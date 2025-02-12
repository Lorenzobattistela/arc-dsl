{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Type

import Prelude hiding (Integer)
import Consts
-- import Functions

import System.Directory (listDirectory)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Example = Example
  { input  :: Grid
  , output :: Grid
  } deriving (Show, Eq, Generic)


data GridOp
  = HConcat Grid Grid
  | Crop IntegerTuple IntegerTuple
  | Replace Integer Integer
  | Rot180
  deriving (Show, Eq)

data IntConsts
  = Six
  | Two
  | Seven
  | Five
  deriving (Show, Eq)

data IntTupConsts
  = TwoByTwo
  | Origin
  deriving (Show, Eq)

convertIntConsts :: IntConsts -> Integer
convertIntConsts Six = six
convertIntConsts Two = two
convertIntConsts Seven = seven
convertIntConsts Five = five

convertIntTupConsts :: IntTupConsts -> IntegerTuple
convertIntTupConsts TwoByTwo = twoByTwo
convertIntTupConsts Origin = origin


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
               -- putStrLn $ "Example " ++ show i ++ ":"
               -- putStrLn "Input:"
               -- mapM_ print (input ex)
               -- putStrLn "Expected Output:"
               -- mapM_ print (output ex)
               -- putStrLn "Solved Output:"
               -- mapM_ print solvedOutput
               putStrLn $ "Correct: " ++ show (solvedOutput == output ex)
               putStrLn ""
            ) (zip [1..] task)
    Nothing -> putStrLn $ "Task " ++ taskName ++ " not found."

-- solve_a416b8f3 :: Grid -> Grid
-- solve_a416b8f3 inputGrid = hconcat inputGrid inputGrid

-- | hconcat concatenates two grids horizontally.
-- It zips the two grids row by row and concatenates each corresponding pair of rows.
hconcat :: Grid -> Grid -> Grid
hconcat a b = zipWith (++) a b

-- | crop extracts a subgrid from the given grid.
-- The subgrid is specified by a starting position (row, column) and dimensions (number of rows, number of columns).
crop :: Grid -> IntegerTuple -> IntegerTuple -> Grid
crop grid (startRow, startCol) (nRows, nCols) =
  map (take nCols . drop startCol) (take nRows (drop startRow grid))

-- | replace performs a color substitution on the grid.
-- It replaces every occurrence of 'replacee' with 'replacer'.
replace :: Grid -> Integer -> Integer -> Grid
replace grid replacee replacer =
  map (map (\v -> if v == replacee then replacer else v)) grid

-- | rot180 rotates the grid by 180 degrees.
-- This is achieved by reversing the order of the rows and reversing each row.
rot180 :: Grid -> Grid
rot180 grid = map reverse (reverse grid)

-- a416b8f3, b1948b0a, c8f0f002, d10ecb37, 3c9b0459
solve_a416b8f3 :: Grid -> Grid
solve_a416b8f3 inputGrid = hconcat inputGrid inputGrid

solve_b1948b0a :: Grid -> Grid
solve_b1948b0a inputGrid = replace inputGrid six two

solve_c8f0f002 :: Grid -> Grid
solve_c8f0f002 inputGrid = replace inputGrid seven five

solve_d10ecb37 :: Grid -> Grid
solve_d10ecb37 inputGrid = crop inputGrid origin twoByTwo

solve_3c9b0459 :: Grid -> Grid
solve_3c9b0459 inputGrid = rot180 inputGrid

-- convertIntTupConsts :: IntTupConsts -> IntegerTuple
-- convertIntConsts :: IntConsts -> Integer
enumerate :: Int -> Grid -> [GridOp]
enumerate 1 grid =
  let -- HConcat using the provided grid twice
      opsHConcat = [HConcat grid grid]

      -- Crop with all combinations of IntTupConsts
      opsCrop = [Crop (convertIntTupConsts t1) (convertIntTupConsts t2) | t1 <- [Origin, TwoByTwo], t2 <- [Origin, TwoByTwo]]

      -- Replace with all pairs of IntConsts
      opsReplace = [Replace (convertIntConsts i) (convertIntConsts j) | i <- [Six, Two, Seven, Five], j <- [Six, Two, Seven, Five]]

      -- Rot180 has no arguments
      opsRot180 = [Rot180]

  in opsHConcat ++ opsCrop ++ opsReplace ++ opsRot180
enumerate _ _ = []

-- | Convert a GridOp candidate into a function on grids.
-- Notice that for operations like HConcat we ignore the grid stored in the candidate
-- and instead use the input grid.
applyGridOp :: GridOp -> Grid -> Grid
applyGridOp (HConcat _ _) grid = hconcat grid grid
applyGridOp (Crop pos dims) grid = crop grid pos dims
applyGridOp (Replace r1 r2) grid = replace grid r1 r2
applyGridOp Rot180 grid = rot180 grid

-- | Try every enumerated operation on the training examples of a task.
-- If one candidate produces the expected output on every example, return it.
solveWithEnumeration :: String -> IO ()
solveWithEnumeration taskName = do
  trainData <- getData True
  case Map.lookup "train" trainData >>= Map.lookup taskName of
    Nothing -> putStrLn $ "Task " ++ taskName ++ " not found."
    Just task -> do
      -- Use the input from the first example to generate candidate operations.
      let firstInput = input (head task)
      let candidates = enumerate 1 firstInput
      -- Filter candidates: for each candidate, check that it gives the expected output on every example.
      let validCandidates = filter (\op ->
            all (\ex -> applyGridOp op (input ex) == output ex) task) candidates
      case validCandidates of
        (op:_) -> do
          putStrLn $ "Found candidate operation for task " ++ taskName ++ ": " ++ show op
          -- Now use this candidate to "solve" the task (it will print each example).
          solve taskName (applyGridOp op)
        [] -> putStrLn "No candidate operation found that fits all training examples."


main :: IO ()
main = do
  -- For demonstration, we try to solve task "a416b8f3" using our enumeration-based approach.
  solveWithEnumeration "a416b8f3"
  -- You could similarly try with other tasks:
  -- solveWithEnumeration "b1948b0a"
  -- solveWithEnumeration "c8f0f002"
  -- solveWithEnumeration "d10ecb37"
  -- solveWithEnumeration "3c9b0459"
