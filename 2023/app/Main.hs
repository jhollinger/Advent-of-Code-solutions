module Main where

import qualified Day1
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  dispatch $ parse args

dispatch :: Maybe (Integer, Integer, String) -> IO ()
dispatch (Just (1, 1, input)) = Day1.part1 input
dispatch (Just (1, 2, input)) = Day1.part2 input
dispatch (Just _) = putStrLn "Unknown day or part"
dispatch Nothing = putStrLn "Error: requires day number, part number, and input path"

parse :: [String] -> Maybe (Integer, Integer, String)
parse (day : part : input : _) = Just (read day, read part, input)
parse _ = Nothing
