module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  dispatch $ parse args

dispatch :: Maybe (Int, Int, String) -> IO ()
dispatch (Just (day, n, input)) = do
  contents <- readFile input
  case day of
    1 -> Day1.part n contents
    2 -> Day2.part n contents
    3 -> Day3.part n contents
    4 -> Day4.part n contents
    5 -> Day5.part n contents
    _ -> putStrLn ("Unknown day" ++ show day)
dispatch Nothing = putStrLn "Error: requires day number, part number, and input path"

parse :: [String] -> Maybe (Int, Int, String)
parse (day : part : input : _) = Just (read day, read part, input)
parse _ = Nothing
