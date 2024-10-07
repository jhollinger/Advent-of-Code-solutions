module Day6 (part) where

import Data.Function ((&))

data Race = Race {time :: Integer, dist :: Integer} deriving (Show)

part :: Int -> String -> IO ()
part 1 contents = do
  let races = parse $ lines contents
      plans = map winningTimes races
      margin = product $ map length plans
  print margin
part 2 contents = do
  let race = parse' $ lines contents
      plans = winningTimes race
      margin = length plans
  print margin
part n _ = putStrLn ("Unknown part " ++ show n)

winningTimes :: Race -> [Integer]
winningTimes r = filter winning [0 .. time r]
  where
    winning hold = (time r - hold) * hold > dist r

parse :: [String] -> [Race]
parse [ts, ds] = zipWith (\t i -> Race t (distances !! i)) times [0 ..]
  where
    times = nums ts
    distances = nums ds
    nums line = line & dropWhile (/= ':') & tail & words & map read
parse _ = []

parse' :: [String] -> Race
parse' [ts, ds] = Race (num ts) (num ds)
  where
    num line = line & dropWhile (/= ':') & tail & filter (/= ' ') & read
parse' _ = Race 0 0
