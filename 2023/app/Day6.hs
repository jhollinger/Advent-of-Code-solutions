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
  putStrLn "Day 6, part 1"
part n _ = putStrLn ("Unknown part " ++ show n)

winningTimes :: Race -> [Integer]
winningTimes r = filter (\hold -> calcDist r hold > dist r) [0 .. time r]

calcDist :: Race -> Integer -> Integer
calcDist (Race t _) hold = (t - hold) * hold

parse :: [String] -> [Race]
parse [ts, ds] = zipWith (\t i -> Race t (distances !! i)) times [0 ..]
  where
    times = nums ts
    distances = nums ds
    nums line = line & dropWhile (/= ':') & tail & words & map read
parse _ = []
