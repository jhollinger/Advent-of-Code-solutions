module Day1 (part1, part2) where

part1 :: String -> IO ()
part1 input = do
  contents <- readFile input
  let n = sum $ map parseNum (lines contents)
  print n

part2 :: String -> IO ()
part2 _ = putStrLn "Day 1, Part 2"

parseNum :: [Char] -> Int
parseNum [] = 0
parseNum line =
  let n = firstNumeric line
      m = firstNumeric (reverse line)
   in read [n, m]

firstNumeric :: [Char] -> Char
firstNumeric [] = '0'
firstNumeric (x : xs)
  | x `elem` nums = x
  | otherwise = firstNumeric xs
  where
    nums = ['0' .. '9']
