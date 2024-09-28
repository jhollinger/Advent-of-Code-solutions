module Day1 (part1, part2) where

part1 :: String -> IO ()
part1 input = do
  contents <- readFile input
  let n = sum $ map parseNum (lines contents)
  print n

part2 :: String -> IO ()
part2 input = do
  contents <- readFile input
  let n = sum $ map parseNum' (lines contents)
  print n

parseNum :: [Char] -> Int
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

parseNum' :: [Char] -> Int
parseNum' line =
  let nums = numerics line
      n = head nums
      m = last nums
   in read [n, m]

numerics :: [Char] -> [Char]
numerics [] = []
numerics chars = case chars of
  ('1' : xs) -> '1' : numerics xs
  ('2' : xs) -> '2' : numerics xs
  ('3' : xs) -> '3' : numerics xs
  ('4' : xs) -> '4' : numerics xs
  ('5' : xs) -> '5' : numerics xs
  ('6' : xs) -> '6' : numerics xs
  ('7' : xs) -> '7' : numerics xs
  ('8' : xs) -> '8' : numerics xs
  ('9' : xs) -> '9' : numerics xs
  ('o' : 'n' : 'e' : xs) -> '1' : numerics ('n' : 'e' : xs)
  ('t' : 'w' : 'o' : xs) -> '2' : numerics ('w' : 'o' : xs)
  ('t' : 'h' : 'r' : 'e' : 'e' : xs) -> '3' : numerics ('h' : 'r' : 'e' : 'e' : xs)
  ('f' : 'o' : 'u' : 'r' : xs) -> '4' : numerics ('o' : 'u' : 'r' : xs)
  ('f' : 'i' : 'v' : 'e' : xs) -> '5' : numerics ('i' : 'v' : 'e' : xs)
  ('s' : 'i' : 'x' : xs) -> '6' : numerics ('i' : 'x' : xs)
  ('s' : 'e' : 'v' : 'e' : 'n' : xs) -> '7' : numerics ('e' : 'v' : 'e' : 'n' : xs)
  ('e' : 'i' : 'g' : 'h' : 't' : xs) -> '8' : numerics ('i' : 'g' : 'h' : 't' : xs)
  ('n' : 'i' : 'n' : 'e' : xs) -> '9' : numerics ('i' : 'n' : 'e' : xs)
  (_ : xs) -> numerics xs
