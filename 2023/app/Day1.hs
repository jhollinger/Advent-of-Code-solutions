module Day1 (part) where

part :: Int -> String -> IO ()
part 1 contents = do
  print $ sum [parseNum line | line <- lines contents]
part 2 contents = do
  print $ sum [parseNum' line | line <- lines contents]
part n _ = putStrLn ("Unknown part " ++ show n)

parseNum :: [Char] -> Int
parseNum line =
  let n = firstNumeric line
      m = firstNumeric (reverse line)
   in read [n, m]

parseNum' :: [Char] -> Int
parseNum' line =
  let nums = numerics line
      n = head nums
      m = last nums
   in read [n, m]

firstNumeric :: [Char] -> Char
firstNumeric [] = '0'
firstNumeric (x : xs)
  | x `elem` nums = x
  | otherwise = firstNumeric xs
  where
    nums = ['0' .. '9']

numerics :: [Char] -> [Char]
numerics chars = case chars of
  [] -> []
  (x@'1' : xs) -> x : numerics xs
  (x@'2' : xs) -> x : numerics xs
  (x@'3' : xs) -> x : numerics xs
  (x@'4' : xs) -> x : numerics xs
  (x@'5' : xs) -> x : numerics xs
  (x@'6' : xs) -> x : numerics xs
  (x@'7' : xs) -> x : numerics xs
  (x@'8' : xs) -> x : numerics xs
  (x@'9' : xs) -> x : numerics xs
  xs@('o' : 'n' : 'e' : _) -> '1' : numerics (tail xs)
  xs@('t' : 'w' : 'o' : _) -> '2' : numerics (tail xs)
  xs@('t' : 'h' : 'r' : 'e' : 'e' : _) -> '3' : numerics (tail xs)
  xs@('f' : 'o' : 'u' : 'r' : _) -> '4' : numerics (tail xs)
  xs@('f' : 'i' : 'v' : 'e' : _) -> '5' : numerics (tail xs)
  xs@('s' : 'i' : 'x' : _) -> '6' : numerics (tail xs)
  xs@('s' : 'e' : 'v' : 'e' : 'n' : _) -> '7' : numerics (tail xs)
  xs@('e' : 'i' : 'g' : 'h' : 't' : _) -> '8' : numerics (tail xs)
  xs@('n' : 'i' : 'n' : 'e' : _) -> '9' : numerics (tail xs)
  (_ : xs) -> numerics xs
