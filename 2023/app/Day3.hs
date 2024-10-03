module Day3 (part1, part2) where

type Coord = (Int, Int)

data Number = Number Int [Coord] deriving (Show)

part1 :: String -> IO ()
part1 input = do
  contents <- readFile input
  let rows = lines contents
      symbolCoords = parseSymbols rows 0
      numberCoords = parseNumbers rows 0
      total = sum $ map (\(Number n _) -> n) $ filter (coordsOverlap symbolCoords) numberCoords
  print numberCoords
  print total

part2 :: String -> IO ()
part2 input = do
  contents <- readFile input
  print "Day 3, part 2"

coordsOverlap :: [Coord] -> Number -> Bool
coordsOverlap _ (Number _ []) = False
coordsOverlap symCoords (Number n (coord : coords)) =
  any (`elem` symCoords) (adjacentCoords coord)
    || coordsOverlap symCoords (Number n coords)
  where
    adjacentCoords (x, y) =
      let y' = y - 1
          y'' = y + 1
       in [(x - 1, y'), (x, y'), (x + 1, y'), (x - 1, y), (x + 1, y), (x - 1, y''), (x, y''), (x + 1, y'')]

parseNumbers :: [String] -> Int -> [Number]
parseNumbers [] _ = []
parseNumbers (row : rows) y = parseRow row 0 ++ parseNumbers rows (y + 1)
  where
    parseRow :: String -> Int -> [Number]
    parseRow [] _ = []
    parseRow chars'@(c : chars) x
      | c `elem` numbers =
          let num = takeNum chars'
              len = length num
              x' = x + len
              coords = map (\x'' -> (x'', y)) [x .. x' - 1]
           in Number (read num) coords : parseRow (drop len chars') x'
      | otherwise = parseRow chars (x + 1)
    takeNum :: String -> String
    takeNum "" = ""
    takeNum (c : chars)
      | c `elem` numbers = c : takeNum chars
      | otherwise = ""
    numbers = ['0' .. '9']

parseSymbols :: [String] -> Int -> [Coord]
parseSymbols [] _ = []
parseSymbols (row : rows) y = parseRow row 0 ++ parseSymbols rows (y + 1)
  where
    parseRow :: String -> Int -> [Coord]
    parseRow [] _ = []
    parseRow (c : chars) x
      | c `elem` symbols = (x, y) : parseRow chars (x + 1)
      | otherwise = parseRow chars (x + 1)
    symbols = "!@#$%^&*()-_=+/"
