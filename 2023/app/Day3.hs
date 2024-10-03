module Day3 (part1, part2) where

type Coord = (Int, Int)

data Symbol = Symbol Char Coord deriving (Show)

data Part = Part Int [Coord] deriving (Show)

type Gear = (Part, Symbol, Part)

part1 :: String -> IO ()
part1 input = do
  contents <- readFile input
  let rows = lines contents
      symbols = parseSymbols rows 0
      numbers = parseNumbers rows 0
      parts = filter (coordsOverlap symbols) numbers
      total = sum $ map (\(Part n _) -> n) parts
  print total

part2 :: String -> IO ()
part2 input = do
  contents <- readFile input
  let rows = lines contents
      symbols = parseSymbols rows 0
      numbers = parseNumbers rows 0
      parts = filter (coordsOverlap symbols) numbers
      stars = filter (\(Symbol c _) -> c == '*') symbols
      gears = foldl (collectGears parts) [] stars
      total = sum $ map (\(Part n _, _, Part m _) -> n * m) gears
  print total
  where
    collectGears :: [Part] -> [Gear] -> Symbol -> [Gear]
    collectGears parts acc symbol =
      case filter (coordsOverlap [symbol]) parts of
        [p1, p2] -> (p1, symbol, p2) : acc
        _ -> acc

coordsOverlap :: [Symbol] -> Part -> Bool
coordsOverlap _ (Part _ []) = False
coordsOverlap symbols (Part n (coord : coords)) =
  any (`elem` symbolCoords) (adjacentCoords coord)
    || coordsOverlap symbols (Part n coords)
  where
    symbolCoords = map (\(Symbol _ c) -> c) symbols
    adjacentCoords (x, y) =
      let y' = y - 1
          y'' = y + 1
       in [(x - 1, y'), (x, y'), (x + 1, y'), (x - 1, y), (x + 1, y), (x - 1, y''), (x, y''), (x + 1, y'')]

parseNumbers :: [String] -> Int -> [Part]
parseNumbers [] _ = []
parseNumbers (row : rows) y = parseRow row 0 ++ parseNumbers rows (y + 1)
  where
    parseRow :: String -> Int -> [Part]
    parseRow [] _ = []
    parseRow chars'@(c : chars) x
      | c `elem` numbers =
          let num = takeNum chars'
              len = length num
              x' = x + len
              coords = map (\x'' -> (x'', y)) [x .. x' - 1]
           in Part (read num) coords : parseRow (drop len chars') x'
      | otherwise = parseRow chars (x + 1)
    takeNum :: String -> String
    takeNum "" = ""
    takeNum (c : chars)
      | c `elem` numbers = c : takeNum chars
      | otherwise = ""
    numbers = ['0' .. '9']

parseSymbols :: [String] -> Int -> [Symbol]
parseSymbols [] _ = []
parseSymbols (row : rows) y = parseRow row 0 ++ parseSymbols rows (y + 1)
  where
    parseRow :: String -> Int -> [Symbol]
    parseRow [] _ = []
    parseRow (c : chars) x
      | c `elem` symbols = Symbol c (x, y) : parseRow chars (x + 1)
      | otherwise = parseRow chars (x + 1)
    symbols = "!@#$%^&*()-_=+/"
