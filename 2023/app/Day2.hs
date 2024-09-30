module Day2 (part1, part2) where

import Helpers

type Red = Int

type Green = Int

type Blue = Int

data Draw = Draw Red Green Blue deriving (Show)

data Game = Game Int [Draw] deriving (Show)

part1 :: String -> IO ()
part1 input = do
  contents <- readFile input
  let games = map parseGame (lines contents)
      games' = filter possible games
      total = sum $ map (\(Game num _) -> num) games'
   in print total
  where
    numRed = 12
    numGreen = 13
    numBlue = 14
    possible (Game _ draws) = all (\(Draw r g b) -> r <= numRed && g <= numGreen && b <= numBlue) draws

part2 :: String -> IO ()
part2 input = do
  contents <- readFile input
  let games = map parseGame (lines contents)
      minimums = map minCubes games
      total = sum $ map (\(r, g, b) -> r * g * b) minimums
   in print total
  where
    minCubes (Game _ draws) = foldl (\(r, g, b) (Draw r' g' b') -> (max r r', max g g', max b b')) (0, 0, 0) draws

parseGame :: String -> Game
parseGame line =
  let (gameMeta, gameData) = splitOn ':' line
      (_, num) = splitOn ' ' gameMeta
      num' = read num :: Int
      draws = map parseDraw (splitOnAll ';' gameData)
   in Game num' draws

parseDraw :: String -> Draw
parseDraw draw =
  let cubeData = splitOnAll ',' draw
   in foldl countColors (Draw 0 0 0) cubeData
  where
    countColors acc (' ' : cubes) = countColors acc cubes
    countColors (Draw r g b) cubes =
      let (num, color) = splitOn ' ' cubes
          num' = read num
       in case color of
            "red" -> Draw (r + num') g b
            "green" -> Draw r (g + num') b
            _ -> Draw r g (b + num')
