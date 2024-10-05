module Day4 (part1, part2) where

import Data.Function ((&))
import Helpers

data Card = Card Int [Int] [Int] deriving (Show)

part1 :: String -> IO ()
part1 input = do
  contents <- readFile input
  let cards = map parseCard (lines contents)
      points = sum $ map calcPoints cards
  print points
  where
    calcPoints (Card _ winning mine) =
      case filter (`elem` winning) mine of
        [] -> 0
        matching -> foldl1 (\acc _ -> acc * 2) $ map (const 1) matching

part2 :: String -> IO ()
part2 input = do
  contents <- readFile input
  let cards = map parseCard (lines contents)
      cards' = copy cards cards
  print $ length cards'
  where
    copy :: [Card] -> [Card] -> [Card]
    copy [] _ = []
    copy (card@(Card n winning mine) : cards) all =
      let count = length $ filter (`elem` winning) mine
          copied = after n all & take count
       in card : copy (copied ++ cards) all
    after n = dropWhile (\(Card m _ _) -> m <= n)

parseCard :: String -> Card
parseCard line =
  let (meta, content) = splitOn ':' line
      num = read $ drop 5 meta
      (winningStr, myStr) = splitOn '|' content
      winning = splitOnAll ' ' winningStr & filter (/= "") & map read
      mine = splitOnAll ' ' myStr & filter (/= "") & map read
   in Card num winning mine
