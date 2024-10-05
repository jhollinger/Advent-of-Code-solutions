module Day4 (part) where

import Data.Function ((&))
import Helpers

data Card = Card Int [Int] [Int] deriving (Show)

part :: Int -> String -> IO ()
part 1 contents = do
  let cards = map parseCard (lines contents)
      points = sum $ map calcPoints cards
  print points
  where
    calcPoints (Card _ winning mine) =
      case filter (`elem` winning) mine of
        [] -> 0
        matching -> foldl1 (\acc _ -> acc * 2) $ map (const 1) matching
part 2 contents = do
  let cards = map parseCard (lines contents)
      cards' = copy cards cards
  print $ length cards'
  where
    copy :: [Card] -> [Card] -> [Card]
    copy [] _ = []
    copy (card@(Card n winning mine) : cards) allCards =
      let count = length $ filter (`elem` winning) mine
          copied = after n allCards & take count
       in card : copy (copied ++ cards) allCards
    after n = dropWhile (\(Card m _ _) -> m <= n)
part n _ = error ("Unknown part " ++ show n)

parseCard :: String -> Card
parseCard line =
  let (meta, content) = splitOn ':' line
      num = read $ drop 5 meta
      (winningStr, myStr) = splitOn '|' content
      winning = splitOnAll ' ' winningStr & filter (/= "") & map read
      mine = splitOnAll ' ' myStr & filter (/= "") & map read
   in Card num winning mine
