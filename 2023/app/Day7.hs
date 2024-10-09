module Day7 (part) where

import Data.Function (on, (&))
import Data.List (group, maximumBy, partition, sort, sortBy, sortOn)
import Data.Ord
import Helpers

data Hand = HighCard [Card] | OnePair [Card] | TwoPair [Card] | ThreeOfKind [Card] | FullHouse [Card] | FourOfKind [Card] | FiveOfKind [Card] deriving (Ord, Eq, Show)

data Card = Joker | Num Int | Tom | Jack | Queen | King | Ace deriving (Ord, Eq, Show)

type Bid = (Hand, Int)

part :: Int -> String -> IO ()
part 1 contents = do
  let bids = map (parse Jack) (lines contents) & sortOn fst
      total = bids & map snd & zipWith (*) [1 ..] & sum
  print total
part 2 contents = do
  let bids = map (parse Joker) (lines contents) & sortOn fst
      total = bids & map snd & zipWith (*) [1 ..] & sum
  print total
part n _ = putStrLn ("Unknown part " ++ show n)

parse :: Card -> String -> Bid
parse j line =
  let (vals, bid) = splitOn ' ' line
   in (parseHand j vals, read bid)

parseHand :: Card -> [Char] -> Hand
parseHand j vals =
  case kinds j of
    [5] -> FiveOfKind cards
    [4, 1] -> FourOfKind cards
    [3, 2] -> FullHouse cards
    [3, 1, 1] -> ThreeOfKind cards
    [2, 2, 1] -> TwoPair cards
    [2, 1, 1, 1] -> OnePair cards
    _ -> HighCard cards
  where
    kinds Joker = groupKinds $ promoteJokers cards
    kinds _ = groupKinds cards
    groupKinds cards' = cards' & sort & group & map length & sortBy (comparing Data.Ord.Down)
    cards = parseCards j vals

promoteJokers :: [Card] -> [Card]
promoteJokers cards =
  case partition (== Joker) cards of
    (jokers, []) -> jokers
    (jokers, others) -> others ++ map (const best) jokers
      where
        best = others & sort & group & maximumBy (compare `on` length) & head

parseCards :: Card -> [Char] -> [Card]
parseCards _ [] = []
parseCards j ('A' : cs) = Ace : parseCards j cs
parseCards j ('K' : cs) = King : parseCards j cs
parseCards j ('Q' : cs) = Queen : parseCards j cs
parseCards j ('J' : cs) = j : parseCards j cs
parseCards j ('T' : cs) = Tom : parseCards j cs
parseCards j (n : cs) = Num (read [n]) : parseCards j cs
