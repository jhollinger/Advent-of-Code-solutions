module Day7 (part) where

import Data.Function ((&))
import Data.List (group, sort, sortBy, sortOn)
import Data.Ord
import Helpers

data Hand = HighCard [Card] | OnePair [Card] | TwoPair [Card] | ThreeOfKind [Card] | FullHouse [Card] | FourOfKind [Card] | FiveOfKind [Card] deriving (Ord, Eq, Show)

data Card = Num Int | Tom | Jack | Queen | King | Ace deriving (Ord, Eq, Show)

type Bid = (Hand, Int)

part :: Int -> String -> IO ()
part 1 contents = do
  let bids = sortOn fst $ map parse (lines contents)
      total = bids & map snd & zipWith (*) [1 ..] & sum
  print total
part 2 contents = do
  putStrLn "Day 7, part 2"
part n _ = putStrLn ("Unknown part " ++ show n)

parse :: String -> Bid
parse line = let (vals, bid) = splitOn ' ' line in (parseHand vals, read bid)

parseHand :: [Char] -> Hand
parseHand vals =
  case kinds of
    [5] -> FiveOfKind cards
    [4, 1] -> FourOfKind cards
    [3, 2] -> FullHouse cards
    [3, 1, 1] -> ThreeOfKind cards
    [2, 2, 1] -> TwoPair cards
    [2, 1, 1, 1] -> OnePair cards
    _ -> HighCard cards
  where
    kinds = cards & sort & group & map length & sortBy (comparing Data.Ord.Down)
    cards = parseCards vals

parseCards :: [Char] -> [Card]
parseCards [] = []
parseCards ('A' : cs) = Ace : parseCards cs
parseCards ('K' : cs) = King : parseCards cs
parseCards ('Q' : cs) = Queen : parseCards cs
parseCards ('J' : cs) = Jack : parseCards cs
parseCards ('T' : cs) = Tom : parseCards cs
parseCards (n : cs) = Num (read [n]) : parseCards cs
