module Day8 (part) where

import Data.Function ((&))

data Direction = L | R deriving (Show)

type Node = (String, (String, String))

part :: Int -> String -> IO ()
part 1 contents = do
  let (directions, nodes) = parse $ lines contents
      steps = walk directions nodes "AAA" "ZZZ"
  print steps
part 2 contents = do
  putStrLn "Day 8, part 2"
part n _ = putStrLn ("Unknown part " ++ show n)

walk :: [Direction] -> [Node] -> String -> String -> Int
walk dirs = walk' 0 (cycle dirs)

parse :: [String] -> ([Direction], [Node])
parse = parse' ([], [])

walk' :: Int -> [Direction] -> [Node] -> String -> String -> Int
walk' n dirs nodes from to
  | from == to = n
  | otherwise = walk' (n + 1) (tail dirs) nodes (step dir) to
  where
    dir = head dirs
    step L = fst next
    step R = snd next
    next = filter (\(key, _) -> key == from) nodes & head & snd

parse' :: ([Direction], [Node]) -> [String] -> ([Direction], [Node])
parse' acc [] = acc
parse' acc ("" : ls) = parse' acc ls
parse' (dirs, nodes) ((n1 : n2 : n3 : ' ' : '=' : ' ' : '(' : l1 : l2 : l3 : ',' : ' ' : r1 : r2 : r3 : ')' : "") : ls) =
  let node = ([n1, n2, n3], ([l1, l2, l3], [r1, r2, r3]))
   in parse' (dirs, node : nodes) ls
parse' (dirs, nodes) (l : ls) = parse' (dirs ++ parseDirs l, nodes) ls
  where
    parseDirs [] = []
    parseDirs ('L' : cs) = L : parseDirs cs
    parseDirs (_ : cs) = R : parseDirs cs
