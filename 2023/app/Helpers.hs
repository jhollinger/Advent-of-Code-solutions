module Helpers
  ( splitOnAll,
    splitOn,
    dedup,
  )
where

import Data.List (elemIndex)

splitOnAll :: Char -> String -> [String]
splitOnAll _ "" = []
splitOnAll char line =
  let (segment, remainder) = splitOn char line
   in segment : splitOnAll char remainder

splitOn :: Char -> String -> (String, String)
splitOn c line =
  case c `elemIndex` line of
    (Just i) ->
      let (segment, remainder) = splitAt i line
       in (segment, tail remainder)
    Nothing -> (line, "")

dedup :: Char -> String -> String
dedup c str = dedup' str False
  where
    dedup' "" _ = ""
    dedup' (x : xs) False
      | c == x = x : dedup' xs True
      | otherwise = x : dedup' xs False
    dedup' (x : xs) True
      | c == x = dedup' xs True
      | otherwise = x : dedup' xs False
