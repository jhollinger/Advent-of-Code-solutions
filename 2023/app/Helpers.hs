module Helpers
  ( splitOnAll,
    splitOn,
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
