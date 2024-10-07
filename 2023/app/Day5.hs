module Day5 (part) where

import Data.Function ((&))
-- import qualified Data.Map as Map
import Helpers

type Seed = Int

data Entity = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location | Foo deriving (Show, Eq)

data Range = Range {destStart :: Int, srcStart :: Int, len :: Int} deriving (Show)

type MapKey = (Entity, Entity)

type MapEntry = (MapKey, [Range])

type Almanac = ([Seed], [MapEntry])

part :: Int -> String -> IO ()
part 1 contents = do
  let (seeds, entries) = parse (lines contents) ([], [])
  putStr $ unlines $ map show entries
part 2 contents = do
  putStrLn "Day 5, part 2"
part n _ = putStrLn ("Unknown part " ++ show n)

parse :: [String] -> Almanac -> Almanac
parse [] almanac = almanac
parse ("" : ls) almanac = parse ls almanac
parse (('s' : 'e' : 'e' : 'd' : 's' : ':' : ' ' : l) : ls) (seeds, mappings) =
  let seeds' = map read $ splitOnAll ' ' l
   in parse ls (seeds ++ seeds', mappings)
parse (l : ls) (seeds, mappings) =
  case l of
    "seed-to-soil map:" -> append (Seed, Soil) $ ranges ls
    "soil-to-fertilizer map:" -> append (Soil, Fertilizer) $ ranges ls
    "fertilizer-to-water map:" -> append (Fertilizer, Water) $ ranges ls
    "water-to-light map:" -> append (Water, Light) $ ranges ls
    "light-to-temperature map:" -> append (Light, Temperature) $ ranges ls
    "temperature-to-humidity map:" -> append (Temperature, Humidity) $ ranges ls
    "humidity-to-location map:" -> append (Humidity, Location) $ ranges ls
    _ -> parse ls (seeds, mappings)
  where
    append key ranges' =
      let ls' = drop (length ranges') ls
          mappings' = (key, ranges') : mappings
       in parse ls' (seeds, mappings')
    ranges [] = []
    ranges ("" : _) = []
    ranges (x : xs) =
      let nums = splitOnAll ' ' x & map read :: [Int]
       in Range {destStart = head nums, srcStart = nums !! 1, len = nums !! 2} : ranges xs

foo :: Entity -> ()
foo Seed = ()
foo Soil = ()
foo Fertilizer = ()
foo Water = ()
foo Light = ()
foo Temperature = ()
foo Humidity = ()
foo Location = ()
