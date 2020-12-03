{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Options.Applicative

data Opts = Opts Int Int

days = [day1, day2, day3]

main :: IO ()
main = execParser optParser >>= run . lookupPart days
  where
    optParser =
      info
        (Opts <$> day <*> part)
        (fullDesc <> header "aoc2020 -- Advent of Code in 2020")
    day = argument auto (metavar "DAY")
    part = argument auto (metavar "PART")

run :: Maybe (Text -> Text) -> IO ()
run = maybe (TIO.putStrLn "Unable to find requested impl") TIO.interact

lookupPart :: [[Text -> Text]] -> Opts -> Maybe (Text -> Text)
lookupPart haystack (Opts day part) =
  (fst <$> uncons (drop (day - 1) haystack))
    >>= \lst -> fst <$> uncons (drop (part - 1) lst)
