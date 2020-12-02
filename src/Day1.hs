{-# LANGUAGE OverloadedStrings #-}

module Day1 (day1) where

import Common (Input (..), liftInteract)
import Data.Bifunctor (bimap)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

day1 :: [Text -> Text]
day1 = liftInteract <$> [part1, part2]

part1 :: Day1 -> Maybe Int
part1 (Day1 nums) = listToMaybe [a * b | a <- nums, b <- nums, a + b == 2020]

part2 :: Day1 -> Maybe Int
part2 (Day1 nums) = listToMaybe [a * b * c | a <- nums, b <- nums, c <- nums, a + b + c == 2020]

newtype Day1 = Day1 [Int]

instance Input Day1 where
  fromInput = readDay1

readDay1 :: Text -> Either Text Day1
readDay1 t = Day1 <$> traverse parse (T.lines t)
  where
    parse :: Text -> Either Text Int
    parse = bimap T.pack fst . TR.decimal
