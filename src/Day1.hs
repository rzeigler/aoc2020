{-# LANGUAGE OverloadedStrings #-}

module Day1 (day1) where

import Common (Input (..), liftInteract)
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

day1 :: [Text -> Text]
day1 = liftInteract <$> [part1, part2]

part1 :: Day1 -> Maybe Int
part1 (Day1 nums) = uncurry (*) <$> find needle haystack
  where
    haystack = [(a, b) | a <- nums, b <- nums]
    needle (a, b) = a + b == 2020

part2 :: Day1 -> Maybe Int
part2 (Day1 nums) = mult <$> find needle haystack
  where
    haystack = [(a, b, c) | a <- nums, b <- nums, c <- nums]
    needle (a, b, c) = a + b + c == 2020
    mult (a, b, c) = a * b * c

newtype Day1 = Day1 [Int]

instance Input Day1 where
  fromInput = readDay1

readDay1 :: Text -> Either Text Day1
readDay1 t = Day1 <$> traverse parse (T.lines t)
  where
    parse :: Text -> Either Text Int
    parse = bimap T.pack fst . TR.decimal