{-# LANGUAGE DeriveAnyClass #-}

module Day4 (day4) where

import Common (Input (..), leftMapParseResult, liftInteract)
import Data.Functor (void)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

day4 :: [Text -> Text]
day4 = liftInteract <$> [part1]

data FieldKey = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
  deriving (Eq, Show)

fieldKey :: Parser FieldKey
fieldKey =
  try (Ecl <$ string "ecl")
    <|> try (Iyr <$ string "iyr")
    <|> try (Eyr <$ string "eyr")
    <|> try (Hgt <$ string "hgt")
    <|> try (Hcl <$ string "hcl")
    <|> try (Ecl <$ string "ecl")
    <|> try (Pid <$ string "pid")
    <|> try (Cid <$ string "cid")
    <|> try (Byr <$ string "byr")

colon = void $ char ':'

fieldValue = T.pack <$> many1 (noneOf ['\n', ' '])

field = (,) <$> fieldKey <* colon <*> fieldValue

fields = sepBy field ((space <|> newline) <* notFollowedBy newline)

records = Records <$> sepBy1 fields (newline *> newline)

newtype Records = Records [[(FieldKey, Text)]]

instance Input Records where
  fromInput = leftMapParseResult . parse records ""

part1 :: Records -> Int
part1 (Records rs) = length $ filter isValid rs

-- For ever field key turn
isValid :: [(FieldKey, Text)] -> Bool
isValid = undefined

reqd = [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]
