module Day2 (day2) where

import Common (Input (..), liftInteract)
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

day2 :: [Text -> Text]
day2 = liftInteract <$> [day part1, day part2]

data Policy = Policy !Int !Int !Char

minCt (Policy m _ _) = m

maxCt (Policy _ m _) = m

ch (Policy _ _ c) = c

data Record = Record !Policy !Text

newtype Day2 = Day2 [Record]

instance Input Day2 where
  fromInput = first (T.pack . show) . parse file ""

unwrap :: Day2 -> [Record]
unwrap (Day2 rs) = rs

day :: (Record -> Bool) -> Day2 -> Int
day isValid = length . filter isValid . unwrap

part1 :: Record -> Bool
part1 (Record policy text) =
  let occurrences = T.count (T.singleton $ ch policy) text
   in occurrences >= minCt policy && occurrences <= maxCt policy

part2 :: Record -> Bool
part2 (Record policy text) = xor (ch1 policy) (ch2 policy)
  where
    needle = ch policy
    -- -1 because we are 1 indexed
    ch1 = (needle ==) . T.index text . subtract 1 . minCt
    ch2 = (needle ==) . T.index text . subtract 1 . maxCt

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

num :: Parser Int
num = read <$> many1 digit

dash :: Parser ()
dash = void $ char '-'

policy :: Parser Policy
policy = Policy <$> (num <* dash) <*> ws num <*> letter

colon :: Parser ()
colon = void $ char ':'

ws :: Parser a -> Parser a
ws p = p <* spaces

password :: Parser Text
password = T.pack <$> many1 letter

record :: Parser Record
record = Record <$> policy <* ws colon <*> password <* newline

file :: Parser Day2
file = Day2 <$> many1 record
