module Day3 (day3) where

import Common
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector ((!))
import qualified Data.Vector as V
import Debug.Trace

day3 :: [Text -> Text]
day3 = liftInteract <$> [part1]

-- Include Unknown here because I'm being lazy and its a good sentinel value for other things
-- It will never be produced by the read
data Point = Tree | Open | Unknown
  deriving (Show)

isNotUnknown :: Point -> Bool
isNotUnknown Unknown = False
isNotUnknown _ = True

isTree :: Point -> Bool
isTree Tree = True
isTree _ = False

toPoint c = case c of
  '.' -> Open
  '#' -> Tree
  _ -> Unknown

newtype Slope = Slope (V.Vector (V.Vector Point))
  deriving (Show)

instance Input Slope where
  -- Lazy on the input since we have the Unknown type
  fromInput text = Right $ Slope $ lineToPoints <$> V.fromList lines
    where
      lines = T.lines text

lineToPoints :: Text -> V.Vector Point
lineToPoints line = toPoint <$> V.unfoldr generator (T.length line - 1)
  where
    generator :: Int -> Maybe (Char, Int)
    generator idx = if idx >= 0 then Just (T.index line idx, idx - 1) else Nothing

part1 :: Slope -> Int
part1 input = length $ filter isTree $ takeWhile isNotUnknown path
  where
    path = positionAt input <$> unfoldPath (3, 1) (0, 0)

positionAt :: Slope -> (Int, Int) -> Point
positionAt (Slope input) (_, y) | y >= V.length input = Unknown
positionAt (Slope input) (x, y) = input ! y ! x'
  where
    x' = x `rem` V.length (V.head input)

unfoldPath :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
unfoldPath delta@(dx, dy) pos@(x, y) = pos : unfoldPath delta (x + dx, y + dy)
