{-# LANGUAGE OverloadedStrings #-}

module Common (Input (..), liftInteract, leftMapParseResult) where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Error (ParseError)
import TextShow

class Input a where
  fromInput :: Text -> Either Text a

instance Input Text where
  fromInput = return

liftInteract :: (Input a, TextShow b) => (a -> b) -> Text -> Text
liftInteract f =
  either ("invalid input" <>) (showt . f) . fromInput

leftMapParseResult :: Either ParseError a -> Either Text a
leftMapParseResult = first (T.pack . show)
