{-# LANGUAGE OverloadedStrings #-}

module Common (Input (..), liftInteract) where

import Data.Text (Text)
import TextShow

class Input a where
  fromInput :: Text -> Either Text a

instance Input Text where
  fromInput = return

liftInteract :: (Input a, TextShow b) => (a -> b) -> Text -> Text
liftInteract f =
  either ("invalid input" <>) (showt . f) . fromInput