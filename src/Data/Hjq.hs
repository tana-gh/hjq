{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq where

import Control.Error.Util
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text
import Data.ByteString.Lazy
import Data.Hjq.Parser
import Data.Hjq.Query

hjq :: ByteString -> Text -> Either Text ByteString
hjq jsonString queryString = do
    value <- note "Invalid json format." $ decode jsonString
    query <- parseJqQuery queryString
    encodePretty <$> executeQuery query value
