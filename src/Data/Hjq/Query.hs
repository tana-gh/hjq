{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Query where

import Control.Lens hiding (index)
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.Hjq.Parser
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _)
    = join $ noteNotFoundError fieldName $ applyFilter n <$> obj ^? key fieldName
applyFilter (JqIndex index n) array@(Array _)
    = join $ noteOutOfRangeError index $ applyFilter n <$> array ^? nth index
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> " : " <> tshow o

noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing  = Left $ "field name not found : " <> s

noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing  = Left $ "out of range : " <> tshow s

tshow :: Show a => a -> T.Text
tshow = T.pack . show

executeQuery :: JqQuery -> Value -> Either T.Text Value
executeQuery (JqQueryObject o) v
    = fmap (Object . H.fromList) . traverse sequence $ fmap (fmap $ flip executeQuery v) o
executeQuery (JqQueryArray l) v
    = fmap (Array . V.fromList) . sequence $ fmap (`executeQuery` v) l
executeQuery (JqQueryFilter f) v = applyFilter f v
