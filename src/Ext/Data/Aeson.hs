{-# LANGUAGE RankNTypes #-}

module Ext.Data.Aeson
  ( droppedPrefixOptions,
    droppedPrefixParse,
    droppedPrefixDecode,
    dropFPrefix,
  )
where

import Data.Aeson
  ( GFromJSON,
    GToJSON,
    Options,
    Value (..),
    Zero,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Types (Parser)
import Data.Char (isLower, isUpper, toLower)
import GHC.Generics (Generic, Rep)

dropFPrefix :: String -> String
dropFPrefix [] = []
dropFPrefix [x] = [toLower x]
dropFPrefix name@(x : y : xs)
  | all isLower name = name
  | isLower x && isLower y = dropFPrefix xs
  | isLower x && isUpper y = dropFPrefix $ y : xs
  | isUpper x && isLower y = toLower x : y : xs
  | otherwise = name

droppedPrefixOptions :: Options
droppedPrefixOptions = defaultOptions {fieldLabelModifier = dropFPrefix}

droppedPrefixParse ::
  forall a.
  (Generic a, GFromJSON Zero (Rep a)) =>
  Value ->
  Parser a
droppedPrefixParse = genericParseJSON droppedPrefixOptions

droppedPrefixDecode ::
  forall a.
  (Generic a, GToJSON Zero (Rep a)) =>
  a ->
  Value
droppedPrefixDecode = genericToJSON droppedPrefixOptions
