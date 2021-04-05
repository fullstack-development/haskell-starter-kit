{-# LANGUAGE RecordWildCards #-}

module Ext.HTTP.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Maybe (maybeToList)
import qualified Data.Text as T

data WebApiHttpError = WebApiHttpError
  { waheMessage :: T.Text,
    waheCode :: T.Text,
    waheData :: Maybe J.Value
  }
  deriving (Show, Eq)

instance J.ToJSON WebApiHttpError where
  toJSON WebApiHttpError {..} =
    let dataField = ("data" .=) <$> maybeToList waheData
     in J.object $ ("code" .= waheCode) : ("message" .= waheMessage) : dataField

mkWebApiHttpError :: T.Text -> T.Text -> WebApiHttpError
mkWebApiHttpError msg code = WebApiHttpError msg code Nothing
