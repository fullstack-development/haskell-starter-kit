module Ext.HTTP.Response where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Maybe (maybeToList)
import Ext.HTTP.Error (WebApiHttpError (..))

data WebApiHttpResponse res
  = FailedWebApiHttpResponse [WebApiHttpError]
  | SuccessfulWebApiHttpResponse res
  deriving (Show, Eq)

instance J.ToJSON res => J.ToJSON (WebApiHttpResponse res) where
  toJSON (FailedWebApiHttpResponse errors) =
    J.object $ ("success" .= False) : errorsField
    where
      errorsField =
        case errors of
          [] -> []
          [e] -> ["error" .= e]
          es -> ["errors" .= es]
  toJSON (SuccessfulWebApiHttpResponse res) =
    J.object ["success" .= True, "result" .= res]

getResult :: WebApiHttpResponse res -> Maybe res
getResult (SuccessfulWebApiHttpResponse res) = Just res
getResult _ = Nothing

isSuccessfulResult :: WebApiHttpResponse res -> Bool
isSuccessfulResult (SuccessfulWebApiHttpResponse _) = True
isSuccessfulResult _ = False

success :: WebApiHttpResponse ()
success = SuccessfulWebApiHttpResponse ()

result :: res -> WebApiHttpResponse res
result = SuccessfulWebApiHttpResponse

failWith :: WebApiHttpError -> WebApiHttpResponse res
failWith = FailedWebApiHttpResponse . (: [])

failWithMany :: [WebApiHttpError] -> WebApiHttpResponse res
failWithMany = FailedWebApiHttpResponse
