{-# LANGUAGE DataKinds #-}

module AppName.API.PhoneVerification where

import qualified AppName.Domain.PhoneVerification as Model
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Servant (JSON, Post, ReqBody, (:<|>) (..), (:>))

type ErrorCode = T.Text

type ErrorMessage = T.Text

newtype PhoneConfirmationRequest = PhoneConfirmationRequest
  { spcrPhone :: Model.UncheckedPhone
  }
  deriving (Eq, Show)

instance J.FromJSON PhoneConfirmationRequest where
  parseJSON =
    J.withObject "RequestPhoneConfirming" $ \o ->
      PhoneConfirmationRequest . Model.UncheckedPhone <$> o J..: "phone"

data PhoneConfirmationResponse
  = RequestPhoneConfirmingSuccess
  | RequestPhoneConfirmingFail
      ErrorCode
      ErrorMessage
  deriving (Eq, Show)

instance J.ToJSON PhoneConfirmationResponse where
  toJSON RequestPhoneConfirmingSuccess = J.object ["success" J..= True]
  toJSON (RequestPhoneConfirmingFail code msg) =
    J.object
      [ "success" J..= False,
        "error" J..= J.object ["code" J..= code, "message" J..= msg]
      ]

data CodeConfirmationRequest = CodeConfirmationRequest
  { tccrCode :: Model.Code,
    tccrPhone :: Model.UncheckedPhone
  }
  deriving (Eq, Show)

instance J.FromJSON CodeConfirmationRequest where
  parseJSON =
    J.withObject "ConfirmCode" $ \o ->
      CodeConfirmationRequest <$> o J..: "code"
        <*> (Model.UncheckedPhone <$> o J..: "phone")

type AccessToken = BL.ByteString

data CodeConfirmationResponse
  = CodeConfirmationResponseSuccess AccessToken
  | CodeConfirmationResponseFail
      ErrorCode
      ErrorMessage
  deriving (Eq, Show)

instance J.ToJSON CodeConfirmationResponse where
  toJSON (CodeConfirmationResponseSuccess token) =
    J.object
      [ "success" J..= True,
        "data" J..= J.object ["token" J..= TLE.decodeUtf8 token]
      ]
  toJSON (CodeConfirmationResponseFail code msg) =
    J.object
      [ "success" J..= False,
        "error" J..= J.object ["code" J..= code, "message" J..= msg]
      ]

-- API: Definition
type RequestConfirmCodeAPI =
  "request" :> ReqBody '[JSON] PhoneConfirmationRequest :> Post '[JSON] PhoneConfirmationResponse

type TryConfirmCodeAPI =
  "confirm" :> ReqBody '[JSON] CodeConfirmationRequest :> Post '[JSON] CodeConfirmationResponse

type PhoneAuthAPI =
  "auth" :> "phone" :> (RequestConfirmCodeAPI :<|> TryConfirmCodeAPI)
