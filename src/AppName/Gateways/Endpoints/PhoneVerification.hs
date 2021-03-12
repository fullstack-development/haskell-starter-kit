{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module AppName.Gateways.Endpoints.PhoneVerification
  ( Externals (..),
    PhoneAuthAPI,
    phoneVerificationAPItype,
    phoneVerificationAPI,
  )
where

import AppName.API.PhoneVerification
import AppName.AppHandle (MonadHandler)
import AppName.Auth.User (AuthenticatedUser)
import qualified AppName.Domain.PhoneVerification as Model
import qualified AppName.Gateways.PhoneVerificationStorage as S
import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (lift)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Time (getCurrentTime)
import qualified Ext.Logger.Config as Log
import qualified Ext.Logger.Colog as Log
import Servant ((:<|>) (..), ServerT)
import qualified Servant.Auth.Server as SAS
import System.Random (newStdGen)

type QueryUser =
  forall m.
  MonadHandler m =>
  Model.Phone ->
  m AuthenticatedUser

type SendCodeToUser = Model.Phone -> Model.Code -> IO ()

data Externals
  = Externals
      { eLogger :: Log.LoggerConfig,
        eJwtSettings :: SAS.JWTSettings,
        eRetrieveUserByPhone :: QueryUser,
        eSendCodeToUser :: SendCodeToUser
      }

data Handle s
  = Handle
      { hParams :: Model.Parameters,
        hLogger :: Log.LoggerConfig,
        hJwtSettings :: SAS.JWTSettings,
        hRetrieveUserByPhone :: QueryUser,
        hSendCodeToUser :: SendCodeToUser,
        hStorage :: s
      }

phoneVerificationAPItype :: Proxy PhoneAuthAPI
phoneVerificationAPItype = Proxy

phoneVerificationAPI ::
  (MonadHandler minternal) =>
  Model.Parameters ->
  Externals ->
  IO (ServerT PhoneAuthAPI minternal)
phoneVerificationAPI params Externals {..} = do
  storage :: S.MemoryStorage <- S.mkStorage
  let h =
        Handle
          { hParams = params,
            hLogger = eLogger,
            hJwtSettings = eJwtSettings,
            hRetrieveUserByPhone = eRetrieveUserByPhone,
            hSendCodeToUser = eSendCodeToUser,
            hStorage = storage
          }
  pure $ requestCode h :<|> tryConfirmCode h

requestCode ::
  (MonadHandler m, S.Storage s) =>
  Handle s ->
  PhoneConfirmationRequest ->
  m PhoneConfirmationResponse
requestCode Handle {..} PhoneConfirmationRequest {..} =
  run $ do
    phone <- either invalidPhone pure $ Model.checkPhone spcrPhone
    time <- liftIO getCurrentTime
    mbExisting <- S.getFromStorage phone hStorage
    traverse_ (bool tooManyReqs (pure ()) . Model.isConfirmReqExpired time) mbExisting
    code <- Model.genConfirmationCode phone hParams <$> liftIO newStdGen
    let waiting = Model.WaitConfirmationEntry phone code time
    S.setToStorage phone waiting hStorage
    liftIO $ hSendCodeToUser phone code
    pure RequestPhoneConfirmingSuccess
  where
    -- logger = logFormatted hLogger
    tooManyReqs = do
      lift $ Log.logError "Too many requests!"
      throwError $
        RequestPhoneConfirmingFail "tooManyRequests" "Try again later."
    invalidPhone e = do
      -- logError logger $ "Invalid phone in phone code auth: " <> tshow e
      throwError $ RequestPhoneConfirmingFail "invalidPhone" "Phone is invalid."
    run action = runExceptT action >>= either pure pure

tryConfirmCode ::
  forall m s.
  (MonadHandler m, S.Storage s) =>
  Handle s ->
  CodeConfirmationRequest ->
  m CodeConfirmationResponse
tryConfirmCode Handle {..} CodeConfirmationRequest {..} =
  run $ do
    -- logDebug logger $ "New confirm code request for phone - " <> tshow tccrPhone
    phone <- either invalidPhone pure $ Model.checkPhone tccrPhone
    mbWaiting <- S.getFromStorage phone hStorage
    waiting <- maybe confirmRequestNotFound pure mbWaiting
    time <- liftIO getCurrentTime
    let Model.WaitConfirmationEntry _ targetCode _ = waiting
        isExpired = Model.isConfirmReqExpired time waiting
        isCodeCorrect = tccrCode == targetCode
        removeWaiter = S.removeFromStorage phone hStorage
    when (isExpired || isCodeCorrect) removeWaiter
    when isExpired codeConfirmTimeExpired
    unless
      isCodeCorrect
      incorrectCode -- TODO: add attempts count
      -- logDebug logger $
      -- "Retrieving user by phone "
      -- <> tshow tccrPhone
    authenticatedUser <- lift $ hRetrieveUserByPhone phone
    eiToken <- liftIO $ SAS.makeJWT authenticatedUser hJwtSettings Nothing
    -- logInfo logger $ "Token generated for phone " <> tshow tccrPhone
    either internalError (pure . CodeConfirmationResponseSuccess) eiToken
  where
    -- logger = logFormatted hLogger
    internalError e =
      throwError $
        CodeConfirmationResponseFail
          "internalError"
          "Internal error occured."
    --  do
    -- let logMsg = "Error happened during code confirming: " <> tshow e
    -- logError logger logMsg >> throw err500
    incorrectCode = do
      -- logError logger $ "Entered code is incorrect"
      throwError $
        CodeConfirmationResponseFail
          "incorrectCode"
          "Provided code was incorrect."
    codeConfirmTimeExpired = do
      -- logError logger $ "Entered code is expired"
      throwError $
        CodeConfirmationResponseFail "expired" "Code confirmation has expired."
    confirmRequestNotFound = do
      -- logError logger $ "Code should be requested first"
      throwError $
        CodeConfirmationResponseFail
          "shouldBeRequestedFirst"
          "Need to request code first."
    invalidPhone e = do
      -- logError logger $ "Phone is invalid for code confirmation: " <> tshow e
      throwError $
        CodeConfirmationResponseFail "invalidPhone" "Phone is invalid."
    run action = runExceptT action >>= either pure pure
