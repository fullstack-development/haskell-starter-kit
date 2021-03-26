module PhoneVerification where

import AppName.API.PhoneVerification (PhoneAuthAPI)
import AppName.Auth (AuthenticatedUser (AuthenticatedClient), defaultJWTSettings, retrieveKey)
import AppName.Auth.Commands (checkAuthKey, createKey)
import qualified AppName.Config as C
import AppName.Domain.PhoneVerification (Code, Phone, codeToText, defParams, phoneToText)
import AppName.Gateways.Endpoints.PhoneVerification (Externals (..), phoneVerificationAPI)
import qualified AppName.Gateways.StatefulRandomGenerator as StatefulRandomGenerator
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVarIO,
    writeTVar,
  )
import Control.Exception.Safe (MonadThrow, throw, try)
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Ext.Logger.Colog (logByteStringStdout, logTextStdout)
import qualified Ext.Logger.Colog as Log
import qualified Ext.Logger.Config as Log
import Network.HTTP.Types (Header, methodPost)
import Network.Wai
import Network.Wai.Test (SResponse)
import Servant (Handler (..), hoistServer, serve)
import qualified Servant.Auth.Server as SAS
import qualified System.Directory as FS
import System.Environment (setEnv)
import System.Random (newStdGen)
import Test.Hspec.Wai
  ( MatchBody (..),
    ResponseMatcher (..),
    WaiSession,
    liftIO,
    post,
    request,
    shouldRespondWith,
    with,
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec
  ( Spec,
    after,
    before,
    describe,
    it,
    shouldNotBe,
    shouldSatisfy,
    testSpec,
  )

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  -- since we are using STM, which is used by server and by tests,
  -- we risk by data consistency between test-cases, so disable parallelism
  checkAuthKey
  firstCodeVar <- newTVarIO Nothing
  secondCodeVar <- newTVarIO mempty
  authUnitsSpec <-
    testSpec "Auth handlers unit-tests" $
      describe "Auth handlers" $ do
        requestCodeUnit firstCodeVar
        confirmCodeUnit secondCodeVar
  defaultMain $ testGroup "Auth" [authUnitsSpec]

api :: Proxy PhoneAuthAPI
api = Proxy

newtype MockUser
  = MockUser Int
  deriving (Eq, Show)

mkApp :: (Phone -> Code -> IO ()) -> MockUser -> IO Application
mkApp onSendCode (MockUser userId) = do
  config <- C.retrieveConfig
  authKeyPath <- C.getKeysFilePath config
  authKey <- SAS.readKey authKeyPath
  randomGen <- StatefulRandomGenerator.newAtomicGen =<< newStdGen
  let logConf :: Log.LoggerConfig
      logConf =
        Log.LoggerConfig
          { appInstanceName = "AppName",
            logToStdout = True,
            logLevel = Log.Debug
          }
      mockExternals =
        Externals
          { eJwtSettings = defaultJWTSettings authKey,
            eRetrieveUserByPhone = \_ -> pure $ AuthenticatedClient userId,
            eSendCodeToUser = onSendCode,
            eRandomGen = randomGen
          }
  impl <- phoneVerificationAPI defParams mockExternals
  pure $ serve api $ hoistServer api hoistTestServer impl

hoistTestServer :: Log.LoggerT Log.Message IO x -> Handler x
hoistTestServer = Handler . ExceptT . try . Log.usingLoggerT mempty

jsonHeaders :: [Header]
jsonHeaders = [("Content-Type", "application/json")]

-- postJsonReq :: B.ByteString -> BL.ByteString -> WaiSession BL.ByteString SResponse
postJsonReq :: B.ByteString -> BL.ByteString -> WaiSession st SResponse
postJsonReq path = request methodPost path jsonHeaders

requestCodeUnit :: TVar (Maybe Code) -> Spec
requestCodeUnit codeVar = do
  let onSendCode _phone code = atomically $ writeTVar codeVar (Just code)
      authenticatedUser = MockUser 120
      receiveCode = liftIO $ readTVarIO codeVar
      cleanCode = atomically $ writeTVar codeVar Nothing

  with (mkApp onSendCode authenticatedUser) . after (const cleanCode) $
    describe "Request confirmation code" $ do
      it "success on first request" $ do
        postJsonReq "/auth/phone/request" "{\"phone\": \"79993332211\"}"
          `shouldRespondWith` "{\"success\":true}"
        code <- receiveCode
        liftIO $ code `shouldSatisfy` isJust
      it "success on first request, and backpress on further with same phone" $ do
        postJsonReq "/auth/phone/request" "{\"phone\": \"79993332211\"}"
          `shouldRespondWith` "{\"success\":true}"
        postJsonReq "/auth/phone/request" "{\"phone\": \"79993332211\"}"
          `shouldRespondWith` "{\"error\":{\"code\":\"tooManyRequests\",\"message\":\"Try again later.\"},\"success\":false}"
        postJsonReq "/auth/phone/request" "{\"phone\": \"79993332211\"}"
          `shouldRespondWith` "{\"error\":{\"code\":\"tooManyRequests\",\"message\":\"Try again later.\"},\"success\":false}"
      it "success on independent request with different phones" $ do
        postJsonReq "/auth/phone/request" "{\"phone\": \"79993332211\"}"
          `shouldRespondWith` "{\"success\":true}"
        postJsonReq "/auth/phone/request" "{\"phone\": \"79996664455\"}"
          `shouldRespondWith` "{\"success\":true}"
      it "fail if non-digits phone sent" $
        postJsonReq "/auth/phone/request" "{\"phone\": \"79993332aa99\"}"
          `shouldRespondWith` "{\"error\":{\"code\":\"invalidPhone\",\"message\":\"Phone is invalid.\"},\"success\":false}"

confirmCodeUnit :: TVar (M.Map T.Text Code) -> Spec
confirmCodeUnit codeVar = do
  let onSendCode phone code =
        atomically $ modifyTVar codeVar . M.insert (phoneToText phone) $ code
      authenticatedUser = MockUser 120
      receiveCode phone = do
        mbCode <- M.lookup phone <$> liftIO (readTVarIO codeVar)
        code <-
          maybe (error $ "Code not found for " <> T.unpack phone) pure mbCode
        pure . TLE.encodeUtf8 . TL.fromStrict . codeToText $ code
      cleanCodes = atomically $ writeTVar codeVar mempty
  with (mkApp onSendCode authenticatedUser) . after (const cleanCodes) $ do
    let confirm phone code =
          postJsonReq
            "/auth/phone/confirm"
            ( "{\"phone\": \"" <> phone <> "\", \"code\": \"" <> code
                <> "\"}"
            )
        requestCode phone =
          postJsonReq "/auth/phone/request" $ "{\"phone\": \"" <> phone <> "\"}"
    describe "Code confirmation" $ do
      it "confirm by sending of correct code after requesting" $ do
        requestCode "79993332211" `shouldRespondWith` "{\"success\":true}"
        code <- receiveCode "79993332211"
        confirm "79993332211" code
          `shouldRespondWith` ResponseMatcher 200 [] (MatchBody matchSuccessConfirmBody)
      it "confirm two independent phones simultuniously" $ do
        requestCode "79993332288" `shouldRespondWith` "{\"success\":true}"
        firstCode <- receiveCode "79993332288"
        requestCode "79993335544" `shouldRespondWith` "{\"success\":true}"
        secondCode <- receiveCode "79993335544"
        liftIO $ secondCode `shouldNotBe` firstCode
        confirm "79993332288" firstCode
          `shouldRespondWith` ResponseMatcher 200 [] (MatchBody matchSuccessConfirmBody)
        confirm "79993335544" secondCode
          `shouldRespondWith` ResponseMatcher 200 [] (MatchBody matchSuccessConfirmBody)
      it "send incorrect code, if code is wrong" $ do
        requestCode "79993332211" `shouldRespondWith` "{\"success\":true}"
        confirm "79993332211" "0000"
          `shouldRespondWith` "{\"error\":{\"code\":\"incorrectCode\",\"message\":\"Provided code was incorrect.\"},\"success\":false}"
      it "fail, if code was not requested at all" $
        confirm "79993332211" "1123"
          `shouldRespondWith` "{\"error\":{\"code\":\"shouldBeRequestedFirst\",\"message\":\"Need to request code first.\"},\"success\":false}"

matchSuccessConfirmBody :: p -> BL.ByteString -> Maybe String
matchSuccessConfirmBody _ body = do
  decoded <- J.decode body :: Maybe J.Value
  let obj = flip J.parse decoded . withObject "ConfirmBody"
      isAppropriate =
        obj $ \o -> do
          d <- o .: "data"
          _token :: String <- d .: "token"
          success :: Bool <- o .: "success"
          pure success
  case isAppropriate of
    J.Success True -> Nothing
    J.Success False -> Just $ "Not succeed: " <> show body
    J.Error e -> Just $ "Not succeed: " <> e <> ", instead got " <> show body
