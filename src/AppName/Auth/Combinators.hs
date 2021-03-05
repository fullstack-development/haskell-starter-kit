module AppName.Auth.Combinators
  ( withClient,
    withAdmin,
  )
where

import AppName.Auth.User (AuthenticatedUser (..))
import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Ext.Data.Text (tshow)
import qualified Ext.Logger.Colog as Log
import Servant (err404)

withClient ::
  (Log.WithLog env Log.Message m, MonadIO m, MonadThrow m) =>
  T.Text ->
  AuthenticatedUser ->
  (Int -> m a) ->
  m a
withClient _ (AuthenticatedClient clientId) handler = handler clientId
withClient handlerName user _ =
  logUnexpectedActor "client" user handlerName >> throw err404

withAdmin ::
  (Log.WithLog env Log.Message m, MonadIO m, MonadThrow m) =>
  T.Text ->
  AuthenticatedUser ->
  (Int -> m a) ->
  m a
withAdmin _ (AuthenticatedAdmin adminId) handler = handler adminId
withAdmin handlerName user _ =
  logUnexpectedActor "client" user handlerName >> throw err404

logUnexpectedActor :: (Log.WithLog env Log.Message m, Show a) => T.Text -> a -> T.Text -> m ()
logUnexpectedActor expected actual source =
  Log.logError $
    "Expected " <> expected <> " role, but got " <> tshow actual
      <> " while invoking handler "
      <> source
