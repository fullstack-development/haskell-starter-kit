module AppName.Config
  ( Config,
    C.require,
    C.lookup,
    retrieveConfig,
    getKeysFilePath,
    getPort,
    getPoolLimit,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Persist.Postgresql (SqlPersistT)
import Ext.Data.Env (Env (..))
import UnliftIO.Exception (throwString)

type Config = C.Config

retrieveConfig :: Env -> IO C.Config
retrieveConfig env = do
  let configPath =
        case env of
          Dev -> "./config/dev.conf"
          Prod -> "./config/prod.conf"
          Test -> "./config/test.conf"
  C.load [C.Required configPath]

getKeysFilePath :: MonadIO m => C.Config -> m FilePath
getKeysFilePath config = liftIO $ C.require config "auth.key_path"

getPort :: MonadIO m => C.Config -> m Int
getPort config = liftIO $ C.require config "web_server.port"

getPoolLimit :: MonadIO m => C.Config -> m Int
getPoolLimit config = liftIO $ C.require config "database.pool_limit"
