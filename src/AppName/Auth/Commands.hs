module AppName.Auth.Commands where

import qualified AppName.Config as C
import Servant.Auth.Server (writeKey)
import qualified System.Directory as FS

createKey :: FilePath -> IO ()
createKey filePath = do
  writeKey filePath
  putStrLn $ "Auth key file is created at " <> filePath

checkAuthKey :: IO ()
checkAuthKey = do
  config <- C.retrieveConfig
  authKeyPath <- C.getKeysFilePath config
  isExist <- FS.doesFileExist authKeyPath
  if isExist then notifyExists authKeyPath else createKey authKeyPath
  where
    notifyExists filePath = putStrLn $ "Using auth key file at " <> filePath
